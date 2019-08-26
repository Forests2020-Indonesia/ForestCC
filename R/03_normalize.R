library(lidR)
library(sf)
library(raster)
library(dplyr)
library(magrittr)

# define function ----
normalize <- function(chunk, line_name)
{
  splitChunkName <- strsplit(chunk@files, "/")
  shortChunkName <- splitChunkName[[1]][length(splitChunkName[[1]])]
  outputFileName <- paste0(dir4Nor, "/", shortChunkName)
  
  if(file.exists(outputFileName)) {
    paste0(shortChunkName, " exist!")
    return(NULL)
  }
  
  las <- readLAS(chunk)
  
  if(is.empty(las)) {
    print(paste0(chunk@files,":  NULL"))
    return(NULL)
  }
  
  # DTM
  print(paste(shortChunkName, ": Creating DTM..."))
  DTMFileName <- paste0(dir4DTM, "/", gsub(".las", ".tif", shortChunkName))
  dtm <- grid_terrain(las, res = 0.5, algorithm = kriging(k= 30L))
  if(is.null(dtm)) return(NULL)
  
  crs(dtm) <- CRS("+init=epsg:32748")
  writeRaster(dtm, filename = DTMFileName, overwrite=TRUE)
  
  # normalize ----
  print(paste(shortChunkName, ":  Normalize..."))
  las <- lasnormalize(las, dtm, na.rm = TRUE)
  
  # CHM
  rm(dtm)
  print(paste(shortChunkName, ":  Creating CHM..."))
  CHMFileName <- paste0(dir4CHM, "/", gsub(".las", ".tif", shortChunkName))
  chm <- grid_canopy(las, res = 0.5, pitfree(c(0,2,5,10,15,20), c(0, 1.5)))
  crs(chm) <- CRS("+init=epsg:32748")
  writeRaster(chm, filename = CHMFileName, overwrite=TRUE)
  rm(chm)
  
  # filter based on Z
  print(paste(shortChunkName, ":  Filter Z outlier..."))
  las <- lasfilter(las, Z >= 0, Z <= 100)
  
  return(las)
}

#baseOutFolder <- "/FORESTS2020/CODES/ForestCC/PROCESSED_DATA/TMP/"
baseOutFolder <- "/FORESTS2020/CODES/ForestCC/PROCESSED_DATA/V03/NORMALIZED/"
baseNorFolder <- paste0(baseOutFolder, "LASNORM/")
baseCHMFolder <- paste0(baseOutFolder, "CHM/")
baseDTMFolder <- paste0(baseOutFolder, "DTM/")

baseInpFolder <- "/FORESTS2020/CODES/ForestCC/PROCESSED_DATA/V03/PREPROCESSED/" # folder in which the line folders exist
listInpFolder <- list.dirs(baseInpFolder)

for(i in 2:length(listInpFolder)) {  # the subfolder starts from index no.2
  
  # setting the process paramaters
  ctg <- catalog(listInpFolder[i])
  opt_chunk_buffer(ctg) <- 0
  opt_chunk_size(ctg)   <- 0 
  opt_cores(ctg) <- 5
  
  splitName <- strsplit(listInpFolder[i], "/")
  shortLineName <- splitName[[1]][length(splitName[[1]])]
  
  # create output folder
  dir4Nor = paste0(baseNorFolder, shortLineName)
  dir4CHM = paste0(baseCHMFolder, shortLineName)
  dir4DTM = paste0(baseDTMFolder, shortLineName)
  
  if(!dir.exists(dir4Nor)) dir.create(dir4Nor)
  if(!dir.exists(dir4CHM)) dir.create(dir4CHM)
  if(!dir.exists(dir4DTM)) dir.create(dir4DTM)
  
  opt_output_files(ctg) <- paste0(dir4Nor, "/{ORIGINALFILENAME}")
  
  catalog_apply(ctg, FUN = function(x) normalize(x, shortLineName))
}
   







