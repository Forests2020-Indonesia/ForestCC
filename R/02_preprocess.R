# see 01_lascheck.R for more information about input-output management

library(lidR)
library(raster)
library(dplyr)
library(magrittr)

lasfilternoise = function(las, sensitivity)
{
  # this function is taken from http://bit.ly/2ytWcPa
  p95 <- grid_metrics(las, ~quantile(Z, probs = 0.95), 10)
  las <- lasmergespatial(las, p95, "p95")
  las <- lasfilter(las, Z < p95*sensitivity)
  las$p95 <- NULL
  return(las)
}

preprocess <- function(chunk)
{
  splitChunkName <- strsplit(chunk@files, "/")
  shortChunkName <- splitChunkName[[1]][length(splitChunkName[[1]])]
  outputFileName <- paste0(dir4Las, "/", shortChunkName)
  
  if(file.exists(outputFileName)) {
    paste0(shortChunkName, " exist!")
    return(NULL)
  }
  
  # read chunk ----
  las <- readLAS(chunk)
  
  if(is.empty(las)) 
  {
    print(paste0(chunk@files,":  NULL"))
    return(NULL)
  }
  
  print(paste0(chunk@files,":  OK"))
  # filter duplicates X, Y, Z  ----
  las <- lasfilterduplicates(las)
  
  # filter duplicates X and Y ----
  # it will retain only the first return
  # to know behind this code line, check out lidR's R source at lascheck function 
  # this issue: https://github.com/Jean-Romain/lidR/issues/39
  las <- lasfilter(las, !duplicated(las@data, by=c("X", "Y")))
  
  # remove unclassified points and low points ----
  las <- lasfilter(las, !(Classification == 0 | Classification == 7))
  
  # scan angle -15 <= x <= 15
  las <- lasfilter(las, ScanAngleRank >= -15, ScanAngleRank <= 15)
  
  # when return number > number of returns, it'd be filtered out
  las <- lasfilter(las, ReturnNumber <= NumberOfReturns)

  # filtering noise
  las <- lasfilternoise(las, sensitivity = 1.2)
}

#baseOutFolder <- "/FORESTS2020/CODES/ForestCC/PROCESSED_DATA/TMP/" # test, uncomment to use
baseOutFolder <- "/FORESTS2020/CODES/ForestCC/PROCESSED_DATA/V03/PREPROCESSED/"
#baseInpFolder <- "/DATA/LIDAR GIZ/LASOUTLIERS/" # test, uncomment to use
baseInpFolder <- "/DATA/LIDAR GIZ/LAS/" # folder in which the line folders exist
listInpFolder <- list.dirs(baseInpFolder)

for(i in 2:length(listInpFolder)) {  # the subfolder starts from index no.2
  
  # setting the process paramaters
  ctg <- catalog(listInpFolder[i])
  opt_chunk_buffer(ctg) <- 0
  opt_chunk_size(ctg)   <- 0 
  opt_cores(ctg) <- 6
  
  splitName <- strsplit(listInpFolder[i], "/")
  shortLineName <- splitName[[1]][length(splitName[[1]])]
  
  dir4Las = paste0(baseOutFolder, shortLineName)
  
  if(!dir.exists(dir4Las)) dir.create(dir4Las)
      
  opt_output_files(ctg) <- paste0(dir4Las, "/{ORIGINALFILENAME}")
  catalog_apply(ctg, preprocess)

}




