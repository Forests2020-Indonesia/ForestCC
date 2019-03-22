library(lidR)
# library(mapview)

rm(list=ls())

ctg <- catalog("PROCESSED_DATA/SEL_PREPROCESS/")

# setting the process paramaters ----

opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg)   <- 0
opt_cores(ctg) <- 6
opt_output_files(ctg) <- paste0("PROCESSED_DATA/SEL_NORM/{ORIGINALFILENAME}")

# define function ----
normalize <- function(chunk)
{
  las <- readLAS(chunk)
  if(is.empty(las)) return(NULL)
  
  lst <- strsplit(chunk@save, "/")
  fdtm <- paste0(gsub(lst[[1]][2], "SEL_DTM", chunk@save), ".tif")
  

  # make dtm ----
  # dtm
  # 5- pts as an average of pts available in 9 grid of 1 m x 1m grid
  dtm <- grid_terrain(las, res = 0.5, algorithm = kriging(k= 10L))
  
  if(is.null(dtm)) return(NULL)
  
  crs(dtm) <- CRS("+init=epsg:32748")
  writeRaster(dtm, filename = fdtm, overwrite=TRUE)
  
  # normalize ----
  print("Normalize...")
  las <- lasnormalize(las, dtm)
  
  # filter based on Z
  print("Filter Z outlier")
  las <- lasfilter(las, Z >= 0, Z <= 100)
}

catalog_apply(ctg, normalize)

# check las after normalization

newctg <- catalog("PROCESSED_DATA/SEL_NORM/")

opt_chunk_buffer(newctg) <- 0
opt_chunk_size(newctg)   <- 0 
opt_cores(newctg) <- 4

lasInCtgCheck <- function(chunk)
{
  las <- readLAS(chunk)
  
  if(is.empty(las))
    return(NULL)
  
  print(paste("-----------", chunk@files, "---------"))
  print(summary(las@data$Z))
  print(lascheck(las))
  
}

catalog_apply(newctg, lasInCtgCheck)

