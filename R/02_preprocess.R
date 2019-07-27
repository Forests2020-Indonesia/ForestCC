# see 01_lascheck.R for more information about input-output management
library(lidR)
library(raster)
library(dplyr)
library(magrittr)

ctg <- catalog("/DATA/LIDAR GIZ/LAS (DSM)/PINDAHDATA/STAGE3/")

# setting the process paramaters ----
opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg)   <- 0 
opt_cores(ctg) <- 6
opt_output_files(ctg) <- "PROCESSED_DATA/SEL_PREPROCESS3/{ORIGINALFILENAME}"

sink("PROCESSED_DATA/LOGS/01_sel_preprocess3.txt")

preprocess <- function(chunk)
{
  # read chunk ----
  las <- readLAS(chunk)
  if(is.empty(las)) 
  {
    print(chunk@files)
    return(NULL)
  }
  # filter duplicates X, Y, Z  ----
  las <- lasfilterduplicates(las)
  
  # filter duplicates X and Y ----
  # it will retain only the first return
  # to know behind this code line, check out lidR's R source at lascheck function 
  # this issue: https://github.com/Jean-Romain/lidR/issues/39
  las <- lasfilter(las, !duplicated(las@data, by=c("X", "Y")))
  
  # remove unclassified points and low points ----
  las <- lasfilter(las, !(Classification == 0 | Classification == 7))

}

catalog_apply(ctg, preprocess)
sink("PROCESSED_DATA/LOGS/01_sel_preprocess3.txt", append=TRUE)

newctg <- catalog("PROCESSED_DATA/SEL_PREPROCESS3/")

opt_chunk_buffer(newctg) <- 0
opt_chunk_size(newctg)   <- 0 
opt_cores(newctg) <- 1

sink("PROCESSED_DATA/LOGS/02_lascheck.txt")

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
sink("PROCESSED_DATA/LOGS/02_lascheck.txt", append = TRUE)
# maybe better to check after all processes done





