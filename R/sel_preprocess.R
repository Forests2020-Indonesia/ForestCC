# a copy from RMULiDAR project's code

rm(list=ls())
ctg <- catalog("/DATA/LIDAR GIZ/SELECTEDLAS/")

# setting the process paramaters ----

opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg)   <- 0 
opt_cores(ctg) <- 6
opt_output_files(ctg) <- "PROCESSED_DATA/SEL_PREPROCESS/{ORIGINALFILENAME}"

# define function ----
preprocess <- function(chunk)
{
  las <- readLAS(chunk)
  
  if(is.empty(las)) return(NULL)
  
  # lasfilterduplicates
  las <- lasfilterduplicates(las)
  
  # drop Z less than zero
  las <- lasfilter(las, Z >= 0)
}

newctg = catalog_apply(ctg, preprocess)





