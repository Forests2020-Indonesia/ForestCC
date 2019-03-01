# a copy from RMULiDAR project's code

library(magrittr)
library(sf)

rm(list=ls())
ctg <- catalog("/DATA/LIDAR GIZ/SELECTEDLAS/")

# setting the process paramaters ----

opt_chunk_buffer(rmu.cat) <- 10
opt_chunk_size(rmu.cat)   <- 500 
opt_chunk_alignment(rmu.cat) <- c(738965L, 9676285L) # WAJIB INTEGER !
opt_output_files(rmu.cat) <- paste0("PROCESSED_DATA/PREPROCESSED_TILES/tile_500x500_{ID}")

# define function ----
preprocess <- function(chunk)
{
  las <- readLAS(chunk)
  if(is.empty(las)) return(NULL)
  
  # lasfilterduplicates
  las <- lasfilterduplicates(las)
  
  # rn > nr
  las <- lasfilter(las, ReturnNumber <= NumberOfReturns)
  
  # reduce pcl density into 10 pts/m2
  set.seed(123456789)
  las <- lasfilterdecimate(las, algorithm = homogenize(density=10, res=1))
}

newctg = catalog_apply(rmu.cat, preprocess)

# this is how to make separated las object (files) into one object
newctg <- catalog(unlist(newctg))

# then, convert into single las dataset
rmu.las <- readLAS(newctg)

# check wether processing is good
lascheck(rmu.las)

# write it and done
writeLAS(rmu.las, "PROCESSED_DATA/PREPROCESSED_FULL/rmu_las_preprocessed.laz")



