library(lidR)
# library(mapview)

rm(list=ls())

ctg <- catalog("PROCESSED_DATA/PREPROCESSED_FULL/")

# setting the process paramaters ----

opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg)   <- 500
opt_chunk_alignment(ctg) <- c(738965L, 9676285L) # WAJIB INTEGER !
opt_cores(ctg) <- 2
opt_output_files(ctg) <- paste0("PROCESSED_DATA/NORMALIZED_TILES/area_{ID}_normalized")

# define function ----
normalize <- function(chunk)
{
  las <- readLAS(chunk)
  print(las)
  if(is.empty(las) || nrow(las@data) < 100000) return(NULL)
  
  # lasnormalized
  dtm <- grid_terrain(las, res=0.5, algorithm=kriging(k=10L))
  
  las <- lasnormalize(las, dtm)
  
  return(las)
}

newctg = catalog_apply(ctg, normalize)
fulctg <- catalog(unlist(newctg))
fullas <- readLAS(fulctg)

writeLAS(fullas, "PROCESSED_DATA/NORMALIZED_FULL/rmu_normalized.laz")
rm(fullas)