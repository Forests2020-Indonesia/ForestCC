library(lidR)
# library(mapview)

rm(list=ls())

ctg <- catalog("PROCESSED_DATA/CTG_SINGLE/")

# setting the process paramaters ----

opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg)   <- 0
opt_cores(ctg) <- 1

opt_output_files(ctg) <- paste0("PROCESSED_DATA/SEL_NORM/{ORIGINALFILENAME}")

# define function ----
mkDTM <- function(chunk)
{
  las <- readLAS(chunk)
  if(is.empty(las)) return(NULL)
  
  lst <- strsplit(chunk@save, "/")
  fdtm <- paste0(gsub(lst[[1]][2], "SEL_DTM", chunk@save), ".tif")

  # dtm
  dtm <- grid_terrain(las, res=0.25, algorithm=kriging(k=50L))
  crs(dtm) <- CRS("+init=epsg:32748")
  writeRaster(dtm, filename=fdtm, overwrite=TRUE)
  
  # lasnormalize
  las <- lasnormalize(las, dtm)
  return(las)
  
}

newctg = catalog_apply(ctg, mkDTM)

opt_output_files(ctg) <- paste0("PROCESSED_DATA/SEL_DTM/{ORIGINALFILENAME}")



fulctg <- catalog(unlist(newctg))
fullas <- readLAS(fulctg)

writeLAS(fullas, "PROCESSED_DATA/NORMALIZED_FULL/rmu_normalized.laz")
rm(fullas)