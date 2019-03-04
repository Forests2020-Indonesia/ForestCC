library(lidR)
# library(mapview)

rm(list=ls())

las <- 
ctg <- catalog("PROCESSED_DATA/CTG_SINGLE/")

# setting the process paramaters ----

opt_chunk_buffer(ctg) <- 50
opt_chunk_size(ctg)   <- 0
opt_cores(ctg) <- 1
opt_filter(ctg) <- "-drop_z_below 0"
opt_output_files(ctg) <- paste0("PROCESSED_DATA/CTG_OUT/{ORIGINALFILENAME}")

d1 <- Sys.time()
print(d1)
dtm <- grid_terrain(ctg, res=0.25, algorithm = kriging())
d2 <- Sys.time()
d2-d1 # Time difference of 56.23551 secs

opt_output_files(ctg) <- paste0("PROCESSED_DATA/SEL_NORM/{ORIGINALFILENAME}")
ctgnorm <- lasnormalize(ctg, dtm)


newctg = catalog_apply(ctg, normalize)
fulctg <- catalog(unlist(newctg))
fullas <- readLAS(fulctg)

writeLAS(fullas, "PROCESSED_DATA/NORMALIZED_FULL/rmu_normalized.laz")
rm(fullas)
