library(lidR)

# preprocessing with real dataset
# see the preprocess_simulation.R script to understand how the code works with smaller dataset

library(magrittr)

rm(list=ls())

ctg43208 <- catalog("/DATA/LIDAR GAL/43208/") 

# setting the process paramaters ----

opt_cores(ctg43208) <- 4L
opt_chunk_buffer(ctg43208) <- 0
opt_chunk_size(ctg43208)   <- 1000
opt_chunk_alignment(ctg43208) <- c(397000L, 9772000L) # WAJIB INTEGER !
opt_output_files(ctg43208) <- "/DATA/LIDAR GAL/L_43208/l_43208_{XLEFT}_{YBOTTOM}"
opt_filter(ctg43208) <- "-drop_z_above 90"  # based on Google Earth DTM

newctg <- catalog_retile(ctg43208)


