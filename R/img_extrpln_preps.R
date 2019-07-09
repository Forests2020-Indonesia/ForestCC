library(raster)

rm(list=ls())

rasterOptions(maxmemory = 1.5e+10)
rasterOptions(chunksize = 1e+9)

sel_raster_path = "PROCESSED_DATA/SEL_RASTER"

sel_raster <- list.files(sel_raster_path, pattern = glob2rx("*.tif"), full.names = TRUE)
sel_raster0 <- list.files(sel_raster_path, pattern = glob2rx("*.tif"), full.names = FALSE)
sel_raster0 <- gsub(".tif", "", sel_raster0)
sel_raster <- sel_raster[1:6]
sel_raster0 <- sel_raster0[1:6]

list_brick <- lapply(sel_raster, FUN = function(x) brick(x))
list_vector <- lapply(list_brick, function(x) x[])
colwise_bands <- do.call(cbind, list_vector)

saveRDS(colwise_bands, "PROCESSED_DATA/SEL_RASTER_COMBINED/colwise_bands_with_NA.tif")
# svr can't handle NA, need to adjust dataset
colwise_bands[is.na(colwise_bands)] <- -9999
saveRDS(colwise_bands, "PROCESSED_DATA/SEL_RASTER_COMBINED/colwise_bands_wo_NA.tif")

