keras8 <- load_model_hdf5("PROCESSED_DATA/SEL_MODELS/model_fold_8_20190509.h5")
colwise_bands <- readRDS("PROCESSED_DATA/SEL_RASTER_COMBINED/colwise_bands_with_NA.tif")

extrapolate_FRCI <- keras8 %>% predict(colwise_bands)
