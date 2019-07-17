# initially developed by Felix Tampinongkol (2019)
# modified by Wim Ikbal Nursal (2019)


library(dplyr)
library(e1071)
library(caret)
library(sf)
library(magrittr)
library(kernlab)


rm(list=ls())

rasterOptions(maxmemory = 1.5e+10)
rasterOptions(chunksize = 1e+9)
memory.size(TRUE)

dataset_frci <- readRDS("PROCESSED_DATA/OLDS/SET_SAMPLES/dataset_frci.RDS")

frci.bands <- dataset_frci %>% dplyr::select(FRCI, b2, b3, b4, b5, b6, b7)

model_svr <- vector("list", 10)
svr_rmse <- vector("list", 10)

for(fold in unique(dataset_frci$fold)){
  
  cat("\n Fold: ", fold)
  
  sel <- which(dataset_frci$fold == fold)
  training_data <- frci.bands[-sel, ] %>% st_drop_geometry()
  validate_data <- frci.bands[sel, ] %>% st_drop_geometry()
   
  model_svr[[fold]] <- svm(FRCI ~ . , training_data)
  pred_FRCI <- model_svr[[fold]] %>% predict(validate_data)
  svr_rmse[[fold]] <- caret::RMSE(pred=pred_FRCI, obs = validate_data$FRCI)
  
}

colwise_bands <- readRDS("PROCESSED_DATA/SEL_RASTER_COMBINED/colwise_bands_wo_NA.tif")
extrapolate_svr <- model_svr[[8]] %>% raster::predict(colwise_bands, datatype="matrix")

saveRDS(model_svr[[1]], "PROCESSED_DATA/SEL_MODELS/svr_20190510_test.RDS")


