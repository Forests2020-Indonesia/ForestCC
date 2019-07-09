# work like charm

library(rgdal)
library(MODIS)
library(sf)
library(magrittr)
library(doParallel)
library(foreach)

lst_evi <- list.files("/DATA/MOD13Q120102018/EVIQA/",
                      pattern = "_EVI", full.names = TRUE)
lst_qa <- list.files("/DATA/MOD13Q120102018/EVIQA/",
                     pattern = "_pixel_reliability", full.names = TRUE)


#s_evi <- lapply(lst_evi, FUN = function(x) brick(x)) %>% stack
#s_qa <- lapply(lst_qa, FUN = function(x) brick(x)) %>% stack

usedcores <- detectCores() - 1
clusters <- makeCluster(usedcores)
registerDoParallel(clusters)
i=1

foreach(i=1:length(lst_evi)) %dopar% {
  library(raster)
  # open datawt evi with index
  evi <- raster(lst_evi[i])
  qal <- raster(lst_qa[i])
  
  m <- qal
  m[(qal < 0 | qal > 1)] <- NA
  
  m_evi <- mask(evi, m)
  fname <- gsub(pattern = ".tif", "_masked.tif", lst_evi[i])
  writeRaster(m_evi, fname, overwrite = TRUE)
  # open dataset qa with the same index on evi
  # masking evi
  
}

stopCluster(clusters)

