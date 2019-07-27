# converting value of modis
library(rgdal)

evifiles <- list.files("/DATA/MOD13Q120102018/EVIMASKED/", full.names = TRUE)

sapply(evifiles, FUN = function(x) { 
  r <- raster(x)
  r <- calc(r, fun = function(x) x * 0.0001)
  fname <- gsub(pattern = "EVIMASKED", replacement = "EVIMASKEDVALUECONVERTED", x)
  print(fname)
  writeRaster(r, filename = fname, overwrite = TRUE)
}
) 

