library(bfast)
library(rgdal)
library(raster)
library(sf)
library(MODISTools)
library(MODIS)
library(strucchange)
library(zoo)
library(magrittr)
library(velox)

rasterOptions(maxmemory = 1.0e+10)
# rasterOptions(chunksize = 1e+9)

# functions
timeser <- function(index, dt) {
  z <- zoo(index, dt)
  yr <- as.numeric(format(time(z), "%Y"))
  jul <- as.numeric(format(time(z), "%j"))
  delta <- min(unlist(tapply(jul, yr, diff))) # 16
  zz <- aggregate(z, yr + (jul - 1) / delta / 23)
  (tso <- as.ts(zz))
  return(tso)
}

# stacking layers

evifiles <- list.files("/DATA/MOD13Q120102018/EVIMASKEDVALUECONVERTED/", full.names = TRUE)
EVIs <- stack(lapply(evifiles, FUN = function(x) raster(x)))
# vxEVI <- velox(EVIs)

# load pts
aoipts <- read_sf("/DATA/LIDAR GIZ/AOIGRID_MODIS_SINUS/all_line_sinus_modis_vgrid_pts.shp")
aoipts <- as(aoipts, "Spatial")

class(aoipts)

xtrpts <- raster::extract(EVIs, aoipts, sp = TRUE)
odsn <- "/FORESTS2020/CODES/ForestCC/PROCESSED_DATA/MODISBFAST/xtrpts.shp"
writeOGR(xtrpts, 
         dsn = "/FORESTS2020/CODES/ForestCC/PROCESSED_DATA/MODISBFAST/xtrpts.shp",
         layer = "xtrpts.shp",
         driver = "ESRI Shapefile")
