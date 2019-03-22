# code for mageries preparation

# Vegetation: NDVI, CNDVI, EVI, Normalized difference senescent
# vegetation index (NDSVI)
# Water Index: NDWI, 
# Soil Index: NBR, MSAVI, BI (Bare soil index? )
# NPV: SATVI, Normalized Different Senecent

# Tsseled Cap: Greenness, TCA, Wetness
library(raster)
library(glcm)

rm(list=ls())

rasterOptions(maxmemory = 1e+10)
folders <- "/DATA/LANDSAT SUMSEL/2015 124062 RAW/"
subname <- "LC08_L1TP_124062_20150626_20170407_01_T1_"

bQ <- raster(paste0(folders, subname, "pixel_qa-utm48S.tif"))
b2 <- raster(paste0(folders, subname, "sr_band2-utm48S.tif"))
b3 <- raster(paste0(folders, subname, "sr_band3-utm48S.tif"))
b4 <- raster(paste0(folders, subname, "sr_band4-utm48S.tif")) 
b5 <- raster(paste0(folders, subname, "sr_band5-utm48S.tif"))
b6 <- raster(paste0(folders, subname, "sr_band6-utm48S.tif"))
b7 <- raster(paste0(folders, subname, "sr_band7-utm48S.tif")) 

b2[b2 > 10000 | b2 < 0] <- NA
b2 <- b2 * 0.0001
b3[b3 > 10000 | b3 < 0] <- NA
b3 <- b3 * 0.0001
b4[b4 > 10000 | b4 < 0] <- NA
b4 <- b4 * 0.0001
b5[b5 > 10000 | b5 < 0] <- NA
b5 <- b5 * 0.0001
b6[b6 > 10000 | b6 < 0] <- NA
b6 <- b6 * 0.0001
b7[b7 > 10000 | b7 < 0] <- NA
b7 <- b7 * 0.0001

NDVI <- (b5 - b4) / (b5 + b4) # normalized difference vegetation index
SATVI <- (b6 - b3) / (b6 + b3 + 0.5) * 1.5 - 0.5 * b7 # spil adjusted total vegetation index (for detcting NPV)
NBR <- (b4 - b6) / (b4 + b6)  # normalized burned ratio
NDWI <- (b3 - b5) / (b3 + b5) # normalized difference water index
NDSI <- (b7 - b3) / (b7 + b3) # normalized difference soil index
TCB = 0.30290*b2 + 0.2786*b3 + 0.47330*b4 + 0.5599*b5 +  0.508*b6 + 0.18720*b7
TCG = -0.2941*b2 + -0.243*b3 + -0.5424*b4 + 0.7276*b5 + 0.0713*b6 + -0.1608*b7
TCW = 0.15110*b2 + 0.1973*b3 +  0.3283*b4 + 0.3407*b5 + 0.7117*b6 + 0.45590*b7
TCA = atan(TCG / TCB) 
TCD = sqrt(TCB^2 + TCG^2)


Tb4M <- glcm(b4, statistics = "mean")
Tb5M <- glcm(b5, statistics = "mean")
Tb6M <- glcm(b6, statistics = "mean")
Tb7M <- glcm(b7, statistics = "mean")
Tb4V <- glcm(b4, statistics = "variance")
Tb5V <- glcm(b5, statistics = "variance")
Tb6V <- glcm(b6, statistics = "variance")
Tb7V <- glcm(b7, statistics = "variance")
 
# writeRaster(get("Tb4M"), filename = "PROCESSED_DATA/SEL_RASTER/Tb4M.tif", overwrite=TRUE) 


RasterObj <- c("RasterLayer", "RasterStack", "RasterBrick")
# save only *Raster objects
show_raster_obj <- function(x)
{
  if(class(get(x)) %in% RasterObj)
   return(x)
}

save_raster_obj <- function(x)
{
  writeRaster(get(x), 
              filename = paste0("PROCESSED_DATA/SEL_RASTER/", x, ".tif"),
              overwrite=TRUE)
}

save_all_raster_obj <- function(x)
{
  if(class(get(x))[1] %in% RasterObj) {
    writeRaster(get(x), 
                filename = paste0("PROCESSED_DATA/SEL_RASTER/", x, ".tif"),
                overwrite=TRUE)
  }
}

lst_raster_obj <- unlist(sapply(ls(), FUN = show_raster_obj), use.names = FALSE)
sapply(lst_raster_obj, FUN = save_raster_obj)






