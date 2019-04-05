rm(list=ls())
library(raster)
library(sf)
library(rgdal)
library (magrittr)
library(velox)

dirmetric <- "PROCESSED_DATA/SEL_METRICS2/"
dirthemes <- "/DATA/TOPO SUMSEL/"
dirimages <- "PROCESSED_DATA/SEL_RASTER/"

# preparing all shape metrics (derived from sel_metrics)
shpmetrics <- list.files(dirmetric, pattern = glob2rx("*.shp"), full.names = TRUE)
lst_metrics <- lapply(shpmetrics, 
                   FUN = function(x) { 
                     sf_obj = read_sf(x)
                     fname <- strsplit(x, "/")
                     print(fname[[1]][length(fname[[1]])])
                     fname <- gsub("-metrics.shp", "", fname[[1]][length(fname[[1]])])
                     sf_obj$tileID <- fname
                     return(sf_obj) }
                   )


sel_metrics <- do.call(rbind, lst_metrics)
names(sel_metrics)[4] <- "LC_Lidar" #for the first round, the concerned attribute names "Tuplah"
sel_metrics

# embedding data from klhk
lc_klhk <- read_sf(paste0(dirthemes, "pl_2015-2016_sumsel_utm48S.shp"))
names(lc_klhk)
head(lc_klhk)
sel_metrics <- st_join(sel_metrics, dplyr::select(lc_klhk, PL__2015, PL__2016))

# embedding raster data
tif_fpath <- list.files(dirimages, pattern = glob2rx("*.tif"), full.names = TRUE)

open_tifs <- function(fname)
{
  obj_brick <- raster(fname)
  chr_tif = paste0(substr(fname, nchar(dirimages) + 1, nchar(fname)-4))
  assign(x = chr_tif, 
         value = obj_brick,
         envir = .GlobalEnv)
  return(chr_tif)
}

env_tif <- sapply(tif_fpath, FUN = open_tifs, USE.NAMES = FALSE) 
ls_tif <- stack(sapply(env_tif, FUN = function(x) get(x)))
stack_tif <- stack(ls_tif)
# stack_vlx <- velox(stack_tif) # ca't work with multipoints sf

# extract by sp
sp_sel_metrics <- as(sel_metrics, "Spatial")
sel_extract <- raster::extract(stack_tif, sp_sel_metrics, df=TRUE)
#binding (sp apprach)
sel_metrics <- dplyr::bind_cols(sel_metrics, as.data.frame(sel_extract))
st_write(sel_metrics, "PROCESSED_DATA/FINAL_SEL_METRICS/sel_metrics2.shp")

