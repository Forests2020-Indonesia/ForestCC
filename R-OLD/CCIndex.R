library(lidR)
library(dplyr)
library(magrittr)
library(sf)

# load the data ----
las <- readLAS("EXAMPLE_DATA/lassample.laz")

# normalize ----
dtm <- grid_terrain(las, res=0.5, method="kriging")
lasnormalize(las, dtm)


# chm ----
chm <- grid_canopy(las, res = 0.5, subcircle = 0.2, na.fill="knnidw", k=10, p=2)
plot(chm)

# LiDAR metric ----
# all return
CCIdx <- function(z, cls)
{
  # all point clouds (returns) which classified as vegetation
  vg <- (cls >= 3) & (cls <= 5) & (z >= 2.5)
  vg <- sum(vg)
  
  # all return despite of their classification; 
  # use 'length' function because of z is a vector object
  ar <- length(z)
  
 return(list(arci = vg/ar)) # the output must be a "list" object, so it uses 'list' function
  
}
  
cc <- grid_metrics(las, func=CCIdx(Z, Classification), res=30)  
cc

# turn CC into sf 
cc_sf = cc %>% as.data.frame %>% st_as_sf(coords = c("X","Y"), crs=32748)
st_write(cc_sf, "PROCESSED_DATA/cc_sf.shp", delete_layer = TRUE)  
