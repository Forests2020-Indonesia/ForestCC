library(lidR)

ctg <- catalog("/DATA/LIDAR GAL/43208/43028.laz")
shp <- as.spatial(ctg)
shp %<>% st_as_sf(shp) 

# take out file name of ctg and save as shapefile
strsplit(ctg@data$filename, "\\\\") # strsplit; "\\\\" means split based on \\.
fn <- strsplit(ctg@data$filename, "\\\\")[[1]][5]  # execute without assigning to a variable in order to understand the code
write_sf(shp, paste0(ctg@data$filename, ".shp"))

