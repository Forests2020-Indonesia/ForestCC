library(lidR)
library(raster)
library(dplyr)
library(magrittr)
library(sf)
library(ggplot2)
library(rgdal)



sel_metrics = function(z, htree, rn, nr, cls)
{
  vg <- (cls >= 3 & cls <= 5) # vegetation
  
  # arci = all return canopy index
  ar <- (z >= 0) # semua return
  ac =  vg & (z >= htree) # return dari kanopi harus dari vegetasi dengan ketinggian minimal 2.5m (pengalaman field work di RMU)
  arci = sum(ac) / sum(ar)
  
  # FRCI = first return canopy index
  sr <- (nr == 1) # single return when only 1 beam and 1 return )
  fr <- (nr  > 1) & (rn == 1)  # frist return from many return
  
  # frci'a denominator
  fc <- fr & vg & (z >= htree) 
  sc <- sr & vg & (z >= htree)
  
  frci = (sum(sc) + sum(fc)) / (sum(sr) + sum(fr))
  
  # mean intensity from first return only
  #mean_ity <- mean(ity)
  
  # print(paste0("FRCI = ", frci))
  # return all indexes
  return(list(ARCI = arci, FRCI = frci))
}

calc_pos <- function(chunk_name) 
{
  aoidir <- "/DATA/LIDAR GIZ/AOI/"
  vgrdir <- "/FORESTS2020/CODES/ForestCC/ANCILLARY/VGRID/"
  
  split_chunk_name <- strsplit(chunk_name, "/")
  fname_las <- split_chunk_name[[1]][length(split_chunk_name[[1]])]
  split_fname_las <- strsplit(fname_las, "_")
  
  fn_flightline <- paste0(split_fname_las[[1]][1], 
                          "_",
                          split_fname_las[[1]][2], 
                          ".shp")
  fn_vecgrid <- paste0(split_fname_las[[1]][1], 
                       "_",
                       split_fname_las[[1]][2], 
                       "-vgrid.shp")
  
  tilename <<- gsub(".las", "", fname_las)
  
  # READ THIS !!
  # the operator <<- used to create and assign a global variable within a function 
  # see:  https://stackoverflow.com/questions/10904124/global-and-local-variables-in-r
  
  fl_data <- st_read(paste0(aoidir, fn_flightline)) # 
  vg_data <- st_read(paste0(vgrdir, fn_vecgrid))
  sel_tile <- fl_data %>% filter(TxtMemo == tilename) # select which tile
  
  sel_grid <<- vg_data[sel_tile, op=st_within] # select the grid within sel_tile boundary
  sel_grid$gridID <<- 1:nrow(sel_grid) # add ID for selection
  
  grid01 = sel_grid %>% filter(gridID == 1) # select the first grid
  pos <- raster::extent(grid01)
  
  return(pos)
}

# read las
lasfname <-"/DATA/LIDAR GIZ/LAS (DSM)/PINDAHDATA/STAGE3/LINE_2_82.las"
line2.82 <- readLAS(lasfname)
lascheck(line2.82)

# filter duplicates
lasf <- lasfilterduplicates(line2.82)
lascheck(lasf)

# normalization
## dtm creation
dtm <- grid_terrain(lasf, res=0.5, algorithm = kriging(k=10L))
## normalize
ndsm <- lasnormalize(lasf, dtm)

names(ndsm@data)
summary(ndsm@data$ScanAngleRank)

# filter for narrow scan angle
narrowsca <- lasfilter(ndsm, ScanAngleRank >= -15, ScanAngleRank <= 15)
narrowsca <- lasfilter(narrowsca, Z >= 0)

coords <- calc_pos(lasfname)

resolution=30

mtr.nar <- grid_metrics(narrowsca, 
                            sel_metrics(Z, htree=2.5, ReturnNumber, NumberOfReturns, Classification), 
                            res=resolution,
                            start=c(coords[1], coords[3]))
names(mtr.nar) <- c("naARCI", "naFRCI", "naITY")

mtr.ori <- grid_metrics(ndsm, 
                        sel_metrics(Z, htree=2.5, ReturnNumber, NumberOfReturns, Classification), 
                        res=resolution,
                        start=c(coords[1], coords[3]))

mtr.sub <- mtr.nar$naFRCI - mtr.ori$FRCI


# center grid
centergrd <- st_centroid(sel_grid)
spgrd <- as(centergrd, "Spatial")

# stacking FRCI only
mtr.sta <- stack(mtr.nar$naFRCI, mtr.ori$FRCI)
# stacking ARCI only
mtr.arci <- stack(mtr.nar$naARCI, mtr.ori$ARCI)

# stacking ARCI dan FRCI
arci.frci <- stack(mtr.nar$naARCI, mtr.nar$naFRCI)
df.acci <- as.data.frame(arci.frci)

# extraction FRCI stacks
mtr.all <- raster::extract(mtr.sta, spgrd)
mtr.sp <- raster::extract(mtr.sta, spgrd, sp = TRUE) # spatial object is created after extraction
mtr.sp$diff <- mtr.sp$naFRCI - mtr.sp$FRCI
summary(mtr.sp$diff)
mtr.dfa <- as.data.frame(mtr.all)

arci.sp <- raster::extract(mtr.arci, spgrd, sp = TRUE)

names(mtr.dfa) 
head(mtr.dfa)
mtr.ord <- mtr.dfa[order(mtr.dfa$naFRCI),]
mtr.ord$sortID <- 1:nrow(mtr.ord)

# ARCI vs FRCI
mtr.acci <- raster::extract(arci.frci, spgrd, sp = TRUE) 
mtr.acci$diff <- mtr.acci$naARCI - mtr.acci$naFRCI

# plotting
ggplot(mtr.ord, aes(x=sortID)) + geom_line(aes(y = naFRCI, colour = "naFRCI")) + 
  geom_line(aes(y = FRCI, colour = "FRCI")) + 
  geom_point(aes(y=FRCI)) + geom_point(aes(y=naFRCI)) +
  scale_color_manual(values = c("red", "black")) + ylab("CC Index") + xlab("obs.index")

df.acci <- df.acci[order(df.acci$naARCI), ]
df.acci$sortID <- 1:nrow(df.acci)
ggplot(df.acci, aes(x=sortID)) + geom_line(aes(y = naFRCI, colour = "naFRCI")) + 
  geom_line(aes(y = naARCI, colour = "naARCI")) + 
  #geom_point(aes(y=naFRCI)) + geom_point(aes(y=naARCI)) +
  scale_color_manual(values = c("red", "black")) + ylab("CC Index") + xlab("obs.index")


# sel_grid was created in

# savings some objects
writeRaster(mtr.sta, "PROCESSED_DATA/metric_scan_angle_effect.tif")
writeRaster(mtr.sub, "PROCESSED_DATA/metric_scan_angle_effect_diff.tif", 
            overwrite = TRUE)
writeOGR(mtr.sp, dsn = "PROCESSED_DATA/metric_scan_angle_effect.shp",
         layer = "metric_scan_angle_effect", 
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

writeOGR(mtr.acci, dsn = "PROCESSED_DATA/metric_arci_vs_arci_narrow_scangle.shp",
         layer = "metric_arci_vs_arci_narrow_scangle", 
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# check point cloud in a grid
lasclipBufferPnt <- function(las, x, y, res)
{
  require(magrittr)
  
  lasclipRectangle(las, xleft = x - res / 2, ybottom = y - res / 2, 
                   xright = x + res / 2, ytop = y + res / 2) %>% return
  
}

# grid 01
pcl.grid01 <- lasclipBufferPnt(narrowsca, x = 487260.00, y = 9767800.00, res = 90) 
fcl.grid01 <- lasfilter(pcl.grid01, ReturnNumber == 1)
plot(pcl.grid01)
plot(fcl.grid01)

pcl.grid01@data %>% 
  group_by(as.factor(Classification)) %>% 
  summarise(minZ = min(Z), meanZ = mean(Z), maxZ = max(Z), npcl = n())

fcl.grid01@data %>% 
  group_by(as.factor(Classification)) %>% 
  summarise(minZ = min(Z), meanZ = mean(Z), maxZ = max(Z), npcl = n())



mtr.grid01 <- grid_metrics(pcl.grid01, 
                           sel_metrics(Z, htree=2.5, ReturnNumber, NumberOfReturns, Classification), 
                           res=30,
                           start=c(487260.00 - 45, 9767800.00 - 45))

writeRaster(mtr.grid01, "PROCESSED_DATA/mtr_grid01.tif", overwrite = TRUE)

pcl.grid01@data %>% st_as_sf(coords = c("X", "Y"), crs = 32748 ) %>% 
  write_sf(dsn="PROCESSED_DATA/sf_grid01.shp", delete_layer = TRUE)
fcl.grid01@data %>% st_as_sf(coords = c("X", "Y"), crs = 32748 ) %>% 
  write_sf(dsn="PROCESSED_DATA/sf_grid01_fr.shp", delete_layer = TRUE)


# grid 02

mtr.grid02 <- grid_metrics(pcl.grid02, 
             sel_metrics(Z, htree=2.5, ReturnNumber, NumberOfReturns, Classification), 
             res=30,
             start=c(487350.0 - 30, 9767710.0 - 30))

writeRaster(mtr.grid02, "PROCESSED_DATA/mtr_grid02.tif", overwrite = TRUE)



