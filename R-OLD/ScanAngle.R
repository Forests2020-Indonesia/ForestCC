# (1) assessing narrow scan angle filtering effect to Canopy Cover index
# (2) comparing with raster based canopy cover (CHM)


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
saveRDS(lasf, "PROCESSED_DATA/LINE_2_82/lasf.rds")
# normalization
## dtm creation
dtm <- grid_terrain(lasf, res=0.5, algorithm = kriging(k=10L))
saveRDS(dtm, "PROCESSED_DATA/LINE_2_82/dtm.rds")
## normalize
ndsm <- lasnormalize(lasf, dtm)
saveRDS(ndsm, "PROCESSED_DATA/LINE_2_82/nDSM.rds")

# filter for narrow scan angle
narrowsca <- lasfilter(ndsm, ScanAngleRank >= -15, ScanAngleRank <= 15)
narrowsca <- lasfilter(narrowsca, Z >= 0)
saveRDS(narrowsca, "PROCESSED_DATA/LINE_2_82/narrowscan.rds")

# calculate pcl-based CC index ----
coords <- calc_pos(lasfname)
resolution=30
mtr.nar <- grid_metrics(narrowsca, 
                            sel_metrics(Z, htree=2.5, ReturnNumber, NumberOfReturns, Classification), 
                            res=resolution,
                            start=c(coords[1], coords[3]))
names(mtr.nar) <- c("naARCI", "naFRCI")
saveRDS(mtr.nar, "PROCESSED_DATA/LINE_2_82/CCindex_narrow_scanangle.rds")

mtr.ori <- grid_metrics(ndsm, 
                        sel_metrics(Z, htree=2.5, ReturnNumber, NumberOfReturns, Classification), 
                        res=resolution,
                        start=c(coords[1], coords[3]))
saveRDS(mtr.ori, "PROCESSED_DATA/LINE_2_82/CCindex_ori_scanangle.rds")

# subtraction of narrow-scan-filtered FRCI and unfiltered FRCI 
mtr.sub <- mtr.nar$naFRCI - mtr.ori$FRCI
writeRaster(mtr.sub, "PROCESSED_DATA/metric_scan_angle_effect_diff.tif", 
            overwrite = TRUE)

# center grid
# sel_grid was created in a
centergrd <- st_centroid(sel_grid) 
spgrd <- as(centergrd, "Spatial")

# stacking FRCI only and save
mtr.sta <- stack(mtr.nar$naFRCI, mtr.ori$FRCI)
writeRaster(mtr.sta, "PROCESSED_DATA/metric_scan_angle_effect.tif")
mtr.sp <- raster::extract(mtr.sta, spgrd, sp = TRUE) # spatial object is created after extraction
mtr.sp$diff <- mtr.sp$naFRCI - mtr.sp$FRCI
writeOGR(mtr.sp, dsn = "PROCESSED_DATA/metric_scan_angle_effect.shp",
         layer = "metric_scan_angle_effect", 
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# ARCI vs FRCI in shapefile format
mtr.acci <- raster::extract(arci.frci, spgrd, sp = TRUE) 
mtr.acci$diff <- mtr.acci$naARCI - mtr.acci$naFRCI
writeOGR(mtr.acci, dsn = "PROCESSED_DATA/metric_arci_vs_arci_narrow_scangle.shp",
         layer = "metric_arci_vs_arci_narrow_scangle", 
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# plotting
# extraction FRCI stacks
mtr.all <- raster::extract(mtr.sta, spgrd)
mtr.dfa <- as.data.frame(mtr.all)
mtr.ord <- mtr.dfa[order(mtr.dfa$naFRCI),]
mtr.ord$sortID <- 1:nrow(mtr.ord)

ggplot(mtr.ord, aes(x=sortID)) + geom_line(aes(y = naFRCI, colour = "naFRCI")) + 
  geom_line(aes(y = FRCI, colour = "FRCI")) + 
  geom_point(aes(y=FRCI)) + geom_point(aes(y=naFRCI)) +
  scale_color_manual(values = c("red", "black")) + ylab("CC Index") + xlab("obs.index")

# stacking ARCI dan FRCI
arci.frci <- stack(mtr.nar$naARCI, mtr.nar$naFRCI)
df.acci <- as.data.frame(arci.frci)
df.acci <- df.acci[order(df.acci$naARCI), ]
df.acci$sortID <- 1:nrow(df.acci)
ggplot(df.acci, aes(x=sortID)) + geom_line(aes(y = naFRCI, colour = "naFRCI")) + 
  geom_line(aes(y = naARCI, colour = "naARCI")) + 
  #geom_point(aes(y=naFRCI)) + geom_point(aes(y=naARCI)) +
  scale_color_manual(values = c("red", "black")) + ylab("CC Index") + xlab("obs.index")


# check PCLs in a grid ----
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


# grid 02: start=c(487350.0, 9767710.0)
# do the work please

# raster based CC index ----
# by point to raster
r_ndsm <- grid_canopy(ndsm, res = 0.5, 
                      p2r(subcircle = 0.2, na.fill = kriging() ))
saveRDS(r_ndsm, "PROCESSED_DATA/LINE_2_82/raster_chm_p2r.rds")
writeRaster(r_ndsm, "PROCESSED_DATA/LINE_2_82/raster_chm_p2r.tif",
            overwrite = TRUE)
plot(r_ndsm, col = height.colors(40))

# by pit2free algorithm
r_pfr <- grid_canopy(ndsm, res = 0.5, pitfree(c(0,2,5,10,15,20), c(0, 1.5)))
saveRDS(r_pfr, "PROCESSED_DATA/LINE_2_82/raster_chm_pit2free.rds")
writeRaster(r_pfr, "PROCESSED_DATA/LINE_2_82/raster_chm_pit2free.tif",
            overwrite = TRUE)
plot(r_pfr, col = height.colors(40))

# by pit2free algorithm v2
r_pfr2 <- grid_canopy(ndsm, res = 0.5, 
                     pitfree(c(0,2,5,10,15,20), c(0, 1.5), subcircle = 0.2))
saveRDS(r_pfr2, "PROCESSED_DATA/LINE_2_82/raster_chm_pit2free_disk.rds")
writeRaster(r_pfr2, "PROCESSED_DATA/LINE_2_82/raster_chm_pit2free_disk.tif",
            overwrite = TRUE)
plot(r_pfr2, col = height.colors(40))

# zonal operations
r_pfr2[r_pfr2 < 2.5] <- NA
mask.pfr <- mask(r_pfr2, mtr.ori)


plot(r_pfr2, col = height.colors(40))

align.pfr <- alignExtent(mtr.ori, r_pfr2, snap = 'near')
npcl <- zonal(crop.pfr, mtr.ori$ARCI, fun = 'sum', na.rm = TRUE )
merge.pfr <- merge(r_pfr2, raster(align.pfr)
