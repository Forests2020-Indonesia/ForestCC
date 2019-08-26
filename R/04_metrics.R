library(lidR)
library(raster)
library(sf)
library(rgdal)
library(magrittr)
library(dplyr)


ccmetrics = function(z, htree, rn, nr, cls)
{
  
  vg <- (cls >= 3 & cls <= 5) # vegetation
  
  # arci = all return canopy index
  ar <- (z >= 0) # semua return
  ac =  vg & (z >= htree) # return dari kanopi harus dari vegetasi dengan ketinggian minimal 2.5m (pengalaman field work di RMU)
  arci = sum(ac) / sum(ar)
  
  # FRCI = first return canopy index
  sr <- (nr == 1) # single return when only 1 beam and 1 return )
  fr <- (nr  > 1) & (rn == 1)  # first return from many return
  
  # frci denominator
  fc <- fr & vg & (z >= htree) 
  sc <- sr & vg & (z >= htree)
  
  frci = (sum(sc) + sum(fc)) / (sum(sr) + sum(fr))
  
  # print(paste0("FRCI = ", frci))
  # return all indexes
  return(list(ARCI = arci, FRCI = frci))
}

calc_pos <- function(chunk_name) 
{
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
  
  # the operator <<- used to create and assign a global variable within a function 
  # see:  https://stackoverflow.com/questions/10904124/global-and-local-variables-in-r
  
  fl_data <- st_read(paste0(aoidir, fn_flightline)) # 
  vg_data <- st_read(paste0(vgrdir, fn_vecgrid))
  sel_tile <- fl_data %>% filter(TxtMemo == tilename) # select which tile
  
  sel_grid <<- vg_data[sel_tile, op=st_within] # select the grid within sel_tile boundary
  sel_grid$gridID <- 1:nrow(sel_grid) # add ID for selection
  
  grid01 = sel_grid %>% filter(gridID == 1) # select the first grid
  pos <- raster::extent(grid01)
  
  return(pos)
}

calc_metrics <- function(chunk, resolution)
{
  las <- readLAS(chunk)
  if(is.empty(las)) return(NULL)
  
  coords <- calc_pos(chunk@files)
  
  cci <- grid_metrics(las, 
                          ccmetrics(Z, htree=2.5, ReturnNumber, NumberOfReturns, Classification), 
                          res=resolution,
                          start=c(coords[1], coords[3]))

  return(cci)
}

baseOutFolder <- "/FORESTS2020/CODES/ForestCC/PROCESSED_DATA/V03/METRICS/CC/" # the base folder where output will be placed (see notes below)
baseInpFolder <- "/FORESTS2020/CODES/ForestCC/PROCESSED_DATA/V03/NORMALIZED/LASNORM" # folder in which the line folders exist
listInpFolder <- list.dirs(baseInpFolder)

# folder for AOI
aoidir <- "/DATA/LIDAR GIZ/AOI/" 
vgrdir <- "/DATA/LIDAR GIZ/AOIGRID/"


for(i in 2:length(listInpFolder)) {  # the subfolder starts from index no.2
  
  # setting the process paramaters
  ctg <- catalog(listInpFolder[i])
  opt_chunk_buffer(ctg) <- 0
  opt_chunk_size(ctg)   <- 0 
  opt_cores(ctg) <- 5
  
  splitName <- strsplit(listInpFolder[i], "/")
  shortLineName <- splitName[[1]][length(splitName[[1]])]
  dir4Las = paste0(baseOutFolder, shortLineName)
  
  if(!dir.exists(dir4Las)) dir.create(dir4Las)
  opt_output_files(ctg) <- paste0(dir4Las, "/{ORIGINALFILENAME}")
  
  catalog_apply(ctg, FUN = function(x) calc_metrics(x, 30))

}


# NOTES:
# The structure of lidar input data:
# - there is a single folder that contains subfolders represents lidar line
# - within line subfolder, the lidar line data is divided into several lidar tiles
# - each lidar dataset (las) represent a tile

# the output is also mimic the input structure





