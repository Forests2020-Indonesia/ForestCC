library(lidR)
library(raster)
library(sf)
library(rgdal)
library(magrittr)
library(dplyr)


ctg <- catalog("PROCESSED_DATA/SEL_NORM/")
opt_chunk_buffer(ctg) <- 0
opt_cores(ctg) <- 6
opt_output_files(ctg) <- "PROCESSED_DATA/SEL_METRICS/{ORIGINALFILENAME}-metrics"

sel_metrics = function(z, htree, rn, nr, cls, ity)
{
  
  vg <- (cls >= 3 & cls <= 5) # vegetation
  
  # arci = all return canopy index
  ar <- (z >= 0) # semua return
  ac =  vg & (z >= htree) # return dari kanopi harus dari vegetasi dengan ketinggian minimal 2.5m (pengalaman field work di RMU)
  arci = sum(ac) / sum(ar)
  
  # FRCI = first return canopy index
  sr <- (nr == 1) # single return when only 1 beam and 1 return )
  fr <- (rn == 1) & (nr > 1) # frist return from many return
  
  # frci'a denominator
  fc <- fr & vg & (z >= htree) 
  sc <- sr & vg & (z >= htree)
  
  frci = (sum(sc) + sum(fc)) / (sum(sr) + sum(fr))
  
  # mean intensity
  mean_ity <- mean(ity)
  
  # print(paste0("FRCI = ", frci))
  # return all indexes
  return(list(ARCI = arci, FRCI = frci, ITY = mean_ity))
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
embed_theme <- function(sf_metrics, themefolder, thematic = NULL, sel_field)
{
  # images folder
  if(is.null(thematic))
  {
    # will write some code here
    list.files(themefolder)
    # will write some code here
    return(NULL)
  }
  sf_target <- read_sf(paste0(themefolder, thematic))
  sf_metrics <- st_join(sf_metrics, dplyr::select(sf_target, sel_field))
  return(sf_metrics)
}
calc_metrics <- function(chunk, resolution)
{
  las <- readLAS(chunk)
  if(is.empty(las)) return(NULL)
  
  coords <- calc_pos(chunk@files)
  
  lst_metrics <- grid_metrics(las, 
                              sel_metrics(Z, htree=2.5, ReturnNumber, NumberOfReturns, Classification, Intensity), 
                              res=resolution,
                              start=c(coords[1], coords[3]))

  sf_metrics <- lst_metrics %>%
                  rasterToPoints %>%
                   as.data.frame %>%
                      st_as_sf(coords = c("x", "y"), crs=32748)
  
  sf_metrics = sf_metrics[sel_grid, op=st_within]
  
 #  selected image layers were processed by using different script
  sf_metrics <- embed_theme(sf_metrics, 
                            themefolder = "/DATA/LIDAR GIZ/ORTHOPHOTO_CLASSIFICATION/",
                            thematic = paste0(tilename, ".shp"),
                            sel_field = "Tuplah")
  
  #sf_whole <<- tryCatch(get("sf_whole"), error = function(cond) return(NULL))
  #sf_whole <- rbind(sf_whole, sf_metrics)
  
  return(sf_metrics)
  
}

lst_metrics <- catalog_apply(ctg, FUN = function(x) calc_metrics(x, 30))

# embed_images <- function(sf_metrics)
# {
#   # images folder
#   img_dir <- "/DATA/"
#   
# }



