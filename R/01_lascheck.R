# loop through datset
   # drop below 0
   # lascheck
   # print(las) (to check pcl density), etc
   # check suitability of classification
   # check minimum and maximum height
   # give height plot
# end
# the structure of lid should be:
#  base_folder:
#    line_1:
#    line_2:
#    line_i:
#      tile_i_1
#      tile_i_2
#      tile_i_j

options(stringsAsFactors = FALSE)
options(digits = 22)

library(lidR)
library(dplyr)
library(magrittr)

chkLas = function(fname, line_name)
{
  splitName <- strsplit(fname, "/")
  shortFileName <- splitName[[1]][length(splitName[[1]])]
  
  sinkFileName <- paste0(baseLogFolder, line_name, "/",
                         gsub(".las", ".txt", shortFileName))

  if(file.exists(sinkFileName)) {
    print(paste("File log of: ", fname, "exist!"))
    return(NULL)
  }
  cat(shortFileName)
  las <- readLAS(fname)
  
  if(is.empty(las)) return(NULL)
  writeLines("\n")  
  
  sink(sinkFileName)
  
  # print las info
  print(las); writeLines("\n")
  
  # lascheck
  lascheck(las); writeLines("\n")
  
  # classification
  print("Classification:")
  las@data %>% 
    group_by(as.factor(Classification)) %>% summarize(N = n()) %>% print()
  writeLines("\n")
  
  # height
  print("Height:")
  print(summary(las@data$Z)); writeLines("\n")
  
  # ScanAngleRank (i.e.:Scanning Angle)
  print("Scanning Angle:")
  print(summary(las@data$ScanAngleRank)); writeLines("\n")
  
  closeAllConnections()
  
  # plot
  plotFileName <- paste0(baseFigFolder, line_name,  "/",
                         gsub(".las", ".jpg", shortFileName))       
  jpeg(plotFileName) # the output folder should be predetermined
  graphics::hist(las@data$Z, main=fname, breaks=15)
  dev.off()
  rm(las)
}
 
baseOutFolder <- "/FORESTS2020/CODES/ForestCC/PROCESSED_DATA/V03/LASCHECK/"
baseLogFolder <- paste0(baseOutFolder, "LOGS/")
baseFigFolder <- paste0(baseOutFolder, "FIGS/")

baseInpFolder <- "/DATA/LIDAR GIZ/LAS" # folder in which the line folders exist
listInpFolder <- list.dirs(baseInpFolder)

# listInpFolder <- listInpFolder[1:2] # test

for(i in 2:length(listInpFolder)) {  # the subfolder starts from index no.2
  splitName <- strsplit(listInpFolder[i], "/")
  shortLineName <- splitName[[1]][length(splitName[[1]])]
  
  # create output folders
  dir4Log = paste0(baseLogFolder, shortLineName) # output folder for lascheck perline
  dir4Fig = paste0(baseFigFolder, shortLineName)
  
  if(!dir.exists(dir4Log)) dir.create(dir4Log)
  if(!dir.exists(dir4Fig)) dir.create(dir4Fig)
  
  # list the las files under the base input folder and apply a funtion to check
  listFiles <- list.files(listInpFolder[i], full.names = TRUE)
  sapply(listFiles, FUN = function(x) chkLas(x, shortLineName))
}



