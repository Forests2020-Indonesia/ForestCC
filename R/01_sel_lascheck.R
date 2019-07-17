# loop through datset
   # drop below 0
   # lascheck
   # print(las) (to check pcl density), etc
   # check suitability of classification
   # check minimum and maximum height
   # give height plot
# end

options(stringsAsFactors = FALSE)
options(digits = 22)

library(lidR)
library(dplyr)
library(magrittr)

#as_tibble(read.csv("/DATA/LIDAR GIZ/CLASSIFICATION_META/line_2.csv"))
listFiles <- read.csv("/DATA/LIDAR GIZ/CLASSIFICATION_META/line_2.csv")[,1]
lasPath <- "/DATA/LIDAR GIZ/LAS/"

# fname <- "line_2_81"
chkLas = function(fname)
{
  
  spl <- strsplit(fname, "_")
  linePath <- sapply(spl, function(x) paste0(x[1], "_", x[2]))
  linePath <- tolower(base::unique(linePath))
  
  # 
  las <- readLAS(paste0(lasPath, linePath, "/", fname, ".las"))
  
  if(is.empty(las)) return(NULL)
  writeLines("\n")  
  
  cat(fname)
  # lascheck
  lascheck(las)
  
  # print las info
  writeLines("\n")
  print(las)
  
  # classification
  writeLines("\n")
  cat("Unique class: ",unique(las@data$Classification))
  print(table(las@data$Classification))
  
  # height
  writeLines("\n")
  cat("MinZ:", min(las@data$Z), " MaxZ:", max(las@data$Z))
  writeLines("\n")
  
  # plot
  jpeg(paste0("PROCESSED_DATA/PLOTS_LASCHECK/", fname, ".jpg")) # the output folder should be predetermined
  graphics::hist(las@data$Z, main=fname, breaks=15)
  dev.off()
  
}

sink("PROCESSED_DATA/LOGS/01_lascheck.txt")
sapply(listFiles, FUN = chkLas)
closeAllConnections()
