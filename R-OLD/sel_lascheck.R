# loop through datset
   # drop below 0
   # lascheck
   # print(las) (to check pcl density), etc
   # check suitability of classification
   # check minimum and maximum height
   # give height plot
# end

d1 <- date()
options(stringsAsFactors = FALSE)
options(digits = 22)

library(lidR)
library(dplyr)

as_tibble(read.csv("ANCILLARY/Selected LAS South Sumatra.csv"))
listFiles <- read.csv("ANCILLARY/Selected LAS South Sumatra.csv")[,1]

pPath <- "/DATA/LIDAR GIZ/LAS (DSM)/Point Cloud GIZ Buffer 10m/"

chkLas = function(fname)
{
  
  spl <- strsplit(fname, "_")
  foldpath <- sapply(spl, function(x) paste0(x[1], "_", x[2]))
  foldpath <- tolower(base::unique(foldpath))
  
  las <- readLAS(paste0(pPath, foldpath, "/", fname, ".las"), 
                 filter = "-drop_z_below 0")
  
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
  graphics::hist(las@data$Z, main=fname, breaks=15)
  writeLines("\n")
}

sapply(listFiles, FUN = chkLas)
d2 <- date()
d2 - d1