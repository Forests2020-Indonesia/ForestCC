# loop through folder
# make catalog
  # loop through tiles
    # lascheck
  # end
# end

options(stringsAsFactors = FALSE)
library(lidR)
library(dplyr)

as_tibble(read.csv("ANCILLARY/Selected LAS South Sumatra.csv"))
listFiles <- read.csv("ANCILLARY/Selected LAS South Sumatra.csv")[,1]

spl <- strsplit(listFiles, "_")
listFolds <- sapply(spl, function(x) paste0(x[1], "_", x[2]))
listFolds <- tolower(base::unique(listFolds))

pPath <- "/DATA/LIDAR GIZ/LAS (DSM)/Point Cloud GIZ Buffer 10m/"

ctgProcess = function(x)
{
  writeLines(x)
  ctg <- catalog(paste0(pPath, x))
  lascheck(ctg)
}

lapply(listFolds, FUN = ctgProcess)
