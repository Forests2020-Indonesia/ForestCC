library(bfast)
library(rgdal)
library(MODISTools)
library(MODIS)
library(strucchange)
library(zoo)
library(magrittr)



xtrpts <- readOGR("/FORESTS2020/CODES/ForestCC/PROCESSED_DATA/MODISBFAST/xtrpts.shp")

# extract date from modis filename
ndate <- extractDate(names(xtrpts), 
                     pos1 = 10, pos2 = 16, 
                     asDate = TRUE, format = "%Y%j")
ndate <- ndate$inputLayerDates[-c(1:2)]
# ndate <- gsub(x=ndate, "-",".")
dfExtr <- xtrpts@data[c(3:length(names(xtrpts)))]
pt0001 <- as.numeric(dfExtr[1,])

ts.pt0001 <- bfastts(pt0001, as.Date(ndate, format="%Y-%m-%d"), type="16-day")
plot(ts.pt0001, main = "EVI 16-day")
bfm1 <- bfastmonitor(ts.pt0001, response ~ trend + harmon, 
                     order = 3, start = c(2013,6))
plot(bfm1)


