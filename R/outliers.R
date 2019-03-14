library(lidR)
library(magrittr)
library(dplyr)
library(raster)
library(sf)

options(digits = 22)
rm(list=ls())
# the data ----
# there are two data, with and without very extreme z values outliers
las <- readLAS("PROCESSED_DATA/TEST_LAS_INP/LN81SUB.las")
plot(las) # with outliers
lascheck(las)
las <- lasfilter(las, duplicated(las@data, by = c("X", "Y")))
sum(duplicated(las@data, by=c("X", "Y", "Z")))
sum(duplicated(las@data, by=c("X", "Y")))

layout = raster(extent(las))
res(layout) = 0.25
dtm <- grid_terrain(las, res=layout, algorithm=knnidw())
dtm2 <- grid_terrain(las, res=layout, algorithm=kriging())
w_norm <- lasnormalize(las, dtm, na.rm=TRUE)
plot(w_norm)
w_ftr <- lasfilter(w_norm, Z >= 0, Z <= 80)
w_ftr
plot(w_ftr)
summary(w_norm@data$Z)
w_gnd <- lasfilter(las, Classification == 2)
plot(w_gnd)
summary(w_gnd@data$Z)

n_out <- readLAS("PROCESSED_DATA/TEST_LAS_INP/LN81SUB2.laz")
plot(n_out) # no outliers
nrow(n_out@data)

dem <- brick("ANCILLARY/south-sumatra-dem90m.tif")

plot(las, col="Classification")

# using its own dsm
n_out <- lasfilterduplicates(n_out)

layout = raster(extent(n_out))
res(layout) = 0.25

dsm <- grid_terrain(n_out, res=layout, algorithm=kriging(), keep_lowest=TRUE)
plot(dsm)
lfnorm <- lasnormalize(lfout, dsm)

las <- readLAS("/DATA/LIDAR CIDANAU BANTEN/07. POINT CLOUD/AREA_1_ALL _CLASS.las")
dsm <- grid_terrain(las, res=0.5, algorithm=kriging())
lnorm <- lasnormalize(las, dsm)



# using simple treshold: SUCCESS !! ----
# asumsinya: dari ketinggian minimum + 100 m tidak akan melebihi tinggi pepohonan
# 
# first dataset
las_out <- lasfilter(las, Z > (min(Z) + 100))
plot(las_out)


las_ftr <- lasfilter(las, Z <= (min(Z) + 100))
plot(las_ftr)
dsm <- grid_terrain(las_ftr, res=0.25, algorithm=kriging(), keep_lowest=TRUE)

# second dataset
las_out2 <- lasfilter(n_out, Z <= (min(Z) + 100))
plot(las_out2)
nrow(las_out2@data)

# using kmeans: SUCCESS !! ----
# this algorithm is meant to remove abnormal high points above the forests
# with slopy terrain 

## implemented in the sel_preprocess.R

# problem dengan kmeans: tidak bisa digunakan untuk normal data
# kecuali ada algorithm tambahan untuk mengisolasi/memfilter k-points dengan
# center yang abnormal

# third way: using ancillary coarser DTM for prior information
# about the the average height over the place of the concerned dataset
# then use the first algorithm above by replacing the min.value with DTM value 
# (already implemented with k-mean algorithm above)

# by using boxplot: NOT REPLICABLE ----
# test with unremoved point clouds from (lidar) sensor platform
z <- las@data$Z
z_out <- boxplot.stats(z, coef=5.0)$out # for very very extreme values, set coef.> 5
# so z_out is the outlier values within Z vector that you want to remove
# using long whisker is better than SOR, since it may remove many cloud points, --
# including point clouds that were from high trees (emergence)

las_out <- lasfilter(las, (Z %in% z_out))
las_ftr <- lasfilter(las, !(Z %in% z_out))

plot(las_ftr)
plot(las_out)

dtm <- grid_terrain(las_ftr, res = 0.25, algorithm = knnidw(k=10L, p=2))
las_nrm <- lasnormalize(las_ftr, dtm)
summary(las_nrm@data$Z)
nrow(dplyr::filter(las_nrm@data, Z < 0))

writeLAS(las_nrm, "PROCESSED_DATA/TEST_LAS_OUT/LINE_8_1_nrm.laz")

# using Rlof package : FAIL ----
# due to low support on huge data
# Rlof::lof(z, 10, cores=4) # 

# using fpoutliers : FAIL ----
# not sure if it is right
# library(fpmoutliers)
# 
# df <- read.csv(system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
# head(df)
# fpi_xyz <- fpmoutliers::FPI(xyz@data)  








