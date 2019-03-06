library(lidR)
library(magrittr)
library(dplyr)

rm(list=ls())
# the data ----
# there are two data, with and without very extreme z values outliers
w_out <- readLAS("PROCESSED_DATA/TEST_LAS_INP/LN81SUB.laz")
plot(w_out) # with outliers

n_out <- readLAS("PROCESSED_DATA/TEST_LAS_INP/LN81SUB2.laz")
plot(n_out) # no outliers


# using simple treshold: SUCCESS !! ----
# asumsinya: dari ketinggian minimum + 100 tidak melebihi tinggi pepohonan
# first dataset
las_out <- lasfilter(w_out, Z > (min(Z) + 100))
plot(las_out)

las_ftr <- lasfilter(w_out, Z <= (min(Z) + 100))
plot(las_ftr)

# second dataset
las_out <- lasfilter(n_out, Z > (min(Z) + 100))
nrow(las_ftr2@data)


# since trees will not reach 100 m
# therefore, 
# actually this technique is so simple that I was not aware of


# using kmeans: SUCCESS !! ----
z <- kmeans(w_out@data$Z, 5)
w_out@data$clust <- z$cluster
las_out3 <- lasfilter(w_out, clust == which(z$centers == max(z$centers)))
plot(las_out3)

# by using boxplot: NOT REPLICABLE ----
# test with unremoved point clouds from (lidar) sensor platform
z <- w_out@data$Z
z_out <- boxplot.stats(z, coef=5.0)$out # for very very extreme values, set coef.> 5
# so z_out is the outlier values within Z vector that you want to remove
# using long whisker is better than SOR, since it may remove many cloud points, --
# including point clouds that were from high trees (emergence)

las_out <- lasfilter(w_out, (Z %in% z_out))
las_ftr <- lasfilter(w_out, !(Z %in% z_out))

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








