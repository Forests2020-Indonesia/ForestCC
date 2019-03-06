library(lidR)
library(grDevices)

l <- readLAS("D:/FORESTS2020/DATA/LIDAR GIZ/LAS/Point Cloud GIZ Buffer 10m/line_8/LINE_8_1.las")

# lraw <- readLAS("D:/FORESTS2020/DATA/LIDAR GIZ/LAS/Point Cloud GIZ Buffer 10m/line_8/LINE_8_1.las", filter="-drop_z_below 0")
# writeLAS(lraw, "EXAMPLE_DATA/LINE_8_1.laz")
# rm(lraw)

z <- l@data$Z
class(z)
z
z[z %in% boxplot.stats(z)$out]
zout <- z[!(z %in% boxplot.stats(z)$out)]
length(zout)

zout2 <- z[!(z %in% boxplot.stats(z, coef = 5.0)$out)]
length(zout2)

z_ftr <- lasfilter(l, Z %in% zout)

dtm <- grid_terrain(z_ftr, res = 0.25, algorithm = knnidw(k=10L, p=2))
lnorm <- lasnormalize(z_ftr, dtm)

writeLAS(lnorm, "PROCESSED_DATA/LAS_OUT/lnorm.laz")


lnorm <- readLAS("PROCESSED_DATA/LAS_OUT/lnorm.laz")
plot(lnorm)

nrow(dplyr::filter(l@data, Z > 100))
lzLT100 <- lasfilter(l, Z < 100)
dtm2 <- grid_terrain(lzLT100, res = 0.25, algorithm = knnidw(k=10L, p=2))
dtm3 <- grid_terrain(lzLT100, res = 0.25, algorithm = kriging())

lnorm2 <- lasnormalize(lzLT100, dtm2)
plot(lnorm2)
summary(lnorm2@data$Z)
nrow(dplyr::filter(lnorm2@data, Z < 0))










