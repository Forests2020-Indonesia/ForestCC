library(lidR)

las <- readLAS("/DATA/LIDAR GAL/L_43208/l_43208_411000_9775000.las")
plot(las)
lascheck(las)
unique(las@data$Classification)
