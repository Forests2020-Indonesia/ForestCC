# codes for machine learning

# play with pixel qa----
# refine samples ----
# :: only use clear pixel
# see: Landsat 8 Surface Reflectance Code LaSRC Product Guide pp. 13 Table 7-3

library(sf)
library(dplyr)
library(magrittr)

rm(list=ls())

pixqa <- list(
clear = c(322, 386, 834, 898, 1346),
water = c(324, 388, 836, 900, 1348),
cloud_shadow = c(328, 392, 840, 904, 1350),
snow_ice = c(336, 368, 400, 432, 848, 880, 912, 944, 1352),
cloud = c(352, 368, 416,432, 480, 864, 880, 928, 944, 992),
low_cfd_cloud = c(322, 324, 328, 336, 352, 368, 834, 836, 840, 848, 864, 880),
med_cfd_cloud = c(386, 388, 392, 400, 416, 432, 898, 900, 904, 928, 944),
hgh_cfd_cloud = c(480, 992),
low_cfd_cirrus = c(322, 324, 328, 336, 352, 368, 386, 388, 392, 400, 416, 432, 480),
hgh_cfd_cirrus = c(834, 836, 840, 848, 864, 880,898,900, 904, 912, 928, 944, 992),
terrain_oclusn = c(1346, 1348, 1350, 1352))

sel_metrics <- st_read("PROCESSED_DATA/FINAL_SEL_METRICS/sel_metrics.shp")
sel_metrics %$% table(LClidar2)
sel_metrics %>% 
  filter(LClidar2 == 'Hutan Mangrove Primer') %$% unique(tileID)

sel_metrics2 <- sel_metrics %>% 
  filter(!((bQ %in% pixqa[["cloud_shadow"]]) | 
                          (bQ %in% pixqa[["cloud"]]) |
                          (bQ %in% pixqa[["hgh_cfd_cloud"]]) |
                          (bQ %in% pixqa[["hgh_cfd_cirrus"]])))

write_sf(sel_metrics2, "PROCESSED_DATA/FINAL_SEL_METRICS/sel_metrics2.shp")

