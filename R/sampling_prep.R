# codes for machine learning

# play with pixel qa----
# refine samples ----
# :: only use clear pixel
# see: Landsat 8 Surface Reflectance Code LaSRC Product Guide pp. 13 Table 7-3

pixqa <- list(
clr = c(322, 386, 834, 898, 1346),
wtr = c(324, 388, 836, 900, 1348),
cld_shadow = c(328, 392, 840, 904, 1350),
snw_ice = c(336, 368, 400, 432, 848, 880, 912, 944, 1352),
cld = c(352, 368, 416,432, 480, 864, 880, 928, 944, 992),
low_cfd_cloud = c(322, 324, 328, 336, 352, 368, 834, 836, 840, 848, 864, 880),
med_cfd_cloud = c(386, 388, 392, 400, 416, 432, 898, 900, 904, 928, 944),
hgh_cfd_cloud = c(480, 992),
low_cfd_cirrus = c(322, 324, 328, 336, 352, 368, 386, 388, 392, 400, 416, 432, 480),
hgh_cfd_cirrus = c(834, 836, 840, 848, 864, 880,898,900, 904, 912, 928, 944, 992),
ter_ocl = c(1346, 1348, 1350, 1352))

