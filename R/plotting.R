library(dplyr)
library(magrittr)
library(sf)
library(reshape2)
library(ggplot2)

sel_metrics2 <- read_sf("PROCESSED_DATA/FINAL_SEL_METRICS/sel_metrics2.shp")

layer_names <- c( 
"ITY" = "Intensity", 
"PL__2015" = "MoEF's 2015 Landcover", "PL__2016" = "MoEF's 2016 Landcover",
"b2" = "Band 2", "b3" = "Band 3", "b4" = "Band 4",
"b5" = "Band 5", "b6" = "Band 6", "b7" = "Band 7", "bQ" = "Band Q/A",          
"NBR"= "Normalized Burnt Ratio",
"NDSI" = "Normalized Difference Soil Index",
"NDVI" = "Normalized Difference Vegetation Index",     
"NDWI" = "Normalized Difference Water Index",
"SATVI"= "Soil Adjusted Total Vegetation Index",
"Tb4M" = "GLCM-Mean B4", "Tb4V" = "GLCM-Variance B4",
"Tb5M" = "GLCM-Mean B5", "Tb5V" = "GLCM-Variance B5",     
"Tb6M" = "GLCM-Mean B6", "Tb6V" = "GLCM-Variance B6",     
"Tb7M" = "GLCM-Mean B7", "Tb7V" = "GLCM-Variance B7",   
"TCA"  = "TC Angle", "TCD" = "TC Distance", "TCB"  = "TC Brightness", 
"TCG" = "TC Greeness", "TCW" = "TC Wetness",
LCLidar2 = "Lidar's Landcover"
)

# Band subset
bands <- sel_metrics2 %>% 
  select(FRCI, b2, b3, b4, b5, b6, b7, LClidar2) %>% 
    st_drop_geometry %>%
      melt(id.vars = c("FRCI", "LClidar2"))

hutan <- bands %>% 
          filter(LClidar2 %in% 
                   c("Hutan Tanaman", "Hutan Mangrove Primer", "Hutan Rawa"))
g1 <- hutan %>% ggplot(aes(x = value, y = FRCI, color=LClidar2)) + 
  geom_point(alpha=0.2) + facet_wrap(~variable, 
                                     labeller = as_labeller(layer_names))
g1
unique(bands$LClidar2)

# vegetation index subset
idx <- sel_metrics2 %>% 
  select(FRCI, NBR, NDVI, NDWI, SATVI, NDSI, LClidar2) %>% 
    st_drop_geometry %>%
      melt(id.vars = c("FRCI", "LClidar2"))

idxhutan <- idx %>% 
  filter(LClidar2 %in% 
           c("Hutan Tanaman", "Hutan Mangrove Primer", "Hutan Rawa"))

g2 <- idxhutan %>% ggplot(aes(x = value, y = FRCI, color = LClidar2)) +
  geom_point(alpha=0.2) + facet_wrap(~variable, labeller = as_labeller(layer_names))

g2

# Tasseled Cap subset
tcap <- sel_metrics2 %>% 
  select(FRCI, TCA, TCD, TCB, TCG, TCW, b6, LClidar2) %>% 
    st_drop_geometry %>%
      melt(id.vars = c("FRCI", "LClidar2"))

tchutan <- tcap %>% 
  filter(LClidar2 %in% 
           c("Hutan Tanaman", "Hutan Mangrove Primer", "Hutan Rawa"))

g3 <- tchutan %>% ggplot(aes(x = value, y = FRCI, color = LClidar2)) +
  geom_point(alpha=0.2) + facet_wrap(~variable, labeller = as_labeller(layer_names))

g3

# texture mean subset

txture <- sel_metrics2 %>% 
  select(FRCI, LClidar2, 
         Tb4M, Tb5M, Tb6M, Tb7M) %>% 
    st_drop_geometry %>%
      melt(id.vars = c("FRCI", "LClidar2"))

txthutan <- txture %>% 
  filter(LClidar2 %in% 
           c("Hutan Tanaman", "Hutan Mangrove Primer", 
             "Hutan Rawa", ))

g4 <- txthutan %>% ggplot(aes(x = value, y = FRCI, color = LClidar2)) +
  geom_point(alpha=0.2) + facet_wrap(~variable, labeller = as_labeller(layer_names))

g4

# texture variance subset
txture <- sel_metrics2 %>% 
  select(FRCI, LClidar2, 
         Tb4V, Tb5V, Tb6V, Tb7V) %>% 
  st_drop_geometry %>%
  melt(id.vars = c("FRCI", "LClidar2"))

txthutan <- txture %>% 
  filter(LClidar2 %in% 
           c("Hutan Tanaman", "Hutan Mangrove Primer", "Hutan Rawa"))

g5 <- txthutan %>% ggplot(aes(x = value, y = FRCI, color = LClidar2)) +
  geom_point(alpha=0.2) + facet_wrap(~variable, labeller = as_labeller(layer_names))

g5

# intensity subset

intensity <- sel_metrics2 %>% select(b4, ITY, LClidar2) %>% st_drop_geometry 

g6 <- intensity %>% ggplot(aes(x = b4, y = ITY)) +
  geom_point(alpha=0.2) 
g6
