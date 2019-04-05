library(dplyr)
library(magrittr)
library(sf)
library(reshape2)
library(ggplot2)

rm(list=ls())

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
"LCLidar2" = "Lidar's Landcover"
)

# Band subset
bands <- sel_metrics2 %>% 
  dplyr::select(FRCI, b2, b3, b4, b5, b6, b7, LClidar2) %>% 
    st_drop_geometry %>%
      melt(id.vars = c("FRCI", "LClidar2"))

hutan <- bands %>% 
          filter(LClidar2 %in% 
                   c("Hutan Tanaman", "Hutan Mangrove Primer", 
                     "Hutan Rawa", "Kebun Sawit"))

gbands <- hutan %>% ggplot(aes(x = value, y = FRCI, color=LClidar2)) + 
            geom_point(alpha=0.25) + 
              facet_wrap(~variable, labeller = as_labeller(layer_names),
                         scales = "free")
gbands + labs(x = "Spectral response") +
  scale_color_discrete(name="Landcover", 
                       labels = c("Mangrove forest",
                                  "Peatswamp forest",
                                  "Forest plantation",
                                  "Oil palm"))

# spectral index subset
idx <- sel_metrics2 %>% 
  dplyr::select(FRCI, NBR, NDVI, NDWI, SATVI, NDSI, LClidar2) %>% 
    st_drop_geometry %>%
      melt(id.vars = c("FRCI", "LClidar2"))

idxhutan <- idx %>% 
  filter(LClidar2 %in% 
           c("Hutan Tanaman", "Hutan Mangrove Primer", 
             "Hutan Rawa", "Kebun Sawit"))

gidx <- idxhutan %>% ggplot(aes(x = value, y = FRCI, color = LClidar2)) +
  geom_point(alpha=0.25) + 
    facet_wrap(~variable, scales = "free")

gidx + labs(x = "Spectral index response") +
  scale_color_discrete(name="Landcover", 
                       labels = c("Mangrove forest",
                                  "Peatswamp forest",
                                  "Forest plantation",
                                  "Oil palm"))

# Tasseled Cap subset
tcap <- sel_metrics2 %>% 
  dplyr::select(FRCI, TCA, TCD, TCB, TCG, TCW,  LClidar2) %>% 
    st_drop_geometry %>%
      melt(id.vars = c("FRCI", "LClidar2"))

tchutan <- tcap %>% 
  filter(LClidar2 %in% 
           c("Hutan Tanaman", "Hutan Mangrove Primer", 
             "Hutan Rawa", "Kebun Sawit"))

gtcap <- tchutan %>% ggplot(aes(x = value, y = FRCI, color = LClidar2)) +
  geom_point(alpha=0.2) + 
    facet_wrap(~variable, scales = "free")

gtcap + labs(x = "Tasseled cap response") +
  scale_color_discrete(name="Landcover", 
                       labels = c("Mangrove forest",
                                  "Peatswamp forest",
                                  "Forest plantation",
                                  "Oil palm"))

# texture mean subset

meantxt <- sel_metrics2 %>% 
  dplyr::select(FRCI, LClidar2, Tb4M, Tb5M, Tb6M, Tb7M) %>% 
    st_drop_geometry %>%
      melt(id.vars = c("FRCI", "LClidar2"))

meantxthutan <- meantxt %>% 
  filter(LClidar2 %in% 
           c("Hutan Tanaman", "Hutan Mangrove Primer", 
             "Hutan Rawa", "Kebun Sawit"))

gtxt <- meantxthutan %>% ggplot(aes(x = value, y = FRCI, color = LClidar2)) +
  geom_point(alpha=0.25) + 
    facet_wrap(~variable, 
               labeller = as_labeller(layer_names),
               scales = "free")

gtxt + labs(x = "GLCM-mean response") +
  scale_color_discrete(name="Landcover", 
                       labels = c("Mangrove forest",
                                  "Peatswamp forest",
                                  "Forest plantation",
                                  "Oil palm"))

# texture variance subset

vartxture <- sel_metrics2 %>% 
  dplyr::select(FRCI, LClidar2, Tb4V, Tb5V, Tb6V, Tb7V) %>% 
    st_drop_geometry %>%
      melt(id.vars = c("FRCI", "LClidar2"))

vartxthutan <- vartxture %>% 
  filter(LClidar2 %in% 
           c("Hutan Tanaman", "Hutan Mangrove Primer", 
             "Hutan Rawa", "Kebun Sawit"))

gtxtvar <- vartxthutan %>% ggplot(aes(x = value, y = FRCI, color = LClidar2)) +
  geom_point(alpha=0.2) + 
    facet_wrap(~variable, 
               labeller = as_labeller(layer_names),
               scales = "free")

gtxtvar + labs(x = "GLCM-variance response") +
  scale_color_discrete(name="Landcover", 
                       labels = c("Mangrove forest",
                                  "Peatswamp forest",
                                  "Forest plantation",
                                  "Oil palm"))

# intensity subset

intensity <- sel_metrics2 %>% 
  dplyr::select(b4, ITY, LClidar2) %>% st_drop_geometry 

(g_int <- intensity %>% ggplot(aes(x = b4, y = ITY, color = LClidar2)) +
  geom_point(alpha=0.25))

