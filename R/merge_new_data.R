# merging dataset pertama (sel_metrics-v2) dan kedua (sel_metrics2)
# rearrange the attribute and attribute values

sel_metrics1 <- read_sf("PROCESSED_DATA/FINAL_SEL_METRICS/sel_metrics-v2.shp")
sel_metrics2 <- read_sf("PROCESSED_DATA/FINAL_SEL_METRICS/sel_metrics2.shp")

table(sel_metrics1$LClidar2)
table(sel_metrics2$LC_Lidar)

# setelah melihat situasi data di atas, diputuskan disini untuk mengambil data air saja
# first thing first is to add new LClidar2 attribute to 
# (hopefully) equalize the number of attribute and their names 

names(sel_metrics1)
names(sel_metrics2)

sel_metrics2 %<>% mutate(LClidar2 = LC_Lidar)
wtr_feature <- sel_metrics2 %>% filter(LClidar2 == "Badan Air")
head(wtr_feature)
nrow(wtr_feature)

names(wtr_feature)
sel_metrics3 <- rbind(sel_metrics1, wtr_feature) # can't use dplyr::bind_rows??

write_sf(sel_metrics3, "PROCESSED_DATA/FINAL_SEL_METRICS/sel_metrics3.shp")

table(sel_metrics3$LClidar2)
