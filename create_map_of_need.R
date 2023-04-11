library(sf)
library(tmap)
library(dplyr)
library(stringr)
library(readxl)

#Get the ICB shape file
subicbshape <- st_read(dsn ="Z:/Data/GIS files/Polygon Data/Sub_Integrated_Care_Board_Locations_(July_2022)_EN_BFC/SICBL_JUL_2022_EN_BFC.shp")

icbshape <- st_read(dsn ="Z:/Data/GIS files/Polygon Data/ICBs_July2022/ICB_July2022.shp")

subicb_to_icb_lookup <- read_excel("data/subicb_to_icb_lookup.xlsx") |> 
  clean_names() |> 
  filter(region_name == "MIDLANDS COMMISSIONING REGION")

subicb_to_icb_lookup <- select(subicb_to_icb_lookup,icb_code,sub_icb_location_ods_code) |>
  rename(sub_icb_location_code = sub_icb_location_ods_code)


subicbshape <- subicbshape |>
  mutate(sub_icb_location_code = str_extract(SICBL22NM, "[0-9A-Z]{3,5}$"))



subicbshape <- subicbshape |> left_join(subicb_to_icb_lookup)

subicbshape <- subicbshape |>
 filter(icb_code != "NA")

subicb_oab <- left_join (subicbshape,list_size_subicb_prev_oab,  by = "sub_icb_location_code")
subicb_bph <- left_join (subicbshape,list_size_subicb_prev_bph,  by = "sub_icb_location_code")


tmap_options(check.and.fix = TRUE)
tmap_mode("view")
tm_shape(subicb_bph) +
  tm_fill("overall_prev",title="Prevalence BPH",style = "pretty", palette = "YlOrRd") +
  tm_borders("grey25", alpha=.5) 

tm_shape(subicb_oab) +
  tm_fill("overall_prev",title="Prevalence OAB",style = "pretty", palette = "YlOrRd") +
  tm_borders("grey25", alpha=.5) 

subicb_bph_ratio <- left_join (subicbshape,bph_activity,  by = "sub_icb_location_code")


tm_shape(subicb_bph_ratio) +
  tm_fill("activityneedratio",title="Activity/Need Ratio BPH",style = "pretty", palette = "YlOrRd") +
  tm_borders("grey25", alpha=.5) 




 
