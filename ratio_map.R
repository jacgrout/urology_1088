
library(StrategyUnitTheme)
# ------

su_theme_cols(
 palette = c(NA, "main", "oranges", "charcoals", "slates", "reds", "blues")
)

bph_providers <- read_excel("data/bph_providers.xlsx") |> 
  clean_names() |>
  filter(post_code != "NULL") 

oab_providers <- read_excel("data/oab_providers.xlsx") |> 
  clean_names() |>
  filter(post_code != "NULL") 

ppi_providers <- read_excel("data/ppi_providers.xlsx") |> 
  clean_names() |>
  filter(post_code != "NULL") 

bph_providers_sf <- st_as_sf(bph_providers, coords = c('longitude_1m', 'latitude_1m'), crs=4326)

oab_providers_sf <- st_as_sf(oab_providers, coords = c('longitude_1m', 'latitude_1m'), crs=4326)

ppi_providers_sf <- st_as_sf(ppi_providers, coords = c('longitude_1m', 'latitude_1m'), crs=4326)

subicb_bph_ratio <- left_join (subicbshape,bph_activity,  by = "sub_icb_location_code")

subicb_oab_ratio <- left_join (subicbshape,oab_activity,  by = "sub_icb_location_code")

subicb_ppi_ratio <- left_join (subicbshape,ppi_activity,  by = "sub_icb_location_code")




tm_shape(subicb_bph_ratio) + 
  tm_fill("activityneedratio",title="Activity/Need Ratio BPH",style = "pretty", palette = "-viridis") +
  tm_borders("grey25", alpha=.5) +  
 # tm_shape(bph_providers_sf) + 
#  tm_bubbles(size="num",labels="name") + 
  tm_shape(bph_providers_sf) + 
  tm_dots(col = "grey",size = 0.05)+
          #col= "role",legend.show = T) +
  #tm_text("provider_code")
  tm_basemap(server = c('OpenStreetMap'))




tm_shape(subicb_oab_ratio) +
  tm_fill("activityneedratio",title="Activity/Need Ratio OAB",style = "pretty", palette = "YlOrRd") +
  tm_borders("grey25", alpha=.5) +
  tm_shape(oab_providers_sf) + 
  tm_dots(col = "grey",size = 0.05)+
  tm_basemap(server = c('OpenStreetMap'))

tm_shape(subicb_ppi_ratio) +
  tm_fill("activityneedratio",title="Activity/Need Ratio PPI",style = "pretty", palette = "YlOrRd") +
  tm_borders("grey25", alpha=.5) +
  tm_shape(ppi_providers_sf) + 
  tm_dots(col = "grey",size = 0.05)+
  tm_basemap(server = c('OpenStreetMap'))



bph_activity_icb <- bph_activity_icb |> rename(sub_icb_location_code=sub_icb)

subicb_bph_ratio_icb <- left_join (subicbshape,bph_activity_icb,  by = "sub_icb_location_code")

tm_shape(subicb_bph_ratio_icb) +
  tm_fill("activityneedratio",title="Activity/Need Ratio BPH",style = "pretty", palette = "YlOrRd") +
  tm_borders("grey25", alpha=.5) 

tmap

 