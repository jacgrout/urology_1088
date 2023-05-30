
library(StrategyUnitTheme)
# ------

su_theme_cols(
 palette = c(NA, "main", "oranges", "charcoals", "slates", "reds", "blues")
)

bph_providers <- read_excel("data/bph_providers.xlsx") |> 
  clean_names() |>
  filter(post_code != "NULL")  |>
  mutate(short_role = case_when (role == "NHS TRUST SITE" ~ "NHS",
                                 .default = "Independent")
  )

oab_providers <- read_excel("data/oab_providers.xlsx") |> 
  clean_names() |>
  filter(post_code != "NULL")  |>
  mutate(short_role = case_when (role == "NHS TRUST SITE" ~ "NHS",
                                 .default = "Independent")
  )

ppi_providers <- read_excel("data/ppi_providers.xlsx") |> 
  clean_names() |>
  filter(post_code != "NULL") |>
  mutate(short_role = case_when (role == "NHS TRUST SITE" ~ "NHS",
                           .default = "Independent")
         )

bph_providers_sf <- st_as_sf(bph_providers, coords = c('longitude_1m', 'latitude_1m'), crs=4326)

oab_providers_sf <- st_as_sf(oab_providers, coords = c('longitude_1m', 'latitude_1m'), crs=4326)

ppi_providers_sf <- st_as_sf(ppi_providers, coords = c('longitude_1m', 'latitude_1m'), crs=4326)

subicb_bph_ratio <- left_join (subicbshape,bph_activity,  by = "sub_icb_location_code")

subicb_oab_ratio <- left_join (subicbshape,oab_activity,  by = "sub_icb_location_code")

subicb_ppi_ratio <- left_join (subicbshape,ppi_activity,  by = "sub_icb_location_code")

saveRDS(subicb_bph_ratio,"subicb_bph_ratio.rds")
saveRDS(subicb_oab_ratio,"subicb_oab_ratio.rds")
saveRDS(subicb_ppi_ratio,"subicb_ppi_ratio.rds")
saveRDS(bph_providers_sf,"bph_providers_sf.rds")
saveRDS(oab_providers_sf,"oab_providers_sf.rds")
saveRDS(ppi_providers_sf,"ppi_providers_sf.rds")

tm_shape(subicb_bph_ratio) + 
  tm_fill("activityneedratio",title="Activity/Need Ratio BPH",style = "pretty", palette = "-viridis") +
  tm_borders("grey25", alpha=.5) +  
 # tm_shape(bph_providers_sf) + 
#  tm_bubbles(size="num",labels="name") + 
  tm_shape(bph_providers_sf) + 
  tm_dots(col = "grey", size = 0.05,legend.show = T)+
          #col= "short_role",legend.show = T) +
  #tm_text("provider_code")
  tm_basemap(server = c('OpenStreetMap'))




tm_shape(subicb_oab_ratio) +
  tm_fill("activityneedratio",title="Activity/Need Ratio OAB",style = "pretty", palette = "-viridis") +
  tm_borders("grey25", alpha=.5) +
  tm_shape(oab_providers_sf) + 
  tm_dots(col = "short_role",size = 0.05)+
  tm_basemap(server = c('OpenStreetMap'))

tm_shape(subicb_ppi_ratio) +
  tm_fill("activityneedratio",title="Activity/Need Ratio PPI",style = "pretty", palette = "-viridis") +
  tm_borders("grey25", alpha=.5) +
  tm_shape(ppi_providers_sf) + 
  tm_dots(col = "short_role",size = 0.05)+
  tm_basemap(server = c('OpenStreetMap'))



#bph_activity_icb <- bph_activity_icb |> rename(sub_icb_location_code=sub_icb)

subicb_bph_ratio_icb <- left_join (subicbshape,bph_activity_icb,  by = "sub_icb_location_code")

tm_shape(subicb_bph_ratio_icb) +
  tm_fill("activityneedratio",title="Activity/Need Ratio BPH",style = "pretty", palette = "-viridis") +
  tm_borders("grey25", alpha=.5) 

#tmap



subicb_oab_ratio_icb <- left_join (subicbshape,oab_activity_icb,  by = "sub_icb_location_code")

tm_shape(subicb_oab_ratio_icb) +
  tm_fill("activityneedratio",title="Activity/Need Ratio OAB",style = "pretty", palette = "-viridis") +
  tm_borders("grey25", alpha=.5) 

#tmap


subicb_ppi_ratio_icb <- left_join (subicbshape,ppi_activity_icb,  by = "sub_icb_location_code")

tm_shape(subicb_ppi_ratio_icb) +
  tm_fill("activityneedratio",title="Activity/Need Ratio PPI",style = "pretty", palette = "-viridis") +
  tm_borders("grey25", alpha=.5) 

#tmap
 