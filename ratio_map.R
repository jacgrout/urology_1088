
# ------

su_theme_cols(
  palette = c(NA, "main", "oranges", "charcoals", "slates", "reds", "blues")
)

subicb_bph_ratio <- left_join (subicbshape,bph_activity,  by = "sub_icb_location_code")

subicb_oab_ratio <- left_join (subicbshape,oab_activity,  by = "sub_icb_location_code")


tm_shape(subicb_bph_ratio) +
  tm_fill("activityneedratio",title="Activity/Need Ratio BPH",style = "pretty", palette = "YlOrRd") +
  tm_borders("grey25", alpha=.5) 

tm_shape(subicb_oab_ratio) +
  tm_fill("activityneedratio",title="Activity/Need Ratio OAB",style = "pretty", palette = "YlOrRd") +
  tm_borders("grey25", alpha=.5)




bph_activity_icb <- bph_activity_icb |> rename(sub_icb_location_code=sub_icb)

subicb_bph_ratio_icb <- left_join (subicbshape,bph_activity_icb,  by = "sub_icb_location_code")


tm_shape(subicb_bph_ratio_icb) +
  tm_fill("activityneedratio",title="Activity/Need Ratio BPH",style = "pretty", palette = "YlOrRd") +
  tm_borders("grey25", alpha=.5) 




 