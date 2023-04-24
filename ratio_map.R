
# ------

subicb_bph_ratio <- left_join (subicbshape,bph_activity,  by = "sub_icb_location_code")


tm_shape(subicb_bph_ratio) +
  tm_fill("activityneedratio",title="Activity/Need Ratio BPH",style = "pretty", palette = "YlOrRd") +
  tm_borders("grey25", alpha=.5) 


bph_activity_icb <- bph_activity_icb |> rename(sub_icb_location_code=sub_icb)

subicb_bph_ratio_icb <- left_join (subicbshape,bph_activity_icb,  by = "sub_icb_location_code")


tm_shape(subicb_bph_ratio_icb) +
  tm_fill("activityneedratio",title="Activity/Need Ratio BPH",style = "pretty", palette = "YlOrRd") +
  tm_borders("grey25", alpha=.5) 




 