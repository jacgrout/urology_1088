




bph_activity_gap <- bph_activity |> 
  mutate(activity_gap = (max(bph_activity$activityneedratio)*bphprev)-bph_spells,
         gap_percentage_change = (activity_gap/bph_spells)*100)

oab_activity_gap <- oab_activity |> 
  mutate(activity_gap = (max(oab_activity$activityneedratio)*oabprev)-oab_spells,
         gap_percentage_change = (activity_gap/oab_spells)*100)
