library(readr)
library(dplyr)
library(janitor)

#read in gp list size males March 2023
bph_spells <- read_csv("data/BPHSpells.csv") |> 
  clean_names() |>
  rename(org_code = gp_prac)

#sum(bph_spells$num) #- 144,765

bph_spells <- bph_spells |>
  group_by(org_code) |>
  summarise(num_bph_spells = sum(num)) |>
  ungroup()
  
  
  bph_joined <- list_size_gpprac_prev_bph |> left_join (bph_spells)
  
  bph_activity <- bph_joined |>
    group_by(sub_icb_location_code) |>
    summarise(bphprev = sum(bphprevnumtotal),
               list_size_bph = sum(list_size_bph_total),
              bph_spells = sum(num_bph_spells)) |>
    ungroup()
  
  bph_activity <- bph_activity |>
    mutate(activityneedratio = bph_spells/bphprev)

# -----------------------------------------------------------------------
  
  list_size_subicb_prev_bph  <-  list_size_gpprac_prev_bph |>
    group_by(sub_icb_location_code) |>
    summarise(bphprev = sum(bphprevnumtotal),
              list_size_bph = sum(list_size_bph_total)
              ) |>
    ungroup()
  
  list_size_subicb_prev_bph <- list_size_subicb_prev_bph |> rename(sub_icb=sub_icb_location_code)
  
  bph_spells_icb <- bph_spells |>
    group_by(sub_icb) |>
    summarise(num_bph_spells = sum(num)) |>
    ungroup()
  
  bph_joined_icb <- list_size_subicb_prev_bph |> left_join (bph_spells_icb)
  

  
  bph_activity_icb <- bph_joined_icb |>
    mutate(activityneedratio = num_bph_spells/bphprev)
  