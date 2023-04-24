library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)
library(FunnelPlotR)

#read in gp to icb file
gp_to_icb <- read_xlsx("data/gp-reg-pat-prac-map-jul22.xlsx") |>
  clean_names() |>
  rename(org_code = practice_code)

#read in gp list size males March 2023
bph_spells <- read_csv("data/BPHSpellsDistinctPatient.csv") |> 
  clean_names() |>
  rename(org_code = gp_practice_code,
         num_bph_spells = num)

#sum(bph_spells$num) #- 137,820
# 
# bph_spells <- bph_spells |>
#   group_by(org_code) |>
#   summarise(num_bph_spells = sum(num)) |>
#   ungroup()
  
  
  bph_joined <- list_size_gpprac_prev_bph |> 
    left_join (bph_spells) |>
    mutate(num_bph_spells = replace_na(num_bph_spells, 0))
  
  
  bph_activity <- bph_joined |>
    group_by(sub_icb_location_code) |>
    summarise(bphprev = sum(bphprevnumtotal),
               list_size_bph = sum(list_size_bph_total),
              bph_spells = sum(num_bph_spells)) |>
    ungroup()
  
ICBTOBPH <- bph_joined |> left_join(gp_to_icb)
  
  bph_activity <- bph_activity |>
    mutate(activityneedratio = bph_spells/bphprev)

# -----------------------------------------------------------------------
  
  list_size_subicb_prev_bph  <-  list_size_gpprac_prev_bph |>
    group_by(sub_icb_location_code) |>
    summarise(bphprev = sum(bphprevnumtotal),
              list_size_bph = sum(list_size_bph_total)
              ) |>
    ungroup()
  
 # list_size_subicb_prev_bph <- list_size_subicb_prev_bph |> rename(sub_icb=sub_icb_location_code)
  
  ICBTOBPH <- bph_joined |> left_join(gp_to_icb)

  bph_spells_icb <- ICBTOBPH |>
    group_by(sub_icb_location_code) |>
    summarise(num_bph_spells_sum = sum(num_bph_spells)) |>
    ungroup()
  
  bph_joined_icb <- list_size_subicb_prev_bph |> left_join (bph_spells_icb)
  

  
  bph_activity_icb <- bph_joined_icb |>
    mutate(activityneedratio = num_bph_spells_sum/bphprev)
  
 # subicb_to_icb_lookup_2 <- subicb_to_icb_lookup |> rename(sub_icb=sub_icb_location_code)
  bph_activity_icb <- bph_activity_icb |> left_join (subicb_to_icb_lookup)
  
  bph_plot <- ggplot(bph_activity_icb,aes(x=bphprev,y=activityneedratio,size=activityneedratio, colour=sub_icb_location_code)) +
    geom_point()
  
  bph_plot
  
  bph_activity_icb2 <- bph_activity_icb |> filter(region_name == "MIDLANDS COMMISSIONING REGION") 
  bph_plot <- ggplot(bph_activity_icb2,aes(x=bphprev,y=num_bph_spells,size=activityneedratio, colour=integrated_care_board_name)) +
  geom_point()
  bph_plot
  
  #------------------------------------------------------------------------
  
  View(bph_joined)
  View(list_size_age_group_compare_23all)

  
  changed_list_size <- bph_joined |> left_join(list_size_age_group_compare_23all)
    
  changed_list_size2 <- changed_list_size |> 
  #  filter(roundedpercent != "NA") |>
    filter(roundedpercent<=20)
  
  bph_activity_gp <- changed_list_size2 |>
    mutate(activityneedratio = num_bph_spells/bphprevnumtotal)
    
  
  
  bph_plot <- ggplot(bph_activity_gp,aes(x=bphprevnumtotal,y=activityneedratio,size=activityneedratio)) +
    geom_point()
  
  bph_plot
  
  bph_plot <- bph_activity_gp |>
    filter(sub_icb_location_code =="01Y") |>
    ggplot(aes(x=org_code,y=activityneedratio))+
    geom_col()
  
  bph_plot <- ggplot(bph_activity_gp,aes(x=org_code,y=activityneedratio)) +
    geom_col()
  
  bph_plot <- bph_activity_gp |>
    filter(sub_icb_location_code =="01Y") |>
    mutate(mean_anr = mean(activityneedratio)) |>
    ggplot(aes(x=list_size_bph_total,y=activityneedratio,size=3, colour="red"))+
    geom_point() +
    geom_hline(aes(yintercept=mean_anr))

    
  bph_plot

  