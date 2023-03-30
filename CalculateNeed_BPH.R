library(readr)
library(dplyr)

#read in gp list size males March 2023
gp_reg_pat_prac_sing_age_male <- read_csv("data/gp-reg-pat-prac-sing-age-male.csv")

list_size_age <- select(gp_reg_pat_prac_sing_age_male, ORG_CODE,AGE,NUMBER_OF_PATIENTS,SUB_ICB_LOCATION_CODE) |>
  filter(AGE != "ALL") |>
  mutate(agenum = case_when(AGE=="95+" ~ "95",
         TRUE ~ AGE)) |>
  mutate(age_group_bph = case_when(as.numeric(agenum)<40 ~ '<40',
                               as.numeric(agenum)>39 & as.numeric(agenum)<50 ~ '40-49',
                               as.numeric(agenum)>49 & as.numeric(agenum)<60 ~ '50-59',
                               as.numeric(agenum)>59 & as.numeric(agenum)<70 ~ '60-69',
                               as.numeric(agenum)>69 & as.numeric(agenum)<80 ~ '70-79',
                               as.numeric(agenum)>79 ~'80+')
  )
  

list_size_age_group <- list_size_age |>
  group_by(ORG_CODE,SUB_ICB_LOCATION_CODE,age_group) |>
  summarise(list_size = sum(NUMBER_OF_PATIENTS))

#create a dataframe with BPH and OAB prevalence in it
#bph
age_group_bph <- c('<40','40-49','50-59','60-69','70-79','80+')
prevpercent <- as.numeric(c('0','14.8','20','29.1','36.8','38.4'))
prev_bph <- data.frame(age_group_bph,prevpercent)
#oab
age_group_oab <- c('<20','20-29','30-39','40-49','50-59','60-69','70+')
prevpercent_male <- as.numeric(c('0','5.3','5.7','6.1','14.5','18.3','16'))
prevpercent_female <- as.numeric(c('0','15.2','11','15.3','18.4','20.6','21.1'))
prev_oab <- data.frame(age_group_oab,prevpercent_male,prevpercent_female)

#join bph prev and calculate prev
list_size_age_group_prev <- list_size_age_group |>
  left_join(prev_bph) |>
  mutate(bphprevnum = (prevpercent/100)*list_size)

