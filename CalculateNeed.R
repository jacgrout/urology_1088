library(readr)
library(dplyr)
library(janitor)

#read in gp list size males March 2023
gp_reg_pat_prac_sing_age_male <- read_csv("data/gp-reg-pat-prac-sing-age-male.csv") |> 
  clean_names()
gp_reg_pat_prac_sing_age_female <- read_csv("data/gp-reg-pat-prac-sing-age-female.csv") |> 
  clean_names()



list_size_age_male <- select(gp_reg_pat_prac_sing_age_male, org_code,age,sex,number_of_patients,sub_icb_location_code) |>
  filter(age != "ALL") |>
  mutate(agenum = case_when(age=="95+" ~ "95",
         TRUE ~ age)) |>
  mutate(age_group_bph = case_when(as.numeric(agenum)<40 ~ '<40',
                                   as.numeric(agenum)>39 & as.numeric(agenum)<50 ~ '40-49',
                                   as.numeric(agenum)>49 & as.numeric(agenum)<60 ~ '50-59',
                                   as.numeric(agenum)>59 & as.numeric(agenum)<70 ~ '60-69',
                                   as.numeric(agenum)>69 & as.numeric(agenum)<80 ~ '70-79',
                                   as.numeric(agenum)>79 ~'80+')
         ) |>
  
  mutate(age_group_oab = case_when(as.numeric(agenum)<20 ~ '<20',
                                   as.numeric(agenum)>19 & as.numeric(agenum)<30 ~ '20-29',
                                   as.numeric(agenum)>29 & as.numeric(agenum)<40 ~ '30-39',
                                   as.numeric(agenum)>39 & as.numeric(agenum)<50 ~ '40-49',
                                   as.numeric(agenum)>49 & as.numeric(agenum)<60 ~ '50-59',
                                   as.numeric(agenum)>59 & as.numeric(agenum)<70 ~ '60-69',
                                   as.numeric(agenum)>69 ~'70+')
  ) 


list_size_age_female <- select(gp_reg_pat_prac_sing_age_female, org_code,age,sex,number_of_patients,sub_icb_location_code) |>
  filter(age != "ALL") |>
  mutate(agenum = case_when(age=="95+" ~ "95",
                            TRUE ~ age)) |>
  mutate(age_group_bph = case_when(as.numeric(agenum)<40 ~ '<40',
                                   as.numeric(agenum)>39 & as.numeric(agenum)<50 ~ '40-49',
                                   as.numeric(agenum)>49 & as.numeric(agenum)<60 ~ '50-59',
                                   as.numeric(agenum)>59 & as.numeric(agenum)<70 ~ '60-69',
                                   as.numeric(agenum)>69 & as.numeric(agenum)<80 ~ '70-79',
                                   as.numeric(agenum)>79 ~'80+')
  ) |>
  
  mutate(age_group_oab = case_when(as.numeric(agenum)<20 ~ '<20',
                                   as.numeric(agenum)>19 & as.numeric(agenum)<30 ~ '20-29',
                                   as.numeric(agenum)>29 & as.numeric(agenum)<40 ~ '30-39',
                                   as.numeric(agenum)>39 & as.numeric(agenum)<50 ~ '40-49',
                                   as.numeric(agenum)>49 & as.numeric(agenum)<60 ~ '50-59',
                                   as.numeric(agenum)>59 & as.numeric(agenum)<70 ~ '60-69',
                                   as.numeric(agenum)>69 ~'70+')
  ) 
  

list_size_age_group_male_bph <- list_size_age_male |>
  group_by(org_code,sub_icb_location_code,age_group_bph) |>
  summarise(list_size_bph = sum(number_of_patients)) |>
  ungroup()

list_size_age_group_female_bph <- list_size_age_female |>
  group_by(org_code,sub_icb_location_code,age_group_bph) |>
  summarise(list_size_bph = sum(number_of_patients)) |>
  ungroup()

list_size_age_group_male_oab <- list_size_age_male |>
  group_by(org_code,sub_icb_location_code,age_group_oab) |>
  summarise(list_size_oab = sum(number_of_patients)) |>
  ungroup()

list_size_age_group_female_oab <- list_size_age_female |>
  group_by(org_code,sub_icb_location_code,age_group_oab) |>
  summarise(list_size_oab = sum(number_of_patients)) |>
  ungroup()

#create a dataframe with BPH and OAB prevalence in it
#bph
age_group_bph <- c('<40','40-49','50-59','60-69','70-79','80+')
prevpercent_male <- as.numeric(c('0.0','14.8','20.0','29.1','36.8','38.4'))
prevpercent_female <- as.numeric(c('0.0','0.0','0.0','0.0','0.0','0.0'))
prev_bph <- data.frame(age_group_bph,prevpercent_male,prevpercent_female)
#oab
age_group_oab <- c('<20','20-29','30-39','40-49','50-59','60-69','70+')
prevpercent_male <- as.numeric(c('0','5.3','5.7','6.1','14.5','18.3','16.0'))
prevpercent_female <- as.numeric(c('0','15.2','11','15.3','18.4','20.6','21.1'))
prev_oab <- data.frame(age_group_oab,prevpercent_male,prevpercent_female)
#ppi
age_group_ppi <- c('<20','20-29','30-39','40-49','50-59','60-69','70+')
prevpercent_male <- as.numeric(c('0','5.3','5.7','6.1','14.5','18.3','16.0'))
prevpercent_female <- as.numeric(c('0','0','0','0','0','0','0'))
prev_ppi <- data.frame(age_group_oab,prevpercent_male,prevpercent_female)

#join bph prev and calculate prev males
list_size_age_group_prev_bph_male <- list_size_age_group_male_bph |>
  left_join(prev_bph) |>
  mutate(bphprevnum = (prevpercent_male/100)*list_size_bph)

#join bph prev and calculate prev females
list_size_age_group_prev_bph_female <- list_size_age_group_female_bph |>
  left_join(prev_bph) |>
  mutate(bphprevnum = (prevpercent_female/100)*list_size_bph)

#join oab prev and calculate prev male
list_size_age_group_prev_oab_male <- list_size_age_group_male_oab |>
  left_join(prev_oab) |>
  mutate(oabprevnum = (prevpercent_male/100)*list_size_oab)
#join oab prev and calculate prev female
list_size_age_group_prev_oab_female <- list_size_age_group_female_oab |>
  left_join(prev_oab) |>
  mutate(oabprevnum = (prevpercent_female/100)*list_size_oab)



#combine male and female
list_size_age_group_prev_bph <- rbind(list_size_age_group_prev_bph_male,list_size_age_group_prev_bph_female)

list_size_gpprac_prev_bph <- list_size_age_group_prev_bph |> 
  group_by(org_code,sub_icb_location_code)|>
  summarise(bphprevnumtotal = sum(bphprevnum),
            list_size_bph_total = sum(list_size_bph)) |>
  ungroup()

list_size_age_group_prev_oab <- rbind(list_size_age_group_prev_oab_male,list_size_age_group_prev_oab_female)

list_size_gpprac_prev_oab <- list_size_age_group_prev_oab |> 
  group_by(org_code,sub_icb_location_code)|>
  summarise(oabprevnumtotal = sum(oabprevnum),
            list_size_oab_total = sum(list_size_oab)) |>
  ungroup()

list_size_subicb_prev_bph <- list_size_age_group_prev_bph |>
  group_by(sub_icb_location_code) |>
  summarise(bphprevnumtotal = sum(bphprevnum),
            list_size_bph_total = sum(list_size_bph)) |>
  ungroup() |>
  mutate(overall_prev = (bphprevnumtotal/list_size_bph_total)*100)

list_size_subicb_prev_oab <- list_size_age_group_prev_oab |>
  group_by(sub_icb_location_code) |>
  summarise(oabprevnumtotal = sum(oabprevnum),
            list_size_oab_total = sum(list_size_oab)) |>
  ungroup() |>
  mutate(overall_prev = (oabprevnumtotal/list_size_oab_total)*100)

# Need to work out PPI prevalence as this will use activity as the denominator rather than population:

#treatment to need ratios:
#compare levels of condition specific treatment activity to the prevalence estimates
#Where possible the analysis will consider both hospital surgical treatments (including inpatient, 
#day case and outpatient treatments) using surgical procedure data as well as primary care management
#using condition specific prescribing data. We will identify all relevant procedure codes and medications 
#for inclusion and confirm these with the project’s clinical contacts.
