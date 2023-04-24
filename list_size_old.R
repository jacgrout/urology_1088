library(readr)
library(dplyr)
library(janitor)
library(ggplot2)

#read in gp list size males March 2018
gp_reg_pat_prac_sing_age_male_mar_18 <- read_csv("data/gp-reg-pat-prac-sing-age-male-mar-18.csv") |> 
  clean_names()
gp_reg_pat_prac_sing_age_female_mar_18 <- read_csv("data/gp-reg-pat-prac-sing-age-female-mar-18.csv") |> 
  clean_names()


list_size_age_group_male_mar_18 <- select(gp_reg_pat_prac_sing_age_male_mar_18, org_code,age,sex,number_of_patients,ccg_code) |>
  filter(age != "ALL") |>
  select(-age) |>
  group_by(org_code,ccg_code) |>
  summarise(list_size = sum(number_of_patients)) |>
  ungroup()


list_size_age_group_female_mar_18 <- select(gp_reg_pat_prac_sing_age_female_mar_18, org_code,age,sex,number_of_patients,ccg_code) |>
  filter(age != "ALL") |>
  select(-age) |>
  group_by(org_code,ccg_code) |>
  summarise(list_size = sum(number_of_patients)) |>
  ungroup()

list_size_age_group_compare_mar_18 <- list_size_age_group_male_mar_18 |> rbind(list_size_age_group_female_mar_18)
list_size_age_group_compare_mar_18 <- select(list_size_age_group_compare_mar_18,-ccg_code) |>
  group_by(org_code) |>
  summarise(list_size_all = sum(list_size)) |>
  ungroup()


list_size_age_group_male_compare <- list_size_age_group_male_bph |>
  group_by(org_code) |>
  summarise(list_size = sum(list_size_bph)) |>
  ungroup()
list_size_age_group_female_compare <- list_size_age_group_female_bph |>
  group_by(org_code) |>
  summarise(list_size = sum(list_size_bph)) |>
  ungroup()
list_size_age_group_compare <- list_size_age_group_male_compare |> rbind(list_size_age_group_female_compare)
list_size_age_group_compare <- list_size_age_group_compare |>
  group_by(org_code) |>
  summarise(list_size_all = sum(list_size)) |>
  ungroup()

list_size_age_group_compare_23 <- left_join(list_size_age_group_compare,list_size_age_group_compare_mar_18, by = "org_code") |>
  mutate(listsizechange = list_size_all.x-list_size_all.y,#2018-2023
         percentlistsizechange = ((list_size_all.x-list_size_all.y)/list_size_all.y)*100)



list_size_age_group_compare_23all <- list_size_age_group_compare_23 |>
  filter(percentlistsizechange != "NA") |>
  mutate(roundedpercent = case_when (percentlistsizechange <0 ~ round(percentlistsizechange*-1),
                          TRUE ~  round(percentlistsizechange)),
         group = case_when(roundedpercent<10 ~ '<10%',
                           roundedpercent>=10 & roundedpercent <20 ~ '10-20%',
                           roundedpercent>=20 & roundedpercent <30 ~ '20-30%',
                           roundedpercent>=30 & roundedpercent <40 ~ '30-40%',
                           roundedpercent>=40 & roundedpercent <50 ~ '40-50%',
                           roundedpercent>=50 & roundedpercent <60 ~ '50-60%',
                           roundedpercent>=60 & roundedpercent <70 ~ '60-70%',
                           roundedpercent>=70 & roundedpercent <80 ~ '70-80%',
                           roundedpercent>=80 & roundedpercent <90 ~ '80-90%',
                           roundedpercent>=90 ~ '90%+',
                           TRUE ~ 'Others'
         )) 

list_size_age_group_compare_23all |>
  ggplot(aes(x=group))+
  geom_bar()

  mean(list_size_age_group_compare_23all$roundedpercent)
  median(list_size_age_group_compare_23all$roundedpercent)

  