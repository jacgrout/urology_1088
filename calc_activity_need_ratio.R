library(readr)
library(dplyr)
library(janitor)

#read in gp list size males March 2023
bph_spells <- read_csv("data/BPHSpells.csv") |> 
  clean_names()