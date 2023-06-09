library(janitor)
library(readr)
library(dplyr)

load(file = "data/projectimage.RData")

#Outpatients Data for 5 years 01/04/2023 to 31/03/2023

urologyoutpatients <- read_csv("data/UrologyOutpatients1819.csv") |>
  clean_names()

urologyoutpatients2 <- read_csv("data/UrologyOutpatients1920.csv") |>
  clean_names()

urologyoutpatients3 <- read_csv("data/UrologyOutpatients2021.csv") |>
  clean_names()

urologyoutpatients4 <- read_csv("data/UrologyOutpatients2122.csv") |>
  clean_names()

urologyoutpatients5 <- read_csv("data/UrologyOutpatients2223.csv") |>
  clean_names()

urologyoutpatientsall <- urologyoutpatients |> 
  rbind(urologyoutpatients2)|>
  rbind(urologyoutpatients3)|>
  rbind(urologyoutpatients4)|>
  rbind(urologyoutpatients5)



midlandsoutpatients <- urologyoutpatientsall |> 
  left_join(midlands_gps, by = join_by(gp_practice_code==org_code)) |>
  filter(reg22nm == "Midlands")
