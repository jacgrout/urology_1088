library(httr)
library(jsonlite)
library(tidyverse)
library(forcats)

#Example of code to get one drug from openprescribing.net
#res = GET("https://openprescribing.net/api/1.0/spending_by_sicbl/?code=0704020J0&format=json")
#rawToChar(res$content)
#data = fromJSON(rawToChar(res$content))

# Source: OpenPrescribing.net, Bennett Institute for Applied Data Science, University of Oxford, 2023

#source("R/functions.R")


#Add the codes of the drugs wanted
#Incontinence
drug_codes= c("0704020AC","0704020AA","0704020AD","0704020G0","0704020AE","0704020J0","0704020P0",
              "0704020AB","0704020AF","0704020N0","0704020Z0")

#Retention
drug_codes = c("0704010A0","0704010C0","0704010I0","0704010M0","0704010W0","0704010V0","0704010U0","0704010T0")

#BPH
drug_codes = c("0604020C0","0704010U0","0704010V0","0704010W0","0704010A0")
# 0604020C0 and 0704010U0 -  most used

#OAB
drug_codes = c("0704020AA","0704020AB","0704020J0","0704040G0","0704020N0","0704020AD","0704020AC","0704020AE","0605020E0")
# 0704020AB and 0704020AE and 0704020J0 and 0704020N0 -  most used

#data_test <-get_drugs(drug_code)

#call the function for each drug in the list but with a slight delay to avoid issues
f <- purrr::slowly(
  purrr::safely(get_drugs),
  rate = purrr::rate_delay(0.1)
)

#format the results from the function call
drug_data <-drug_codes |>
  set_names() |>
  purrr::map(f) |> #run up to here to check for errors, maybe remove the pipe
  map("result") |>
  discard(is.null) |>
  enframe("id","results")

#first_drug <-drug_data$results[[1]]

#create an empty tibble to put all the results in
all_drugs <- tibble()

#loop over and bind together all the data retrieved for each drug in the list
for(i in 1:nrow(drug_data)){
  df <- drug_data$results[[i]]
  all_drugs <- bind_rows(all_drugs,df)
}


#all_drugs <- all_drugs |> mutate(avcost = actual_cost/items)

all_drugs_summary <- all_drugs |>
  group_by(row_id,drug)|>
  summarise(items_total = sum(items)) |>
  ungroup()

bph_drugs_summary <- all_drugs |>
  group_by(row_id)|>
  summarise(items_total = sum(items)) |>
  ungroup()

bph_activity_icb_drugs <-bph_activity_icb |> 
  left_join(bph_drugs_summary,by = join_by(sub_icb_location_code==row_id)) |>
  mutate(presc_anr = items_total/bphprev)


  
  bph_plot <- bph_activity_icb_drugs |>
    filter(sub_icb_location_code %in% midlands_sub_icbs) |>
    mutate(mean_anr = mean(presc_anr),
           median_anr = median(presc_anr),
           ubar = sum(items_total)/sum(bphprev)) |>
    ggplot(aes(x=bphprev,y=presc_anr))+
    geom_point(size=3,colour = "#f9bf07") +
    geom_point(size=3,colour = "black",shape=21) +
    geom_hline(aes(yintercept=ubar),size=1) +
    su_theme()
  
  bph_plot




#incontinence_drugs <-all_drugs
#retention_drugs <-all_drugs

oab_drugs <- all_drugs |> 
  mutate(year=substr(date,1,4),
         finyear=case_when(date<='2019-03-31' ~ '2018/19',
                           date>='2019-04-01' & date <= '2020-03-31' ~ '2019/20', 
                           date>='2020-04-01' & date <= '2021-03-31' ~ '2020/21', 
                           date>='2021-04-01' & date <= '2022-03-31' ~ '2021/22',
                           date>='2022-04-01' & date <= '2023-03-31' ~ '2022/23',
                           TRUE ~ 'xx/xx')
)

bphplot <- all_drugs_summary |> filter(row_id %in% midlands_sub_icbs) |>
ggplot(aes(x=row_id, y=items_total,fill=drug)) + 
  geom_col() +
  ggtitle("Prescription Items for OAB Medications (Last 5 years)")+
  xlab("Sub-ICB")+
  ylab("Total Prescribed Items")+
  scale_fill_su(palette="reds") +
  su_theme()


bph_plot <- ggplot(bph_drugs,aes(x=finyear,y=items,size=items, colour=drug)) 
bph_plot + geom_point() + facet_grid(drug~.)

oab_plot <- ggplot(oab_drugs,aes(x=finyear,y=items,size=items, colour=drug)) 
oab_plot + geom_point() + facet_grid(drug~.)


