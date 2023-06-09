bph_drug_codes = c("0604020C0","0704010U0","0704010V0","0704010W0","0704010A0")

f <- purrr::slowly(
  purrr::safely(get_drugs_by_gp),
  rate = purrr::rate_delay(0.1)
)



drug_codes <- bph_drug_codes

mid_gps_codes <-midlands_gps$org_code


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

