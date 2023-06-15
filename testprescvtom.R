get_drugs_by_gp <- function(id, gp) {
  req <- httr::GET(
    "https://openprescribing.net/",
    path = c("api", "1.0", "spending_by_practice"),
    query = list(
      code = id,
      org = gp,
      format = "json"
    )
  )
  

  stopifnot(httr::status_code(req) == 200)
  
  req |>
    httr::content() |>
    dplyr::bind_rows()
}


#BPH

bph_drug_codes <- c(
  "0604020C0",
  "0704010U0",
  "0704010V0",
  "0704010W0",
  "0704010A0"
)
# 0604020C0 and 0704010U0 -  most used


f <- purrr::slowly(
  purrr::safely(get_drugs_by_gp),
  rate = purrr::rate_delay(0.1)
)


mid_gps_codes <-midlands_gps$org_code

# I'm going to fake this
mid_gps_codes <- paste(
  c("H81068", "Y02414"), # replace with midlands_gps$org_code
  collapse = ","
)

drug_data <- bph_drug_codes |>
  purrr::set_names() |>
  # I suspect your error was forgetting to pass in the GP's to f
  purrr::map(f, mid_gps_codes) |>
  purrr::map("result") |>
  purrr::discard(is.null) |>
  #dplyr::bind_rows(.id = "drug")
enframe("id","results")


#create an empty tibble to put all the results in
all_drugs <- tibble()

#loop over and bind together all the data retrieved for each drug in the list
for(i in 1:nrow(drug_data)){
  df <- drug_data$results[[i]]
  all_drugs <- bind_rows(all_drugs,df)
}
