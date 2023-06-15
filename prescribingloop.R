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

# testing: these work
#get_drugs_by_gp("0212000AA", "H81068")
#get_drugs_by_gp("0103050P0", "Y02414")

# you can combine practices and it returns per practice
#get_drugs_by_gp("0103050P0", paste("C81015", "C81016", sep = ","))

# but combine drugs returns the aggregation
#get_drugs_by_gp(
#  paste("0604020C0", "0704010U0", sep = ","),
#  paste("C81015", "C81016", sep = ",")
#)


bph_drug_codes <- c(
  "0604020C0",
  "0704010U0",
  "0704010V0",
  "0704010W0",
  "0704010A0"
)

oab_drug_codes <- c(
  "0704020AA",
  "0704020AB",
  "0704020J0",
  "0704040G0",
  "0704020N0",
  "0704020AD",
  "0704020AC",
  "0704020AE",
  "0605020E0"
)





f <- purrr::slowly(
  purrr::safely(get_drugs_by_gp),
  rate = purrr::rate_delay(1)
)

# I'm going to fake this
#mid_gps_codes <- midlands_gps$org_code
#mid_gps_codes <-midlands_gps$org_code[1:2]

drug_data_all <- tibble() 

i=1201
for(i in 1201:1287){
drug_data <- oab_drug_codes |>
  purrr::set_names() |>
  # I suspect your error was forgetting to pass in the GP's to f
  purrr::map(f, mid_gps_codes[i]) |>
  purrr::map("result") |>
  purrr::discard(is.null) |>
  dplyr::bind_rows(.id = "drug")

drug_data_all <- drug_data_all |>
  rbind(drug_data)
i=i+1
}

saveRDS(drug_data_all,"oab_drug_data_all.rds")
