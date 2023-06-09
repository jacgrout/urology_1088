
saveRDS(midlandsoutpatients,"midlandsoutpatients.rds")

# get the number of urology outpatient attendances in the five years in the midlands:
sum(midlandsoutpatients$numattends) # 2,304,785

# and by year

midlandsoutpatients |>
  group_by(der_financial_year) |>
  summarise(total = sum(numattends))

#18/19: 
#19/20: 
#20/21: 
#21/22: 
#22/23: 

#new and fup
midlandsoutpatients |>
  group_by(der_appointment_type) |>
  summarise(total = sum(numattends))
# New: 710,879
# FUP: 1,593,898

#midlandsoutpatients <- readRDS("midlandsoutpatients.rds")

table_data <-midlandsoutpatients |>
  filter(der_appointment_type != "N/A") |>
  group_by(der_financial_year,der_appointment_type) |>
  summarise(total = sum(numattends)) |>
  ungroup()

table_data|> 
  pivot_wider(names_from = der_appointment_type, values_from = total, id_cols = der_financial_year) |>
  
  gt(rowname_col = "der_financial_year")|>
  cols_move(FUp, New) |>
  tab_header(
    title = 'Urology Outpatient Attendances - Midlands'
  ) |>
  tab_source_note(source_note = "Data: SUS")

#With a diagnosis code
midlandsoutpatients_NewDiagnosed <- midlandsoutpatients |>
  filter(der_diagnosis_all != "NULL") |>
  filter(der_appointment_type == "New") 

sum(midlandsoutpatients_NewDiagnosed$numattends)

#94,988 = All
#28,982 = NEW and diagnosed




#Had a procedure and a diagnosis
midlandsoutpatients_ProcAndDiagnosed <- midlandsoutpatients |>
  filter(der_diagnosis_all != "NULL") |>
  filter(der_procedure_all != "NULL") |>
  group_by(der_financial_year,der_appointment_type) |>
  summarise(totalProcs = sum(der_number_procedure),
            totalattends = sum(numattends)) |>
  ungroup()

midlandsoutpatients_ProcAndDiagnosed |>
  gt(rowname_col = "der_financial_year",   groupname_col = "der_appointment_type",)|>
  tab_header(
    title = 'Urology Outpatient Diagnosed Attendances and Procedures - Midlands'
  ) |>
  tab_source_note(source_note = "Data: SUS")

#and by sub_icb/icb
midlandsoutpatientsicb <-
  midlandsoutpatients |>
  group_by(icb22nm) |>
  summarise(totalProcs = sum(der_number_procedure),
            totalattends = sum(numattends))|>
  gt()

