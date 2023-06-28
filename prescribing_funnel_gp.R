library(ggrepel)
library(StrategyUnitTheme)

#drug_data_all <- readRDS("bph_drug_data_all.rds")
#OR

#drug_data_all <- readRDS("oab_drug_data_all.rds")
#OR
drug_data_all <- readRDS("ppi_drug_data_all.rds")



ppi_drugs_summary <- drug_data_all |>
  group_by(row_id)|>
  summarise(items_total = sum(items)) |>
  ungroup()


# oab_activity_drugs <- oab_gp |> 
#   left_join(oab_drugs_summary,by = join_by(org_code==row_id)) |>
#   mutate(presc_anr = items_total/oabprevnumtotal)

ppi_activity_drugs <- ppi_gp |> 
  left_join(ppi_drugs_summary,by = join_by(org_code==row_id)) |>
  mutate(presc_anr = items_total/ppiprev)
#replace ppiprev with oabprevnumtotal or bphprevnumtotal

data <-ppi_activity_drugs |>
  rename(activityneedratio = presc_anr)

data <- data |> left_join(sub_icbs_mid) |>
  filter(items_total != "NA")



#ubar <- sum(data$items_total)/sum(data$oabprevnumtotal)
ubar <- sum(data$items_total)/sum(data$ppiprev)

#saveRDS(bph_activity_icb_drugs_mid,"data/bph_activity_icb_drugs_mid.rds")
#write_excel_csv(data,"data_od.csv")

#sub_icb <- midlands_sub_icbs[[i]]

cl_fn <- function(ubar, limit) {
  \(x) ubar + limit * sqrt(ubar / x)
}


plots <-midlands_sub_icbs |>
  set_names() |>
  map(\(sub_icb) {
    ggplot(data, aes(ppiprev, activityneedratio)) +
      geom_jitter(
        data = \(.x) filter(.x, sub_icb_location_code == sub_icb),
        fill = "#f9bf07",
        colour = "black",
        shape="circle filled"
      ) +
      # geom_jitter(
      #   data = \(.x) {
      #     .x |>
      #       filter(sub_icb_location_code != sub_icb)
      #   },
      #   alpha = 0.1,
      #   size = 0.5
      #     ) +
      geom_hline(aes(yintercept=ubar),size=1) +
      geom_text_repel(
        data = \(.x) {
          .x |>
            filter(
              sub_icb_location_code == sub_icb,
              FALSE |
                activityneedratio > cl_fn(ubar, 3.0)(ppiprev) |
                activityneedratio < cl_fn(ubar, -3.0)(ppiprev)
            )
        },
        aes(label = org_code)
      ) +
      geom_function(fun = cl_fn(ubar, +1.96), size = 0.5, linetype=5) +
      geom_function(fun = cl_fn(ubar, -1.96), size = 0.5, linetype=5) +
      geom_function(fun = cl_fn(ubar, +3.00), size = 0.5, linetype="solid") +
      geom_function(fun = cl_fn(ubar, -3.00), size = 0.5, linetype="solid") +
      xlab("Prevalence Number") +
      ylab("Activity to Need Ratio")+
    #  ylim(-0.1, 0.1)+
      labs(title = "Funnel plot for PPI Prescription Items", 
           subtitle = paste0("Sub-ICB: ",sub_icb)) +
      su_theme() 
  })

purrr::walk(plots, plot)
