data <-ICBTOPPI |>
  filter(reg22nm =="Midlands") |>
  mutate(activityneedratio = num_ppi_spells / numtotal)
         
ubar <- sum(data$num_ppi_spells)/sum(data$numtotal)


#sub_icb <- midlands_sub_icbs[[i]]

cl_fn <- function(ubar, limit) {
  \(x) ubar + limit * sqrt(ubar / x)
}


plots <-midlands_sub_icbs |>
  set_names() |>
  map(\(sub_icb) {
    ggplot(data, aes(numtotal, activityneedratio)) +
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
                activityneedratio > cl_fn(ubar, 3.0)(numtotal) |
                activityneedratio < cl_fn(ubar, -3.0)(numtotal)
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
      labs(title = "Funnel plot for Post Prostatectomy Incontinence", 
           subtitle = paste0("Sub-ICB: ",sub_icb)) +
      su_theme() 
  })

plots

