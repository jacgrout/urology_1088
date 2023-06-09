library(ggrepel)

data <-select(bph_activity_icb_drugs_mid, -activityneedratio) |>
  rename(activityneedratio = presc_anr)

ubar <- sum(data$items_total)/sum(data$bphprev)


#sub_icb <- midlands_sub_icbs[[i]]

cl_fn <- function(ubar, limit) {
  \(x) ubar + limit * sqrt(ubar / x)
}


plots <-midlands_sub_icbs |>
  set_names() |>
  map(\(sub_icb) {
    ggplot(data, aes(bphprev, activityneedratio)) +
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
                activityneedratio > cl_fn(ubar, 3.0)(bphprev) |
                activityneedratio < cl_fn(ubar, -3.0)(bphprev)
            )
        },
        aes(label = sub_icb_location_code)
      ) +
      geom_function(fun = cl_fn(ubar, +1.96), size = 0.5, linetype=5) +
      geom_function(fun = cl_fn(ubar, -1.96), size = 0.5, linetype=5) +
      geom_function(fun = cl_fn(ubar, +3.00), size = 0.5, linetype="solid") +
      geom_function(fun = cl_fn(ubar, -3.00), size = 0.5, linetype="solid") +
      xlab("Prevalence Number") +
      ylab("Activity to Need Ratio")+
      ylim(-0.1, 0.1)+
      labs(title = "Funnel plot for Benign Prostatic Hyperplasia Prescription Items", 
           subtitle = paste0("Sub-ICB: ",sub_icb)) +
      su_theme() 
  })

purrr::walk(plots, plot)
