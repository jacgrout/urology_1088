

generate_funnel <- function(data,sub_icb,region,icb_list,condition) {
  
# data = bph_activity_gp
# sub_icb= midlands_sub_icbs[i]
# region = "Midlands"
# icb_list= ICBTOBPH
# condition = "BPH"
  
  
#  sum(bph_activity_gp$num_bph_spells) / sum(bph_activity_gp$bphprevnumtotal)
  
  plot <- data |>
    filter(sub_icb_location_code == sub_icb) |>
    mutate(mean_anr = mean(activityneedratio),
           median_anr = median(activityneedratio),
           ubar = sum(num_spells)/sum(prevnumtotal)) |>
    ggplot(aes(x=prevnumtotal,y=activityneedratio))+
    geom_point(size=2) +
    geom_hline(aes(yintercept=ubar))

  
  plot
  
  data_sub_icb <- data |>
    filter(sub_icb_location_code ==sub_icb) |>
    mutate(ubar = sum(num_spells)/sum(prevnumtotal))
  
  activity_midlands <- icb_list |>
    filter(reg22nm == region) |>
    mutate(ubar = sum(num_spells)/sum(prevnumtotal))
  
  
  # Generate the limits for the plot
  lkup<-data.frame(id=seq(round(min(data_sub_icb$prevnumtotal)), max(data_sub_icb$prevnumtotal), 1))
  #lkup$OneSigma_p <- sqrt(data_sub_icb$mean_anr * ((1-data_sub_icb$mean_anr)/lkup$id))  
  #lkup$TwoSigma_p <- 1.96*(sqrt(data_sub_icb$mean_anr * ((1-data_sub_icb$mean_anr)/lkup$id))) # 95%
 # lkup$LowerTwoSigma_p <- data_sub_icb$mean_anr - (1.96*(sqrt(data_sub_icb$mean_anr * ((1-data_sub_icb$mean_anr)/lkup$id))))
  #lkup$UpperTwoSigma_p <- data_sub_icb$mean_anr + (1.96*(sqrt(data_sub_icb$mean_anr * ((1-data_sub_icb$mean_anr)/lkup$id))))
  #lkup$ThreeSigma_p <- 3*(sqrt(data_sub_icb$mean_anr * ((1-data_sub_icb$mean_anr)/lkup$id))) # 99.8%
 # lkup$LowerThreeSigma_p <- data_sub_icb$mean_anr - (3*(sqrt(data_sub_icb$mean_anr * ((1-data_sub_icb$mean_anr)/lkup$id))))
  #lkup$UpperThreeSigma_p <- data_sub_icb$mean_anr + (3*(sqrt(data_sub_icb$mean_anr * ((1-data_sub_icb$mean_anr)/lkup$id))))
 
  lkup$LowerCI95_u <- data_sub_icb$ubar - (1.96*sqrt(data_sub_icb$ubar/lkup$id))
  lkup$UpperCI95_u <- data_sub_icb$ubar + (1.96*sqrt(data_sub_icb$ubar/lkup$id))
  lkup$LowerCI998_u <- data_sub_icb$ubar - (3*sqrt(data_sub_icb$ubar/lkup$id))
  lkup$UpperCI998_u <- data_sub_icb$ubar + (3*sqrt(data_sub_icb$ubar/lkup$id))
  
  lkup<-gather(lkup, key, value,-id) 
  
  plot <- plot+ geom_line(aes(x=id, y=value, col=key), data=lkup, size = 1) + 
    xlab("Prevalence Number") +
    ylab("Activity to Need Ratio")+
    labs(title = paste0("Funnel plot for ",condition," data"),
         subtitle = paste0("Sub-ICB: ",data_sub_icb$sub_icb_location_code))

    return(plot)
}
