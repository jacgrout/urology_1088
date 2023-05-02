
#library(ggplot2)

midlands_gps <- ICBTOBPH |> filter(reg22nm == "Midlands") 

midlands_sub_icbs <- unique(midlands_gps$sub_icb_location_code)

midlands_icbs <- unique(midlands_gps$icb22)

i=10

for(i in 1:19) {
  
  bph_plot <- bph_activity_gp |>
    filter(sub_icb_location_code ==midlands_sub_icbs[i]) |>
    mutate(mean_anr = mean(activityneedratio),
           median_anr = median(activityneedratio),
           ubar = sum(num_bph_spells)/sum(bphprevnumtotal)) |>
    ggplot(aes(x=bphprevnumtotal,y=activityneedratio))+
    geom_point(size=3,colour = "#f9bf07") +
    geom_point(size=3,colour = "black",shape=21) +
    geom_hline(aes(yintercept=ubar),size=1) +
    su_theme()
  
  bph_plot
  
  bph_activity_gp_sub_icb <- bph_activity_gp |>
    filter(sub_icb_location_code ==midlands_sub_icbs[i]) |>
    mutate(ubar = sum(num_bph_spells)/sum(bphprevnumtotal))
  
  bph_activity_midlands <- ICBTOBPH |>
    filter(reg22nm =="Midlands") |>
    mutate(ubar = sum(num_bph_spells)/sum(bphprevnumtotal))
  
  # bph_activity_gp_sub_icb <- bph_activity_midlands
  
  # NEED TO CALCULATE LIMITS FOR THE WHOLE OF THE MIDLANDS - NEED TO CHECK ABOUT EXCLUDED PRACTICES AND HOW TO HANDLE
  
  # Generate the limits for the plot
  lkup<-data.frame(id=seq(round(min(bph_activity_midlands$bphprevnumtotal)), max(bph_activity_midlands$bphprevnumtotal), 1))
  
  #lkup<-data.frame(id=seq(round(min(bph_activity_gp_sub_icb$bphprevnumtotal)), max(bph_activity_gp_sub_icb$bphprevnumtotal), 1))
  #lkup$OneSigma <- sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))  
  #lkup$TwoSigma <- 1.96*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))) # 95%
  #lkup$LowerTwoSigma <- bph_activity_gp_sub_icb$mean_anr - (1.96*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))))
  #lkup$UpperTwoSigma <- bph_activity_gp_sub_icb$mean_anr + (1.96*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))))
  #lkup$ThreeSigma <- 3*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))) # 99.8%
  #lkup$LowerThreeSigma <- bph_activity_gp_sub_icb$mean_anr - (3*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))))
  #lkup$UpperThreeSigma <- bph_activity_gp_sub_icb$mean_anr + (3*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))))
  lkup$LowerCI95_u <- bph_activity_gp_sub_icb$ubar - (1.96*sqrt(bph_activity_gp_sub_icb$ubar/lkup$id))
  lkup$UpperCI95_u <- bph_activity_gp_sub_icb$ubar + (1.96*sqrt(bph_activity_gp_sub_icb$ubar/lkup$id))
  lkup$LowerCI998_u <- bph_activity_gp_sub_icb$ubar - (3*sqrt(bph_activity_gp_sub_icb$ubar/lkup$id))
  lkup$UpperCI998_u <- bph_activity_gp_sub_icb$ubar + (3*sqrt(bph_activity_gp_sub_icb$ubar/lkup$id))
  
  lkup<-gather(lkup, key, value,-id) 
  #|>
  #  mutate(key_label = case_when(key == "LowerThreeSigma" ~ "99.8% Lower CI",
           #                      key == "LowerTwoSigma" ~ "95% Lower CI",
            #                     key == "UpperThreeSigma" ~ "99.8% Upper CI",
             #                    key == "UpperTwoSigma" ~ "95% Upper CI")
  # )
  
  lkup_inner <- lkup |> filter(key == "LowerCI95_u" | key == "UpperCI95_u")
  lkup_outer <- lkup |> filter(key == "LowerCI998_u" | key == "UpperCI998_u") 

  plot <- bph_plot+ 
 geom_line(aes(x=id, y=value, group=key), colour="black",  data=lkup_inner, size = 0.5, linetype=5) +
 #geom_line(aes(x=id, y=value, col=key), data=lkup_outer, size = 1, linetype="solid") +
    geom_line(aes(x=id, y=value, group=key), colour="black", data=lkup_outer, size = 0.5, linetype="solid") +
      xlab("Prevalence Number") +
    ylab("Activity to Need Ratio")+
    ylim(-0.25, 0.25)+
    labs(title = "Funnel plot for Benign Prostatic Hyperplasia",
         subtitle = paste0("Sub-ICB: ",bph_activity_gp_sub_icb$sub_icb_location_code))


  i=i+1
  
  #p <- ggplotly(plot)
  
  print(plot)
}