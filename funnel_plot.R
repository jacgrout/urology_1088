
#library(ggplot2)

midlands_icbs <- unique(bph_activity_midlands$sub_icb_location_code)

i=1 

for(i in 1:19) {

 bph_plot <- bph_activity_gp |>
   filter(sub_icb_location_code ==midlands_icbs[i]) |>
   mutate(mean_anr = mean(activityneedratio),
          median_anr = median(activityneedratio)) |>
   ggplot(aes(x=bphprevnumtotal,y=activityneedratio))+
   geom_point(size=2) +
   geom_hline(aes(yintercept=mean_anr))
 
 bph_plot
 
 bph_activity_gp_sub_icb <- bph_activity_gp |>
   filter(sub_icb_location_code ==midlands_icbs[i]) |>
   mutate(mean_anr = mean(activityneedratio))
 
 bph_activity_midlands <- ICBTOBPH |>
   filter(reg22nm =="Midlands") |>
   mutate(activityneedratio = num_bph_spells/bphprevnumtotal,
     mean_anr = mean(activityneedratio))
 
# bph_activity_gp_sub_icb <- bph_activity_midlands
 
 
 # Generate the limits for the plot
 lkup<-data.frame(id=seq(round(min(bph_activity_gp_sub_icb$bphprevnumtotal)), max(bph_activity_gp_sub_icb$bphprevnumtotal), 1))
 #lkup$OneSigma <- sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))  
 #lkup$TwoSigma <- 1.96*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))) # 95%
 lkup$LowerTwoSigma <- bph_activity_gp_sub_icb$mean_anr - (1.96*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))))
 lkup$UpperTwoSigma <- bph_activity_gp_sub_icb$mean_anr + (1.96*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))))
 #lkup$ThreeSigma <- 3*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))) # 99.8%
 lkup$LowerThreeSigma <- bph_activity_gp_sub_icb$mean_anr - (3*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))))
 lkup$UpperThreeSigma <- bph_activity_gp_sub_icb$mean_anr + (3*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))))
 
 lkup<-gather(lkup, key, value,-id) |>
   mutate(key_label = case_when(key == "LowerThreeSigma" ~ "99.8% Lower CI",
                                key == "LowerTwoSigma" ~ "95% Lower CI",
                                key == "UpperThreeSigma" ~ "99.8% Upper CI",
                                key == "UpperTwoSigma" ~ "95% Upper CI")
          )
 
 plot <- bph_plot+ geom_line(aes(x=id, y=value, col=key), data=lkup, size = 1) + 
   xlab("Prevalence Number") +
   ylab("Activity to Need Ratio")+
   labs(title = "Funnel plot for BPH data",
        subtitle = paste0("Sub-ICB: ",bph_activity_gp_sub_icb$sub_icb_location_code))
 
 i=i+1
 
 p <- ggplotly(plot)
 
 print(p)
}
 




 

 

 