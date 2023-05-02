
library(ggplot2)
library(plotly)



midlands_gps <- ICBTOBPH |> filter(reg22nm == "Midlands") 

midlands_sub_icbs <- unique(midlands_gps$sub_icb_location_code)


i=1 

for(i in 1:19) {

 bph_plot <- bph_activity_gp |>
   filter(sub_icb_location_code == midlands_sub_icbs[i]) |>
   mutate(mean_anr = mean(activityneedratio),
          median_anr = median(activityneedratio)) |>
   ggplot(aes(x=bphprevnumtotal,y=activityneedratio))+
   geom_point(size=2) +
   geom_hline(aes(yintercept=mean_anr)) 
   # annotate("text", 100, 6.5, label="95% limit", colour="black", 
   #          size=3, hjust=0) +
   # annotate("text", 100, 6.4, label="99.9% limit", colour="red", 
   #          size=3, hjust=0) 
 
 bph_plot
 
 bph_activity_gp_sub_icb <- bph_activity_gp |>
   filter(sub_icb_location_code ==midlands_sub_icbs[i]) |>
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
 
 # ggplotly(plot)
 
 print(plot)
}
 



  
  i=1 
  for(i in 1:length(midlands_sub_icbs)) {
 
    activity_gp <- bph_activity_gp |> rename(prevnumtotal=bphprevnumtotal,
                                             num_spells=num_bph_spells)
    ICBTOCOND <- ICBTOBPH |> rename(prevnumtotal=bphprevnumtotal,
                       num_spells=num_bph_spells)
    plot <- generate_funnel(activity_gp,midlands_sub_icbs[i],"Midlands",ICBTOCOND,"BPH")
    print(plot)
    activity_gp <- oab_activity_gp |> rename(prevnumtotal=oabprevnumtotal,num_spells=num_oab_spells)
    ICBTOCOND <- ICBTOOAB |> rename(prevnumtotal=oabprevnumtotal,
                                    num_spells=num_oab_spells)
    plot <- generate_funnel(activity_gp,midlands_sub_icbs[i],"Midlands",ICBTOCOND,"OAB")
    i=i+1
    print(plot)
  }

 