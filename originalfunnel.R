
#library(ggplot2)
library(StrategyUnitTheme)
library(ggrepel)
#library(plotly)
library(gt)

midlands_gps <- ICBTOBPH |> filter(reg22nm == "Midlands") 

midlands_sub_icbs <- unique(midlands_gps$sub_icb_location_code)

midlands_icbs <- unique(midlands_gps$icb22)

bph_activity_gp <- left_join(bph_activity_gp, select(ICBTOBPH,icb22,icb22nm,org_code), by =  "org_code")

i=1

for(i in 1:19) {
  
  bph_plot <- bph_activity_gp |>
    filter(sub_icb_location_code ==midlands_sub_icbs[i]) |> # review later
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
  

  # Generate the limits for the plot for the whole of the midlands
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

  
  lkup_inner <- lkup |> filter(key == "LowerCI95_u" | key == "UpperCI95_u")
  lkup_outer <- lkup |> filter(key == "LowerCI998_u" | key == "UpperCI998_u") 

  plot <- bph_plot+ 
    geom_text_repel(aes(label = case_when(bph_plot$data$activityneedratio > (bph_plot$data$ubar + (3*sqrt(bph_plot$data$ubar/bph_plot$data$bphprevnumtotal))) |
                                             bph_plot$data$activityneedratio < (bph_plot$data$ubar - (3*sqrt(bph_plot$data$ubar/bph_plot$data$bphprevnumtotal)))
                                           ~ bph_plot$data$org_code
    )
    ))+
 geom_line(aes(x=id, y=value, group=key), colour="black",  data=lkup_inner, size = 0.5, linetype=5) +
    geom_line(aes(x=id, y=value, group=key), colour="black", data=lkup_outer, size = 0.5, linetype="solid") +
      xlab("Prevalence Number") +
    ylab("Activity to Need Ratio")+
    ylim(-0.1, 0.1)+
    labs(title = "Funnel plot for Benign Prostatic Hyperplasia",
         subtitle = paste0("Sub-ICB: ",bph_activity_gp_sub_icb$sub_icb_location_code))


  i=i+1
  
  print(plot)
}

#generate funnel plots by ICB


i=1

for(i in 1:11) {
  
  bph_plot <- bph_activity_gp |>
    filter(icb22 ==midlands_icbs[i]) |>
    mutate(mean_anr = mean(activityneedratio),
           median_anr = median(activityneedratio),
           ubar = sum(num_bph_spells)/sum(bphprevnumtotal)) |>
    ggplot(aes(x=bphprevnumtotal,y=activityneedratio))+
    geom_point(size=3,colour = "#f9bf07") +
    geom_point(size=3,colour = "black",shape=21) +
    geom_hline(aes(yintercept=ubar),size=1) +
    su_theme()
  
  bph_plot
  
  bph_activity_gp_icb <- bph_activity_gp |>
    filter(icb22 ==midlands_icbs[i]) |>
    mutate(ubar = sum(num_bph_spells)/sum(bphprevnumtotal))
  
  bph_activity_midlands <- ICBTOBPH |>
    filter(reg22nm =="Midlands") |>
    mutate(ubar = sum(num_bph_spells)/sum(bphprevnumtotal))
  
  # bph_activity_gp_sub_icb <- bph_activity_midlands
  
  
  # Generate the limits for the plot for the whole ofthe midlands
  lkup<-data.frame(id=seq(round(min(bph_activity_midlands$bphprevnumtotal)), max(bph_activity_midlands$bphprevnumtotal), 1))
  
  lkup$LowerCI95_u <- bph_activity_gp_icb$ubar - (1.96*sqrt(bph_activity_gp_icb$ubar/lkup$id))
  lkup$UpperCI95_u <- bph_activity_gp_icb$ubar + (1.96*sqrt(bph_activity_gp_icb$ubar/lkup$id))
  lkup$LowerCI998_u <- bph_activity_gp_icb$ubar - (3*sqrt(bph_activity_gp_icb$ubar/lkup$id))
  lkup$UpperCI998_u <- bph_activity_gp_icb$ubar + (3*sqrt(bph_activity_gp_icb$ubar/lkup$id))
  
  lkup<-gather(lkup, key, value,-id) 
  
  
  lkup_inner <- lkup |> filter(key == "LowerCI95_u" | key == "UpperCI95_u")
  lkup_outer <- lkup |> filter(key == "LowerCI998_u" | key == "UpperCI998_u") 
  
  plot <- bph_plot+ 
    geom_text_repel(aes(label = case_when(bph_plot$data$activityneedratio > (bph_plot$data$ubar + (3*sqrt(bph_plot$data$ubar/bph_plot$data$bphprevnumtotal))) |
                                            bph_plot$data$activityneedratio < (bph_plot$data$ubar - (3*sqrt(bph_plot$data$ubar/bph_plot$data$bphprevnumtotal)))
                                          ~ bph_plot$data$org_code
    )
    ))+
    geom_line(aes(x=id, y=value, group=key), colour="black",  data=lkup_inner, size = 0.5, linetype=5) +
    geom_line(aes(x=id, y=value, group=key), colour="black", data=lkup_outer, size = 0.5, linetype="solid") +
    xlab("Prevalence Number") +
    ylab("Activity to Need Ratio")+
    ylim(-0.1, 0.1)+
    labs(title = "Funnel plot for Benign Prostatic Hyperplasia",
         subtitle = bph_activity_gp_icb$icb22nm)
  
  

  
  print(plot)
  
  #generate table
  table_data <-select(plot$data,org_code,sub_icb_location_code,list_size_bph_total,bphprevnumtotal,
                      num_bph_spells,group,activityneedratio,icb22,icb22nm,ubar)
  select(table_data,-sub_icb_location_code,-icb22) |> 
    group_by(icb22nm) |> # respects grouping from dplyr
    gt(rowname_col = "org_code") |>
    cols_hide(ubar) |>
    fmt_number(
      columns = c(3:3), # reference cols by position
      decimals = 0 # decrease decimal places
    ) |>
    fmt_number(
      columns = c(6:7), # reference cols by position
      decimals = 3 # decrease decimal places
    ) |>
    tab_spanner(
      label = "Activity To Need Ratio by GP Practice", 
      columns = c(1:7)
    ) |>
    tab_source_note(source_note = "Data: SUS, GP list size etc etc")|>
    
    tab_style(
      style = list(
        cell_fill(color = "#686f73", alpha = 0.2)
      ),
      locations = cells_body(
        columns = c(1:7),
        rows = group == "20-30%" | group == "30-40%" | group == "40-50%" | group == "50-60%" | group == "60-70%" | group == "70-80%" |
          group == "80-90%" | group == "90%+"
      )
    )|>
    
    tab_style(
      style = list(
        cell_text(color = "black"),
        cell_fill(color = "#5881c1", alpha = 0.2)
      ),
      locations = cells_body(
        columns = c(1:7),
        rows = activityneedratio > (ubar + (1.96*sqrt(ubar/bphprevnumtotal)))
      )
    ) |>
    tab_style(
      style = list(
        cell_text(color = "black"),
        cell_fill(color = "#f9bf07", alpha = 0.2)
      ),
      locations = cells_body(
        columns = c(1:7),
        rows = activityneedratio < (ubar - (1.96*sqrt(ubar/bphprevnumtotal)))
      )
    ) 
  
  
  i=i+1 
}


