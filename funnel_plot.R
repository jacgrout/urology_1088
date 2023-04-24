library(FunnelPlotR)
library(COUNT)
library(ggplot2)

# lets use the 'medpar' dataset from the 'COUNT' package. Little reformatting needed
data(medpar)
medpar$provnum<-factor(medpar$provnum)
medpar$los<-as.numeric(medpar$los)

mod<- glm(los ~ hmo + died + age80 + factor(type), family="poisson", data=medpar)
summary(mod)
#> 
#> Call:
#> glm(formula = los ~ hmo + died + age80 + factor(type), family = "poisson", 
#>     data = medpar)
#> 
#> Deviance Residuals: 
#>     Min       1Q   Median       3Q      Max  
#> -5.7309  -1.9554  -0.5529   0.9717  14.5487  
#> 
#> Coefficients:
#>               Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)    2.26875    0.01246 182.011  < 2e-16 ***
#> hmo           -0.07637    0.02393  -3.192  0.00142 ** 
#> died          -0.24574    0.01826 -13.458  < 2e-16 ***
#> age80         -0.02141    0.02050  -1.045  0.29617    
#> factor(type)2  0.24921    0.02099  11.871  < 2e-16 ***
#> factor(type)3  0.74869    0.02627  28.496  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for poisson family taken to be 1)
#> 
#>     Null deviance: 8901.1  on 1494  degrees of freedom
#> Residual deviance: 7977.7  on 1489  degrees of freedom
#> AIC: 13705
#> 
#> Number of Fisher Scoring iterations: 5
#> 
 medpar$prds<- predict(mod, type="response")
 
 a<-funnel_plot(numerator=medpar$los, denominator=medpar$prds, group = medpar$provnum, 
                title = 'Length of Stay Funnel plot for `medpar` data', data_type="SR", limit=99,
                draw_unadjusted = TRUE, draw_adjusted = FALSE, label="outlier")
 print(a)
 

 b<-funnel_plot(numerator=medpar$los, denominator=medpar$prds, group = medpar$provnum, data_type = "SR",
                title = 'Length of Stay Funnel plot for `medpar` data', draw_unadjusted = FALSE,
                draw_adjusted = TRUE, sr_method = "SHMI", label="outlier", limit=99)
 
 print(b)
 
 
 
 bph_plot <- bph_activity_gp |>
   filter(sub_icb_location_code =="01Y") |>
   mutate(mean_anr = mean(activityneedratio),
          median_anr = median(activityneedratio)) |>
   ggplot(aes(x=bphprevnumtotal,y=activityneedratio,size=3, colour="red"))+
   geom_point() +
   geom_hline(aes(yintercept=mean_anr)) +
   geom_hline(aes(yintercept=median_anr))
 
 bph_plot
 
 bph_activity_gp_sub_icb <- bph_activity_gp |>
   filter(sub_icb_location_code =="01Y") |>
   mutate(mean_anr = mean(activityneedratio))
 
 
 
 # Generate the limits for the plot
 lkup<-data.frame(id=seq(round(min(bph_activity_gp_sub_icb$bphprevnumtotal)), max(bph_activity_gp_sub_icb$bphprevnumtotal), 1))
 #lkup$OneSigma <- sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))
 #lkup$TwoSigma <- 1.96*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id)))
 lkup$LowerTwoSigma <- bph_activity_gp_sub_icb$mean_anr - (1.96*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))))
 lkup$UpperTwoSigma <- bph_activity_gp_sub_icb$mean_anr + (1.96*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))))
 #lkup$ThreeSigma <- 3*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id)))
 lkup$LowerThreeSigma <- bph_activity_gp_sub_icb$mean_anr - (3*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))))
 lkup$UpperThreeSigma <- bph_activity_gp_sub_icb$mean_anr + (3*(sqrt(bph_activity_gp_sub_icb$mean_anr * ((1-bph_activity_gp_sub_icb$mean_anr)/lkup$id))))
 
 lkup<-gather(lkup, key, value,-id)
 
 bph_plot+ geom_line(aes(x=id, y=value, col=key), data=lkup)
 
 
 
 
 
 C<-funnel_plot(numerator=bph_plot_data$activityneedratio, denominator=bph_plot_data$bphprevnumtotal, group = bph_plot_data$org_code, 
                data_type = "RC",
                title = 'Funnel plot for `bph_plot_data` data', draw_unadjusted = FALSE,
                draw_adjusted = TRUE, label="outlier", limit=99)
 
 print(c)
 