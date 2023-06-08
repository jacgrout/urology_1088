

# BPH



bph_plot <- bph_activity_icb_drugs |>
  filter(sub_icb_location_code %in% midlands_sub_icbs) |>
  mutate(mean_anr = mean(presc_anr),
         median_anr = median(presc_anr),
         ubar = sum(items_total)/sum(bphprev)) |>
  ggplot(aes(x=bphprev,y=presc_anr))+
  geom_point(size=3,colour = "#f9bf07") +
  geom_point(size=3,colour = "black",shape=21) +
  geom_hline(aes(yintercept=ubar),size=1) +
  su_theme()

bph_plot



oab_plot <- oab_activity_icb_drugs |>
  filter(sub_icb_location_code %in% midlands_sub_icbs) |>
  mutate(mean_anr = mean(presc_anr),
         median_anr = median(presc_anr),
         ubar = sum(items_total)/sum(oabprev)) |>
  ggplot(aes(x=oabprev,y=presc_anr))+
  geom_point(size=3,colour = "#f9bf07") +
  geom_point(size=3,colour = "black",shape=21) +
  geom_hline(aes(yintercept=ubar),size=1) +
  su_theme()

oab_plot


oab_drugs <- all_drugs |> 
  mutate(year=substr(date,1,4),
         finyear=case_when(date<='2019-03-31' ~ '2018/19',
                           date>='2019-04-01' & date <= '2020-03-31' ~ '2019/20', 
                           date>='2020-04-01' & date <= '2021-03-31' ~ '2020/21', 
                           date>='2021-04-01' & date <= '2022-03-31' ~ '2021/22',
                           date>='2022-04-01' & date <= '2023-03-31' ~ '2022/23',
                           TRUE ~ 'xx/xx')
  )


oabplot <- all_drugs_summary |> filter(row_id %in% midlands_sub_icbs) |>
  ggplot(aes(x=row_id, y=items_total,fill=drug)) + 
  geom_col() +
  ggtitle("Prescription Items for OAB Medications (Last 5 years)")+
  xlab("Sub-ICB")+
  ylab("Total Prescribed Items")+
  scale_fill_su(palette="reds") +
  su_theme()


bph_plot <- ggplot(bph_drugs,aes(x=finyear,y=items,size=items, colour=drug)) 
bph_plot + geom_point() + facet_grid(drug~.)

oab_plot <- ggplot(oab_drugs,aes(x=finyear,y=items,size=items, colour=drug)) 
oab_plot + geom_point() + facet_grid(drug~.)
