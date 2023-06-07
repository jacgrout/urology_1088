midlands_gps <- ICBTOBPH |> filter(reg22nm == "Midlands") 

midlands_sub_icbs <- unique(midlands_gps$sub_icb_location_code)

midlands_icbs <- unique(midlands_gps$icb22)

bph_activity_gp <- left_join(bph_activity_gp, select(ICBTOBPH,icb22,icb22nm,org_code), by =  "org_code")


bph_activity_gap <- bph_activity |> 
  mutate(activity_gap = (max(bph_activity$activityneedratio)*bphprev)-bph_spells,
         gap_percentage_change = (activity_gap/bph_spells)*100)

oab_activity_gap <- oab_activity |> 
  mutate(activity_gap = (max(oab_activity$activityneedratio)*oabprev)-oab_spells,
         gap_percentage_change = (activity_gap/oab_spells)*100)

ppi_activity_gap <- ppi_activity |> 
  mutate(activity_gap = (max(ppi_activity$activityneedratio)*ppiprevsum)-ppi_spells,
         gap_percentage_change = (activity_gap/ppi_spells)*100)


bph_activity$activityneedratio

# ------

sub_icbs_mid <- select(ICBTOBPH,sub_icb_location_code, new_sub_icb_location_name, icb22, icb22nm) |>
  filter(sub_icb_location_code %in% midlands_sub_icbs)|>
  filter(icb22 != "NA")|>
  unique()

bph_activity_gap <- bph_activity |> 
  filter(sub_icb_location_code %in% midlands_sub_icbs) |>
  mutate(uq_activity_gap = (quantile(activityneedratio,prob=0.75,type=1)*bphprev)-bph_spells,
         uq_gap_percentage_change = (uq_activity_gap/bph_spells)*100,
         uq_gap_percentage_change_pos = case_when(((uq_activity_gap/bph_spells)*100)<0 ~ 0,
                                                  ((uq_activity_gap/bph_spells)*100)>=0 ~ (uq_activity_gap/bph_spells)*100
                                                  ),
         
         uq_gap_percentage_change_neg = case_when(((uq_activity_gap/bph_spells)*100)>=0 ~ 0,
                                                  ((uq_activity_gap/bph_spells)*100)<0 ~ (uq_activity_gap/bph_spells)*100
                                                   ),
         activity_gap = (max(activityneedratio)*bphprev)-bph_spells,
         gap_percentage_change = (activity_gap/bph_spells)*100)|>
  left_join(sub_icbs_mid) |>
  select(new_sub_icb_location_name,bphprev,list_size_bph,bph_spells,activityneedratio,activity_gap,gap_percentage_change,uq_gap_percentage_change,
         uq_gap_percentage_change_pos,
         uq_gap_percentage_change_neg)
#,uq_activity_gap, uq_gap_percentage_change

bar_yields <- bph_activity_gap |> 
  rowwise() |>
  ungroup() |> 
  mutate(
    bar2 = uq_gap_percentage_change_neg*-1,
    color2 = col_pal(uq_gap_percentage_change,"ggsci::red_material"),
    bar_chart2 = bar_chart(bar2, color = color2),
    bar_chart2 = map(bar_chart2, ~gt::html(as.character(.x))),
    
    bar = uq_gap_percentage_change_pos,
    color = col_pal(uq_gap_percentage_change,"ggsci::blue_material"),
    bar_chart = bar_chart(bar, color = color),
    bar_chart = map(bar_chart, ~gt::html(as.character(.x)))

  ) |>
  select(-bar, -color,-bar2, -color2,-uq_gap_percentage_change_neg,-uq_gap_percentage_change_pos)

bph_activity_gap_table <- 
  bar_yields |>
  gt()|>
  cols_width(c(bar_chart) ~ px(100),
             c(bar_chart2) ~ px(100),
             c(bphprev) ~ px(50),
             c(list_size_bph) ~ px(50),
             c(bph_spells) ~ px(50),
             c(activityneedratio) ~ px(50),
             c(activity_gap) ~ px(50),
             c(gap_percentage_change) ~ px(50),
             c(uq_gap_percentage_change) ~ px(50)
             # ,
             # c(uq_gap_percentage_change_neg) ~ px(50),
             # c(uq_gap_percentage_change_pos) ~ px(50)
  ) %>% 
  fmt_number(columns = 2:4,
             decimals=0) |>
  fmt_number(columns = 5:8,
             decimals=2) |>
  cols_label(
    new_sub_icb_location_name=md("Sub-ICB"),
    bphprev = md("Prevalence"),
    list_size_bph = md("List Size"),
    bph_spells = md("Spells"),
    activityneedratio = md("Activity to Need ratio"),
    activity_gap = md("Activity Gap"),
    gap_percentage_change = md("% change"),
    uq_gap_percentage_change = md("UQ % change"),
    # uq_gap_percentage_change_neg = md("UQ % change neg"),
    # uq_gap_percentage_change_pos = md("UQ % change pos"),
    bar_chart2 = "",
    bar_chart = ""
  ) |>
  cols_align(
    align = "right",
    columns = 2:8
  ) |>
  cols_align(
    align = "left",
    columns = c(bar_chart)
  ) |>
  cols_align(
    align = "right",
    columns = c(bar_chart2)
  ) |>
  tab_header(
    title = 'Benign Prostatic Hyperplasia Activity Gap - Midlands'
  ) |>
  tab_source_note(source_note = "Data: SUS??")

bph_activity_gap_table
