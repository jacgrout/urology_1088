# load(file = "data/projectimage.RData")
# 
# icblistsize <- ICBTOBPH |>
#   group_by(icb22nm,icb22) |>
#   summarise(icb_list_size=sum(list_size_bph_total))|>
#   filter(icb22 %in% midlands_icbs)


# BARPLOT

midlandsoutpatientsicb <- midlandsoutpatientsicb |>
  left_join(icblistsize) |>
  mutate(attendperthousand = (totalattends/icb_list_size)*1000)


bar_yields <- midlandsoutpatientsicb %>% 
  #select(-totalProcs) %>% 
  rowwise() %>% 
  ungroup() %>% 
  select(icb22nm, icb22, totalProcs, totalattends, attendperthousand) %>% 
  mutate(
    bar = round(attendperthousand/max(attendperthousand)*100, digits = 2),
    color = col_pal(bar),
    bar_chart = bar_chart(bar, color = color),
    bar_chart = map(bar_chart, ~gt::html(as.character(.x)))
    ) %>% 
  select(-bar, -color, -icb22)

# BARPLOT

rule10_bar <- bar_yields %>% 
  gt() %>% 
  cols_width(vars(bar_chart) ~ px(100),
             vars(totalProcs) ~ px(75),
             vars(totalattends) ~ px(75),
             vars(attendperthousand) ~ px(75),
  ) %>% 
  cols_label(
    totalProcs = md("Outpatient Procedures"),
    totalattends = md("Outpatient Attendances"),
    attendperthousand = md("Outpatient Attends /1000 List Size"),
    icb22nm = md("ICB"),
    bar_chart = ""
  ) %>% 
  cols_align(
    align = "right",
    columns = 2:5
  ) %>% 
  cols_align(
    align = "left",
    columns = vars(bar_chart)
  ) %>% 
  fmt_number(columns = 2:3,
             decimals=0) %>% 
  fmt_number(columns = 4:4,
             decimals=2) %>% 
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_column_labels(everything())
    )
  ) %>%  
  tab_options(
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.top.color = "white",
    table.border.top.width = px(3),
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    table_body.border.bottom.width = px(2),
    table_body.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(3)
  )
# %>% 
#   tab_footnote(footnote = "Source: SUS April 2018 - March 2023",
#                locations = cells_column_labels(
#                  columns =2:4
#                )) 

rule10_bar
