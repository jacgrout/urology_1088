
source("functions.R")


# Calculate the need
source("calculate_need.R")

# Take the need identified in calculate need script and produce maps
source("create_map_of_need.R")

#Work out the change in list sizes from 2018 to 2023
source("list_size_old.R")

source("calc_activity_need_ratio.R")

source("ratio_map.R")

#source("originalfunnel.R")

#source("originalfunnel_oab.R")

#source("new_funnel_plot_ppi.R")

source("calculate_activity_gap.R")


save.image(file = "data/projectimage.RData")

source("outpatients.R")
#Sources: Strategy Unit analysis; SUS+, National Commisioning Data Repository 


list_size_age_group_compare_23all


View(bph_activity_gp)