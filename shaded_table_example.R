
library(palmerpenguins)
library(tidyverse)
library(kableExtra)
penguin_df <- penguins %>% 
  na.omit()
penguin_table <- penguin_df %>% 
  mutate(q_bill_length = ntile(bill_length_mm, 3)) %>% 
  mutate(q_bill_length = recode(q_bill_length,
                                `1` = "#5881c1", `2` = "#f9bf07", `3` = "#686f73"
  )) %>% 
  select(species, island, bill_length_mm, q_bill_length) %>% 
  sample_n(10)

penguin_table %>%
  select(species, island, bill_length_mm) %>% 
  kbl() %>%
  kable_paper(bootstrap_options = "striped", full_width = F) %>% 
  column_spec(3, background = penguin_table$q_bill_length, color = penguin_table$q_bill_length, width="2em")
