library(dplyr)
library(tidyverse)

data <- read_csv("data/ucr_crime_1975_2015.csv")

new_data <- data %>% 
  mutate(abb = str_sub(ORI, 1, 2)) %>% 
  mutate(state_name = state.name[match(abb,state.abb)]) %>% 
  mutate(city_name = sapply(str_split(data$department_name, ","), head, 1)) %>% 
  mutate(city_state = paste(city_name, state_name, sep = ", ")) %>% 
  select(-ORI, -department_name, -source, -url, -months_reported) %>% 
  drop_na()

write_csv(new_data, "data/clean_data.csv")

