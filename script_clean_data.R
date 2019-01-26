library(dplyr)
library(tidyverse)
library(forcats)

data <- read_csv("data/ucr_crime_1975_2015.csv")

new_data <- data %>% 
  mutate(abb = str_sub(ORI, 1, 2)) %>% 
  mutate(state_name = state.name[match(abb,state.abb)]) %>% 
  mutate(city_name = sapply(str_split(data$department_name, ","), head, 1)) %>% 
  mutate(city_state = paste(city_name, state_name, sep = ", ")) %>% 
  select(-ORI, -department_name, -source, -url, -months_reported) %>% 
  drop_na()

write_csv(new_data, "data/clean_data.csv")

new_data_2 <- new_data %>% 
  gather(homs_sum, rape_sum, rob_sum, agg_ass_sum, violent_crime, key = "crime", value = "cases") %>% 
  select(year, total_pop, state_name, city_name, crime, cases) %>% 
  mutate(crime = as_factor(crime)) %>% 
  mutate(year = as.factor(year))

new_data_2$crime <- 
  fct_recode(new_data_2$crime, 'homicides' = 'homs_sum', 'rape' = 'rape_sum', 'robbery' = 'rob_sum', 
              'aggravated assault' = 'agg_ass_sum', 'total violent crime' = 'violent_crime')

state_avg <- new_data_2 %>% 
              group_by(state_name, crime, year) %>% 
              summarise(avg_case = mean(cases))

state_pop <- new_data_2 %>% 
                group_by(state_name, year) %>% 
                summarise(state_pop = sum(total_pop))

new_data_3 <- left_join(left_join(new_data_2, state_avg), state_pop)

write_csv(new_data_3, "data/clean_data_tidy.csv")
