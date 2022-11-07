library(readr)
library(tidyverse)
library(dplyr)


ca_data <- read_csv('Infrastructure_data/Infrastructure_data/California/CA_water_audit_data.csv')


#filter the entries with acre feet
ca_data_full <- rbind( ca_data %>%
  filter(units == 'Acre-feet') %>%
  #convert the acreft to millions of gallons
  dplyr::mutate(dplyr::across("water supplied own source":"non-revenue water", ~.x*0.271328))%>%
  #update the units columns to millions of gallons us
  mutate(units = 'Million gallons (US)'), 
  ca_data %>% filter(units == 'Million gallons (US)'))


ca_data_full[ca_data_full$year == 16&!is.na(ca_data_full$year),]$year <- 2016
ca_data_full[ca_data_full$year == 20018&!is.na(ca_data_full$year),]$year <- 2018

write_csv(ca_data_full, '1_data_inventory/out/california_data_all_years.csv')


