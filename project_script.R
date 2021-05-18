#FP 653 Project Script

library(tidyverse)
library(janitor)
library(ggplot2)
library(here)
library(rio)

county_sch <- import(here("schools_county_csv.csv")) %>% 
  clean_names() %>%
  as_tibble()

district_sch <- import(here("schools_district_csv.csv")) %>% 
  clean_names() %>%
  as_tibble()

state_sch <- import(here("schools_state_csv.csv")) %>% 
  clean_names() %>%
  as_tibble()