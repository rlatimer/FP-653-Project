#FP 653 Project Script

library(tidyverse)
library(janitor)
library(ggplot2)
library(here)
library(rio)
library(usmap)
library(maps)

#bring in data

county_sch <- import(here("schools_county_csv2.xlsx")) %>% 
  clean_names() %>%
  as_tibble()

#need the variable 'fips' for mapping
cmap <- rename(county_sch, "fips" = "countyfips4")

#only keep 2020 data

#we may not need these next two data sets
#district_sch <- import(here("schools_district_csv.csv")) %>% 
#  clean_names() %>%
#  as_tibble()

#state_sch <- import(here("schools_state_csv.csv")) %>% 
#  clean_names() %>%
#  as_tibble()

#function1


#function2



# us map with counties

p<-plot_usmap(regions = "counties", data = cmap, values = "share_all_closed_50") + 
  labs(title = "title",
       subtitle = "subtitle here") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

#adding state boundaries
states <- plot_usmap(
  "states", 
  color = "black",
  fill = alpha(0.01)
) 

#more formatting for map



#function to output new map for each month?



#shiny app to toggle between months of the 2020 year
