#FP 653 Project Script

library(tidyverse)
library(janitor)
library(ggplot2)
library(here)
library(rio)
library(usmap)
library(maps)
library(glue)
#bring in data
county_sch <- import(here("schools_county_csv2.xlsx")) %>% 
  clean_names() %>%
  as_tibble()

#need the variable 'fips' for mapping
cmap <- rename(county_sch, "fips" = "countyfips4")

#only keep 2020 data
cmap2020 <- 
  cmap %>% 
    filter(year == 2020)

#we may not need these next two data sets
#district_sch <- import(here("schools_district_csv.csv")) %>% 
#  clean_names() %>%
#  as_tibble()

#state_sch <- import(here("schools_state_csv.csv")) %>% 
#  clean_names() %>%
#  as_tibble()

#function1, save plots to a folder 


#function2, convert month numbers into indicative string 



# us map with counties for year 2020

p<-plot_usmap(regions = "counties", data = cmap2020, values = "share_all_closed_50") + 
  labs(title = "Schools experiencing a year-over-year decline of at least 50 percent for month:",
       subtitle = "subtitle here") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))
p
#adding state boundaries
states <- plot_usmap(
  "states", 
  color = "black",
  fill = alpha(0.01)
) 

states

#more formatting for map





#function to output new map for each month?
#may not need a function for this since we can do this with facet_wrap



#shiny app to toggle between months of the 2020 year
