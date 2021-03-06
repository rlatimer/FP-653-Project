#FP 653 Project Script

library(tidyverse)
library(janitor)
library(ggplot2)
library(here)
library(rio)
library(usmap)
library(maps)
library(glue)
library(reactable)
library(repurrrsive)
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

#creating a plot for  each county using CDC data 

cdc <- import(here("county_case_counts.csv")) %>% 
  clean_names() %>%
  #mutate(month = month.name[cdc$month]) #month numbers to months without function
  as_tibble()

#numbers to month names
#numbers to month function, generalized?
monthnum_to_names <- function(column) {
  #create dataframe with a singlecolumen with numbers 1-12
  #number <- c(1:12) 
  found <- rep(NA, length(column))
  df <- data.frame(found)
  #check if number is between 1-12
  df$found <- ifelse(sapply(column, between, arg1 = column,arg2 = 1, arg3 = 12)) 
 
  #if df$found is TRUE
  if (df$found == "TRUE") {
    column <-mutate(month_name = month.name[column])
  } 
}

monthnum_to_names(cdc$month)

cdc$month



cdc_plots <- cdc %>% 
  group_by( fips) %>% 
  nest() %>% 
  mutate(plot = pmap(list(fips, data), ~{
    ggplot(..2, aes(x=case_month,
                    y=total_cases,
                    color = age_group)) +
      geom_point() +
      geom_line(alpha = 0.7, size = 1) +
      scale_x_discrete(limits = c(0, max(cdc$case_month)), 
                       expand = c(0, 0)) +
      labs(title = glue("County with FIPS code: {.x} "),
           x = "Month in 2020",
           y = "Total Cases",
           color = 'Age Group')
  })
  )
#example plot
cdc_plots$plot[[1]]  

fs::dir_create(here::here("plots"))
county <- str_replace_all(tolower(cdc_plots$fips), " ", "-")
path <- here::here("plots", glue("{county}.png"))

walk2(path, cdc_plots$plot, ggsave,
      width = 9.5, 
      height = 6.5,
      dpi = 500)

#table of CDC data
cmap2020_2 <- cmap2020 %>% 
  mutate(fips = as.integer(fips))

cdc_table_data<-left_join(cdc,cmap2020_2) %>% 
  select(1, 8, 25, 26, 3, 4)

cdc_table <- cdc_table_data %>% 
  reactable(
  searchable = TRUE,
  filterable = TRUE,
  columns = list(
    case_month = colDef(name="Month"),
    year = colDef(name="Year"),
    county_name = colDef(name="County"),
    state_abb = colDef(name="State"),
    age_group = colDef(name="Age group"),
    total_cases = colDef(name="Total COVID-19 cases")),
  defaultPageSize = 11
  )

cdc_table
