needs(tidyverse, RSocrata, rio)

## Import COVID Case data for 2020 in West Coast States
months <- as.character(c(1:12)) %>% 
  str_pad(width = 2, side = "left", pad = "0")

states <- c("CA", "OR", "WA")

grid <- expand_grid(months, states)

covid_data <- map2_dfr(grid$months, grid$states, ~read.socrata(glue("https://data.cdc.gov/resource/n8mc-b4w4.json?case_month=2020-{.x}&res_state={.y}"), 
                                                               app_token = "ulwlSXgldpWsLLqFRwL39mCYZ",
                                                               email = "cives@uoregon.edu",
                                                               password = "wujnuR-9ranbe-hogbit"))


export(covid_data, "west_coast_covid_data.csv")


covid_data <- import("west_coast_covid_data.csv")

county_covid <- covid_data %>%
  drop_na(county_fips_code) %>% 
  group_by(case_month, county_fips_code, age_group) %>% 
  nest() %>% 
  mutate(total_cases = map_dbl(data, ~nrow(.x)),
         age_group = ifelse(is.na(age_group), "Unknown", age_group)) %>% 
  select(-data) %>% 
  arrange(county_fips_code, case_month)

export(county_covid, "county_case_counts.csv")

covid_data %>% nrow()
## Process Attendance Data
county_data <- read.csv("schools_county_csv.csv") %>% 
  filter(year == 2020) %>% 
  rename(fips = countyfips3) %>% 
  mutate(county_fips = as.character(county_fips),
         county_fips = str_pad(county_fips, width = 5, side = "left", pad = "0"),
         across(7:18, ~ .x *100))


## Import Map Data
map_url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'

counties_json <- rjson::fromJSON(file=url) %>% 
  unnest()
  