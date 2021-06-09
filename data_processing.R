needs(tidyverse, RSocrata, rio)
import()
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

# Count Cases by County and Age Group
covid_data <- import("west_coast_covid_data.csv")

county_covid <- covid_data %>%
  drop_na(county_fips_code) %>% 
  group_by(case_month, county_fips_code, age_group) %>% 
  nest() %>% 
  mutate(total_cases = map_dbl(data, ~nrow(.x)),
         age_group = ifelse(is.na(age_group), "Unknown", age_group),
         case_month = as.numeric(
           substr(case_month, 6, 7))) %>% 
  select(-data) %>% 
  arrange(county_fips_code, case_month) %>% 
  rename(fips = county_fips_code,
         month = case_month)

## Importing population data for per capita estimates
county_demo <- county_census %>% 
  janitor::clean_names() %>% 
  select(fips, popestimate2019)

## Joining population data with covid case data and calculating additional variables
covid_cases <- county_covid %>% 
  mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
  rename(month = case_month) %>% 
  group_by(fips, month) %>% 
  mutate(all_cases = sum(total_cases)) %>% 
  ungroup() %>% 
  left_join(county_demo) %>% 
  distinct(month, fips, all_cases, popestimate2019) %>% 
  group_by(fips, month) %>% 
  mutate(capita_covid = round((all_cases/(popestimate2019/100000)), 2),
         per_pop = round((1/(all_cases/popestimate2019)), 0),
         share_pop = paste("1 in", per_pop, sep = " ")) %>% 
  select(fips, month, capita_covid, all_cases, per_pop, share_pop)

export(covid_cases, "county_case_counts.csv")

## Process Attendance Data
county_data <- read.csv("schools_county_csv.csv") %>% 
  filter(year == 2020) %>% 
  rename(fips = countyfips3) %>% 
  mutate(fips = as.character(fips),
         fips = str_pad(fips, width = 5, side = "left", pad = "0"),
         across(7:18, ~ .x *100))

export(county_data, "app_attendance_data.csv")



## Import Map Data
map_url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'

counties_json <- rjson::fromJSON(file = url) %>% 
  unnest()


wst_cst <- tigris::counties(state = c("California", "Oregon", "Washington")) %>% 
  janitor::clean_names() %>% 
  select(geoid, geometry) %>% 
  rename(fips = geoid)

us_map <- tigris::counties() %>% 
  janitor::clean_names()

us_map <- us_map %>% 
  janitor::clean_names() %>% 
  select(geoid, geometry) %>% 
  rename(fips = geoid)

write_sf(us_map, "us_county_geom.shp")
write_sf(wst_cst, "wst_cst.shp")

  