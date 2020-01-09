# library(ggmap)
library(tidycensus)
library(lubridate)
library(tidyverse)

### Load data ----
sf19 <- read_csv('Stop Data_09092019.csv')

### Filter for non-traffic stops in which there was a frisk ----
frisks <- sf19 %>% 
  filter(
    person_search_or_protective_pat_down == 1,
    stop_type == 'Non-ticket Stop',
    !grepl('traffic', stop_reason_nonticket, ignore.case = TRUE),
    !grepl('crash', stop_reason_nonticket, ignore.case = TRUE),
    !grepl('equipment', stop_reason_nonticket, ignore.case = TRUE),
    !grepl('moving violation', stop_reason_nonticket, ignore.case = TRUE)
  ) %>% 
  select(
    stop_date,
    stop_time,
    race_ethnicity,
    gender,
    age,
    stop_reason_nonticket,
    arrest_date,
    arrest_charges,
    stop_district,
    stop_location_block
  ) %>% 
  mutate(
    stop_date = mdy(stop_date),
    stop_datetime = as_datetime(paste(stop_date, stop_time)),
    arrest_date = mdy(arrest_date),
    age_bucket=cut(
      as.integer(age), 
      breaks=c(-Inf, 24, 30, 36, 42, 49, Inf),
      labels=c('17-24', '25-30', '31-36', '37-42', '43-49','50+')
    ) %>% 
      droplevels() %>% 
      as.character(), 
    age_group = ifelse(is.na(age_bucket), age, age_bucket)
  ) %>% 
  select(-age_bucket)

### Geocode addresses - DO NOT RUN ----
# Google Maps API key required
# sf_foot <- sf_foot %>% rename(address = stop_location_block)
# frisks_geocoded <- sf_foot %>% mutate_geocode(address)
load('frisks_geocoded_19.RData')

### Pull census data ----
# Census API key required
census_with_frisks <- get_acs(
  geography = "tract",
  variables = c('B02001_001', 'B02001_002', 'B02001_003', 'B02001_004', 'B02001_005', 'B02001_006', 'B02001_007', 'B02001_008', 'B02001_009', 'B02001_010', 'B06011_001'),
  output = "wide",
  state = "DC",
  geometry = TRUE
) %>% 
  transmute(
    census_tract = GEOID,
    MedianIncome = B06011_001E,
    Total = B02001_001E,
    White = B02001_002E,
    Black = B02001_003E,
    Asian = B02001_005E,
    MixedRace = B02001_008E + B02001_009E + B02001_010E,
    AmericanIndian = B02001_004E,
    PacificIslander = B02001_006E,
    Other = B02001_007E,
    prcnt_white = B02001_002E / B02001_001E,
    prcnt_black = B02001_003E / B02001_001E
  ) %>% 
  left_join(
    frisks_geocoded %>%
      mutate(
        census_tract = str_sub(census_code,1,11)
      ) %>%  
      group_by(
        census_tract
      ) %>% 
      summarise(
        black_stops = sum(if_else(race_ethnicity == 'Black', 1, 0)),
        total_stops = n(),
        prcnt_black_stops = black_stops / total_stops, 
        call_for_service = sum(if_else(!is.na(str_match(stop_reason_nonticket, "Call for service")),1,0)),
        prcnt_call_for_service = call_for_service / total_stops
      )
  ) %>% 
  mutate(total_stops = replace_na(total_stops, 0))

save(frisks, census_with_frisks, file = 'frisk_19_clean.RData')
