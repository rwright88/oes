# read, clean, write oes map data =====

library(tidyverse)
library(sf)

df <- read_csv("data/oes.csv") %>% 
  filter(str_sub(area_code, 3, 7) != "00000") %>%
  mutate(emp_rate = as.numeric(emp_rate))

emp_total <- df %>% 
  filter(occupation_code == "000000") %>% 
  select(area_code, emp_total = employment)

cleaned <- df %>% 
  left_join(emp_total, by = "area_code")

shape_cbsa <- sf::read_sf("data/shapefiles/cb_2017_us_cbsa_20m.shp") %>% 
  sf::st_centroid() %>% 
  mutate(CBSAFP = str_pad(CBSAFP, width = 7, pad = "0"))

shape_state <- sf::read_sf("data/shapefiles/cb_2016_us_state_20m.shp") %>% 
  filter(!STATEFP %in% c("02", "15", "72"))

write_rds(cleaned, "app/cleaned.rds")
write_rds(shape_cbsa, "app/shape_cbsa.rds")
write_rds(shape_state, "app/shape_state.rds")
