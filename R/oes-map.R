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
  mutate(GEOID = str_pad(GEOID, width = 7, pad = "0")) %>% 
  select(AFFGEOID, GEOID, NAME, LSAD, ALAND, AWATER, geometry)

shape_necta <- sf::read_sf(("data/shapefiles/cb_2017_us_necta_500k.shp")) %>% 
  sf::st_centroid() %>% 
  mutate(GEOID = str_pad(GEOID, width = 7, pad = "0")) %>% 
  select(AFFGEOID, GEOID, NAME, LSAD, ALAND, AWATER, geometry)

shape_metro <- rbind(shape_cbsa, shape_necta)

shape_state <- sf::read_sf("data/shapefiles/cb_2016_us_state_20m.shp") %>% 
  filter(!STATEFP %in% c("02", "15", "72"))

write_rds(cleaned, "app/cleaned.rds")
write_rds(shape_metro, "app/shape_metro.rds")
write_rds(shape_state, "app/shape_state.rds")
