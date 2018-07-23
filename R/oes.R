# Occupation employment and wages =====

library(tidyverse)

url_oe_series <- "https://download.bls.gov/pub/time.series/oe/oe.series"
url_oe_data   <- "https://download.bls.gov/pub/time.series/oe/oe.data.1.AllData"
url_oe_area   <- "https://download.bls.gov/pub/time.series/oe/oe.area"
url_oe_occup  <- "https://download.bls.gov/pub/time.series/oe/oe.occupation"

# read =====

datatypes <- c("01", "13", "16")

oe_series <- read_tsv(url_oe_series) %>% 
  filter(
    # area_code     == "0000000",    # National
    datatype_code %in% datatypes,  # Employment, annual median wage, employment per 1,000 jobs
    industry_code == "000000"      # Cross-industry, private, federal, state, and local government
  )

oe_data <- read_tsv(url_oe_data) %>% 
  filter(series_id %in% oe_series[["series_id"]])

oe_area <- read_tsv(url_oe_area)

oe_occup <- read_tsv(url_oe_occup)

# clean =====

rec_type <- function(x) {
  case_when(
    x == "01" ~ "employment",
    x == "13" ~ "wage_p50",
    x == "16" ~ "emp_rate"
  )
}

cleaned <- oe_series %>%
  left_join(oe_data, by = "series_id") %>%
  mutate(value = as.numeric(value)) %>%
  mutate(datatype_code = rec_type(datatype_code)) %>% 
  select(area_code, occupation_code, datatype_code, value) %>%
  spread(datatype_code, value) %>% 
  left_join(oe_area, by = "area_code") %>%
  left_join(oe_occup, by = "occupation_code") %>%
  select(area_code, area_name, occupation_code, occupation_name, employment, emp_rate, wage_p50)

rm(oe_series, oe_data)
gc()

# print =====

cleaned %>% 
  filter(str_detect(area_name, "National"), str_detect(occupation_code, "0000$")) %>%
  mutate(occupation_name = str_sub(occupation_name, 1, 40)) %>% 
  select(-area_code) %>% 
  arrange(desc(employment))

cleaned %>% 
  filter(
    !str_detect(area_name, "Division"),
    str_sub(area_code, 3, 7) != "00000",
    str_detect(occupation_name, "Healthcare Practitioners"),
    employment >= 0
  ) %>%
  mutate_at(vars(area_name, occupation_name), funs(str_sub(., 1, 40))) %>% 
  select(-area_code, -occupation_code) %>% 
  arrange(desc(emp_rate))
