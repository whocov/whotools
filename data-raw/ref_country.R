## code to prepare `ref_country` dataset goes here


library(readr)
library(dplyr)
library(stringr)

ref_country <- read_csv("data-raw/country_ref.csv", locale = readr::locale(encoding = "UTF-8")) %>%
  mutate(across(where(is.character), ~str_replace_all(.x, "\\s+", " ")))



usethis::use_data(ref_country, overwrite = TRUE)
