## code to prepare `country_synonyms` dataset goes here


library(readr)
library(dplyr)
library(stringr)

country_synonyms <- read_csv("data-raw/synonyms.csv", locale = readr::locale(encoding = "UTF-8")) %>%
  mutate(across(where(is.character), ~str_replace_all(.x, "\\s+", " ")))


usethis::use_data(country_synonyms, overwrite = TRUE)
