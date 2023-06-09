## code to prepare `country_synonyms` dataset goes here


library(readr)

country_synonyms <- read_csv("data-raw/synonyms.csv")


usethis::use_data(country_synonyms, overwrite = TRUE)
