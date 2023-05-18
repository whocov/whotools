## code to prepare `ref_country` dataset goes here


library(readr)

ref_country <- read_csv("data-raw/country_ref.csv")


usethis::use_data(ref_country, overwrite = TRUE)
