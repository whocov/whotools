## code to prepare `ref_country` dataset goes here


library(readr)
library(dplyr)
library(stringr)

official_pal <- read_csv("data-raw/official_pal.csv", locale = readr::locale(encoding = "UTF-8"))



usethis::use_data(official_pal, overwrite = TRUE)
