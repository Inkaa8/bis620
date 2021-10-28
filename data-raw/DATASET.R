## code to prepare `DATASET` dataset goes here
library(haven)
library(dplyr)

pm_ae <- read_sas("NCT00339183/ae.sas7bdat") %>%
  select(SUBJID, AESTDYI, AESOC) %>%
  mutate(
    AESTDYI = sample(AESTDYI),
    AESOC = sample(AESOC)
  )

usethis::use_data(pm_ae, overwrite = TRUE)



rawdata <- read_sas("data-raw/vital_data_2019.sas7bdat")

usethis::use_data(rawdata, overwrite = TRUE)
