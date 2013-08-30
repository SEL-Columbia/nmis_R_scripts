# DATA
nmis_h <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Health_774_NMIS_Facility.csv")
nmis_e <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Education_774_NMIS_Facility.csv")
nmis_w <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Water_774_NMIS_Facility.csv")
nmis_l <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/All_774_LGA.csv")

library(testthat)
# To run the test, with nmis_R_scripts as wd, type
# test_file("tests/GeneralDataChecks.R")

## HELPER FUNCTIONS
# run tests for all four datasets; separated into each line so traceback output is easy to read
foralldatasets <- function(test) { 
  test(nmis_l)
  test(nmis_h)
  test(nmis_e)
  test(nmis_w)
}

test_that("unique_lga, lga_id, etc are not NA", {
  foralldatasets(function(df) {
    expect_false(any(is.na(df$unique_lga)))
    expect_false(any(is.na(df$lga_id)))
  })    
})

test_that("lga, state, and unique_lga are standardized", {
  lgas <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv")
  foralldatasets(function(df) {
    expect_true("lga_id" %in% names(df))
    expect_true(all(c("lga", "state", "zone", "unique_lga") %in% names(df)))
    expect_true(all(df$lga %in% lgas$lga))
    expect_true(all(df$state %in% lgas$state))
    expect_true(all(df$zone %in% lgas$zone))
    expect_true(all(df$unique_lga %in% lgas$unique_lga))
  })    
})

