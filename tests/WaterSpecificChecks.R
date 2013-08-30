# DATA
nmis_w <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Water_774_NMIS_Facility.csv")
nmis_l <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/All_774_LGA.csv")

library(testthat)
# To run the test, with nmis_R_scripts as wd, type
# test_file("tests/GeneralDataChecks.R")

test_that("unimproved + improved = total", {
  expect_equal(nmis_l$num_improved_water_points + nmis_l$num_unimproved_points, nmis_l$num_total_water_points)
})
