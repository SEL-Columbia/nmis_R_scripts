# DATA
nmis_l <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/All_774_LGA.csv")

library(testthat)
library(stringr)
library(plyr)
source("source_scripts/NMIS_Functions.R")
# To run the test, with nmis_R_scripts as wd, type
# test_file("tests/LGALevelTests.R")

test_that("percent indicators are between 0 and 1", {
  proportion_indicators <- names(nmis_l)[str_detect(names(nmis_l), "percent|proportion")]
  expect_false(any(nmis_l[,proportion_indicators] < 0, na.rm=T))
  expect_false(any(nmis_l[,proportion_indicators] > 1, na.rm=T))
  # d = colwise(function(x) { any(x, na.rm=T) })(data.frame(nmis_l[,proportion_indicators] > 1))
  # print(paste(names(d)[d == T], collapse=' '))
})


test_that("no indicator should be NA for more than 100 LGAs (no hard failure)", {
  na_counts <- colwise(function(x) icount(is.na(x)))(nmis_l)
  high_na_counts <- na_counts[,na_counts>100]
  if(nrow(high_na_counts) > 0) {
    print("The following indicators have high NA counts:")
    print(high_na_counts) 
  }
  expect_true(T)
})
