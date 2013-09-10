nmis_R_scripts
==============

R scripts for NMIS

Style Guide
====

Sourcing files
-----

We expect that you run code in this directory while being on the main (ie, nmis_R_scripts) folder. As a result, all `source` calls should be relative (example: `source("source_scripts/NMIS_Functions.R")` and all data reads should be absolute (example: `read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Health_774_NMIS_Facility.csv")`).
