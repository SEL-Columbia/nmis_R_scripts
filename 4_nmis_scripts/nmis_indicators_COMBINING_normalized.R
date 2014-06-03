# source("source_scripts/NMIS_Utils.R")
source("source_scripts/NMIS_Functions.R")

edu_774_lga <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/Education_LGA_level_774.rds")
water_774_lga <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/Water_LGA_level_774.rds")
health_774_lga <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/Health_LGA_level_774.rds")
external <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external_data/output_data/external_data.rds")

lgas <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv")

combined <- merge(external, edu_774_lga, by="lga_id", all=T)
combined <- merge(combined, water_774_lga, by="lga_id", all=T)
combined <- merge(combined, health_774_lga, by="lga_id", all=T)
combined <- merge_non_redundant(lgas, combined, by="lga_id")

saveRDS(combined, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/All_774_LGA.rds")