##combining 661/113/pilot facility level scripts##
library(plyr)
source("source_scripts/NMIS_Utils.R")
source("source_scripts/NMIS_Functions.R")

######################################################################################
##EDUCATION###########################################################################
######################################################################################

#reading in data
six61 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Education_661_NMIS_Facility.csv", stringsAsFactors=FALSE)
one13 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Education_113_NMIS_Facility.csv", stringsAsFactors=FALSE)
pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Education_Pilot_NMIS_Facility.csv", stringsAsFactors=FALSE)
#subnm <- names(one13)[which(!names(one13) %in% names(six61))]

#combining facility data
combined_113p <- rbind.fill(one13, pilot)
combined_774 <- rbind.fill(six61, combined_113p)
combined_774$sector <- "education"
write.csv(combined_774, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Education_774_NMIS_Facility.csv", row.names=F)

######################################################################################
##HEALTH##############################################################################
######################################################################################
#reading in data
six61 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Health_661_NMIS_Facility.csv", stringsAsFactors=FALSE)
one13 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Health_113_NMIS_Facility.csv", stringsAsFactors=FALSE)
pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Health_Pilot_NMIS_Facility.csv", stringsAsFactors=FALSE)
#subnm <- names(six61)[which(!names(six61) %in% names(pilot))]
##adding unique_id##
one13 <- add_lga_id(one13)
pilot <- add_lga_id(pilot)

#combining facility data
combined_113p <- rbind.fill(one13, pilot)
combined_774 <- rbind.fill(six61, combined_113p)
combined_774$sector <- "health"
write.csv(combined_774, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Health_774_NMIS_Facility.csv", row.names=F)

######################################################################################
##WATER###############################################################################
######################################################################################
#reading in data
six61 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Water_661_NMIS_Facility.csv", stringsAsFactors=FALSE)
one13 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Water_113_NMIS_Facility.csv", stringsAsFactors=FALSE)
pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Water_pilot_NMIS_Facility.csv", stringsAsFactors=FALSE)
#subnm <- names(one13)[which(!names(one13) %in% names(six61))]

##adding unique_id##
one13 <- subset(one13, one13$lga != "N/A")
one13 <- add_lga_id(one13, lgacolname="lga", statecolname="state")
pilot <- add_lga_id(pilot, lgacolname="lga", statecolname="state")

#combining facility data
combined_113p <- rbind.fill(one13, pilot)
combined_774 <- rbind.fill(six61, combined_113p)
combined_774$sector <- "water"
write.csv(combined_774, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Water_774_NMIS_Facility.csv", row.names=F)

######################################################################################
##ALL LGA level#######################################################################
######################################################################################

##education##
lga_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Education_LGA_level_661.csv", stringsAsFactors=FALSE)
lga_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Education_LGA_level_113.csv", stringsAsFactors=FALSE)
lga_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Education_LGA_level_pilot.csv", stringsAsFactors=FALSE)

combined_113p_lga <- rbind.fill(lga_113, lga_pilot)
combined_774_lga_EDU <- rbind.fill(combined_113p_lga, lga_661)
write.csv(combined_774_lga_EDU, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Education_774_NMIS_LGA.csv", row.names=F)

##health##
lga_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Health_LGA_level_661.csv", stringsAsFactors=FALSE)
lga_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Health_LGA_level_113.csv", stringsAsFactors=FALSE)
lga_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Health_LGA_level_pilot.csv", stringsAsFactors=FALSE)

combined_113p_lga <- rbind.fill(lga_113, lga_pilot)
combined_774_lga_H <- rbind.fill(combined_113p_lga, lga_661)
write.csv(combined_774_lga_H, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Health_774_NMIS_LGA.csv", row.names=F)

##water##
lga_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Water_LGA_level_661.csv", stringsAsFactors=FALSE)
lga_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Water_LGA_level_113.csv", stringsAsFactors=FALSE)
lga_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Water_LGA_level_pilot.csv", stringsAsFactors=FALSE)

combined_113p_lga <- rbind.fill(lga_113, lga_pilot)
combined_774_lga_WATER <- rbind.fill(combined_113p_lga, lga_661)
write.csv(combined_774_lga_WATER, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Water_774_NMIS_LGA.csv", row.names=F)

#external data
external <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external_data/output_data/external_data.csv", stringsAsFactors=F)

#combining all
edu_external <- merge(combined_774_lga_EDU, external, by.x="lga_id", by.y="LGA_id", all.y=T)
health_ed_ex <- merge(edu_external, combined_774_lga_H, by="lga_id", all.x=T) 
water_h_ed_ex <- merge(health_ed_ex, combined_774_lga_WATER, by="lga_id", all.x=T)
#organizing
#TODO: wtf is this line?
#water_h_ed_ex <- water_h_ed_ex[,c(1:96,100:147,151:156,158:160,162:165,167:177)]
#selcol <- grep(".x$|.y$",colnames(water_h_ed_ex))
#water_h_ed_ex <- water_h_ed_ex[-c(selcol)]
write.csv(x_y_killa(water_h_ed_ex), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/All_774_LGA.csv", row.names=F)
