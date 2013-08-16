setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
source("scripts/source_scripts/NMIS_Utils.R")

#education
e <- read.csv('in_process_data/nmis/data_113/Education_113_ALL_FACILITY_INDICATORS.csv', stringsAsFactors=F)
e2 <- facility_update(e, edu_bool=T, facility_name_col="school_name")
write.csv(e2, 'in_process_data/nmis/data_113/Education_113_ALL_FACILITY_INDICATORS.csv', row.names=F)


e <- read.csv('in_process_data/nmis/data_661/Education_661_ALL_FACILITY_INDICATORS.csv', stringsAsFactors=F)
e2 <- facility_update(e, edu_bool=T, facility_name_col="school_name")
write.csv(e2, 'in_process_data/nmis/data_661/Education_661_ALL_FACILITY_INDICATORS.csv', row.names=F)

e <- read.csv('in_process_data/nmis/data_pilot/Education_Pilot_ALL_FACILITY_INDICATORS.csv', stringsAsFactors=F)
e2 <- facility_update(e, edu_bool=T, facility_name_col="school_name")
write.csv(e2, 'in_process_data/nmis/data_pilot/Education_Pilot_ALL_FACILITY_INDICATORS.csv', row.names=F)

#Health
h <- read.csv('in_process_data/nmis/data_113/Health_113_ALL_FACILITY_INDICATORS.csv', stringsAsFactors=F)
h2 <- facility_update(h, edu_bool=F, facility_name_col="facility_name")
write.csv(h2, 'in_process_data/nmis/data_113/Health_113_ALL_FACILITY_INDICATORS.csv', row.names=F)


h <- read.csv('in_process_data/nmis/data_661/Health_661_ALL_FACILITY_INDICATORS.csv', stringsAsFactors=F)
h2 <- facility_update(h, edu_bool=F, facility_name_col="facility_name")
write.csv(h2, 'in_process_data/nmis/data_661/Health_661_ALL_FACILITY_INDICATORS.csv', row.names=F)

h <- read.csv('in_process_data/nmis/data_pilot/Health_Pilot_ALL_FACILITY_INDICATORS.csv', stringsAsFactors=F)
h2 <- facility_update(h, edu_bool=F, facility_name_col="facility_name")
write.csv(h2, 'in_process_data/nmis/data_pilot/Health_Pilot_ALL_FACILITY_INDICATORS.csv', row.names=F)

