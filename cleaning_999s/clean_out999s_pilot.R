#PILOT: 999 cleaning
#Directed by Salah Chafik - July 24th 2013
#Wrote by Jang Hyun Kim   - from July 24th 2013
source('./cleaning_999s/999_functions.R')
####################
########health######
####################
h_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Data_Health_Clean_2011.11.18.csv",                    
                    na.strings = c('NA', 'n/a'), stringsAsFactors=F)
h_pilot <- subset(h_pilot, subset=!is.na(geocodeoffacility)) # REMOVING ALL FACILITIES WITHOUT GEO CODE
h_pilot$uuid <- sapply(paste(h_pilot$geocodeoffacility, h_pilot$photo), FUN=digest)
h_pilot <- subset(h_pilot, !duplicated(h_pilot$uuid))

# OUTPUT SHOULD BE 0
anyDuplicated(h_pilot$uuid)
h <- h_pilot

#Cleaning
h$num_toilets_improved_p <- as.numeric(h$num_toilets_improved_p)

h$num_toilets_notimproved_p <- as.numeric(h$num_toilets_notimproved_p)

h$inpatient_care_num_beds <- as.numeric(h$inpatient_care_num_beds)

write.csv(h, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Health_pilot_999Cleaned.csv", row.names=F)


#########################
########Education########
#########################
e_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Education_cleaned_2011Nov17.csv",
                  stringsAsFactors=F, na.strings = c("NA", "n/a"))
e_pilot <- subset(e_pilot, subset=!is.na(gps)) # REMOVING ALL FACILITIES WITHOUT GEO CODE
e_pilot$uuid <- sapply(paste(e_pilot$gps, e_pilot$photo), FUN=digest)

# OUTPUT SHOULD BE 0
anyDuplicated(e_pilot$uuid)
e <- e_pilot

#Cleaning
#sanitation_facilities_total -> there is 2 entry of 1000, but didn't elminiated because it is seemed feasible

#num_tchrs_female_full_time -> 400 not eliminated even second biggest was 57 because it is seemed feasible
#num_tchrs_male_full_time   -> 400 not eliminated even second biggest was 54 because it is seemed feasible     

#tchrs_quarters_sufficient_yn -> varible should be yes or no question, but record in 1 and 2
#by discretion, choose yes for 1 and no for 2

# e$tchrs_quarters_sufficient_yn[e$tchrs_quarters_sufficient_yn == 1] <- "yes"
# e$tchrs_quarters_sufficient_yn[e$tchrs_quarters_sufficient_yn == 2] <- "no"

#X_p_budget_spending <- data is in range format

write.csv(e, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Education_pilot_999Cleaned.csv", row.names=F)


################
#####Water######
################
w_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Water_cleaned_2011Aug29.csv",
                  stringsAsFactors=F, na.strings = c("NA", "n/a"))
w_pilot$uuid <- sapply(paste(w_pilot$gps, w_pilot$photo), FUN=digest)

# OUTPUT SHOULD BE 0
anyDuplicated(w_pilot$uuid)

write.csv(w_pilot, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Water_pilot_999Cleaned.csv", row.names=F)


