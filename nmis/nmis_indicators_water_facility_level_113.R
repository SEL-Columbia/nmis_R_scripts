#script for Facility level water for PIlot data from 113
source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")


w113_raw <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Water_113_999Cleaned.csv",
                     stringsAsFactors=F)
lgas <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv",
                 stringsAsFactors=F)

#removing geographic outliers
w113_raw <- subset(w113_raw, subset=(geocodeoffacility != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
w113_raw$uuid <- sapply(paste(w113_raw$geocodeoffacility, w113_raw$photo), FUN=digest)

# rename gps column to gps
w113_raw <- rename(w113_raw, c("geocodeoffacility"="gps"))

# put in uuids, and make sure there are no duplicates
w113_raw <- subset(w113_raw, !duplicated(w113_raw$uuid))
stopifnot(!anyDuplicated(w113_raw$uuid))

w113 <- merge_non_redundant(lgas, w113_raw, by="lga_id")
stopifnot(nrow(w113) == nrow(w113_raw)) #otherwise calculations below will be wrong

w113 <- subset(w113, select=c("photo", "state", "lga", "lga_id", "uuid", "gps",
                              "community", "ward", "lift_mechanism"))

## GENERAL ##
w113$water_point_type <- 
  ifelse(w113_raw$lift_mechanism %in% c('electric', 'diesel', 'solar'),
         "Tap",
  ifelse(w113_raw$lift_mechanism == "hand_pump",
         "Handpump",
  ifelse(w113_raw$water_source_type %in% c('borehole', 'tube_well'),
         "Borehole",
  ifelse(w113_raw$water_source_type == "protected_dug_well",
         "Unimproved Large Diameter Well",
  ifelse(w113_raw$water_source_type %in% c('other_protected', 'other_unprotected'),
         "Unimproved",
  ifelse(w113_raw$water_source_type == "developed_protected_spring_water",
         "Unprotected Spring",
  ifelse(w113_raw$water_source_type =="rainwater_harvesting_scheme",
         "Unimproved Rainwater Harvesting System",
  ifelse(w113_raw$water_source_type =="developed_and_treated_surface_water",
         "Untreated Surface Water",
         NA))))))))


#improved# 
w113$is_improved <- w113$water_point_type %in% c('Borehole','Handpump','Tap')

#lift mechanism#
w113$lift_mechanism <- recodeVar(w113_raw$lift_mechanism,
    c("hand_pump", "electric", "diesel", "solar", "rope_pulley", "other_nonpowered", "other_powered", "animal"),
    c("Hand Pump", "Electric", "Diesel", "Solar", "Manual", "Other", "Other", "Animal"),
    default=NA)

#functional at time of survey (y/n)# 
w113$functional <- recodeVar(w113_raw$water_source_used_today_yn, c('yes', 'no'), c('Yes', 'No'), default="Don't Know")

#cause of breakdown# 
w113$breakdown <-
  ifelse(w113_raw$water_source_used_today_yn == "yes",
      "Functional",
  ifelse(w113_raw$reasons_not_used_pump_broken  | w113_raw$reasons_not_used_lift_broken,
      "Mechanical Issue", 
  ifelse(w113_raw$reasons_not_used_no_diesel,
      "Mechanical Issue",
  ifelse(w113_raw$reasons_not_used_no_electricity,
      "Mechanical Issue",
  ifelse(w113_raw$reasons_not_used_missing_parts,
      "Mechanical Issue",
  ifelse(w113_raw$reasons_not_used_under_constreas,
      "Under Construction",
  ifelse(w113_raw$reasons_not_used_tap_broken,
      "Mechanical Issue",
  ifelse(w113_raw$reasons_not_used_bad_quality,
      "Poor Water Quality",
  ifelse(w113_raw$reasons_not_used_dry_well,
      "Dry Well",
  ifelse(w113_raw$reasons_not_used_dk,
      "Don't Know",
      NA))))))))))

#fees for use
w113$pay_for_water_yn <- recodeVar(w113_raw$pay_for_water_yn, c('yes', 'no'), c('Yes', 'No'), default="Don't Know")
  
#distribution type
w113$distribution_type <- recodeVar(w113_raw$distribution_type,
    c("single_point","multiple_points_within_100m", "multiple_points_within_1000m", "multiple_points_beyond_1000m"),
    c("Stand Alone Water Point", "Water Scheme, Source within 100m", "Water Scheme, Source within 1km","Water Scheme, Source further than 1km"),
    default=NA)

#Adding distant to every facility
#combining calculated result back to original data
###113
w113 <- lga_boudary_dist(w113, gps_col="gps")
w113_raw <- merge_non_redundant(w113, w113_raw, by="uuid")


#Delete all those have dist >= 35 km
w113_nearbypoints <- subset(w113, dist_fake <= 35 | is.na(dist_fake))
w113_raw <- subset(w113_raw, dist_fake <= 35 | is.na(dist_fake))


write.csv(x_y_killa(w113_nearbypoints), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Water_113_NMIS_Facility.csv", row.names=F)
write.csv(x_y_killa(w113_raw), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Water_113_ALL_FACILITY_INDICATORS.csv", row.names=F)
