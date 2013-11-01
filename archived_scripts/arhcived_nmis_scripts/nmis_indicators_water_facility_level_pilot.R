source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")

#script for Facility level water for PIlot data

wpilot <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Water_pilot_outliercleaned.rds")

wpilot <- subset(wpilot, subset=(geocodeoffacility != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
# TODO: uuid is already there; why?
wpilot$uuid <- sapply(paste(wpilot$geocodeoffacility, wpilot$photo), FUN=digest)
stopifnot(!anyDuplicated(wpilot$uuid))

wp <- subset(wpilot, select=c("photo", "state", "lga", "lga_id", "uuid", "geocodeoffacility",
                              "community", "ward"))
wp <- rename(wp, c("geocodeoffacility"="gps"))

## GENERAL ##
####type note that 210 NAs on water source type could be fixed by hand with photo verification
wp$water_point_type <- 
  ifelse(wpilot$lift_mechanism %in% c('electric_motor_pump', 'diesel_pump', 'solar_pump'),
         "Tap",
  ifelse(wpilot$lift_mechanism == "hand_pump",
         "Handpump",
  ifelse(wpilot$water_source_type %in% c('borehole', 'tube_well'),
         "Borehole",
  ifelse(wpilot$water_source_type == "dug_well",
         "Unimproved Large Diameter Well",
  ifelse(wpilot$water_source_type %in% c('n/a', 'other_protected', 'other_unprotected'),
         "Unimproved",
  ifelse(wpilot$water_source_type =="rainwater",
         "Unimproved Rainwater Harvesting System",
  ifelse(wpilot$water_source_type =="surface_water",
         "Untreated Surface Water",
         NA)))))))

wp$is_improved <- wp$water_point_type %in% c('Borehole','Handpump','Tap')

#lift mechanism#
wp$lift_mechanism <- recodeVar(wpilot$lift_mechanism,
    c("hand_pump", "electric_motor_pump", "diesel_pump", "solar_pump", "rope_and_pulley", "other_nonpowered"),
    c("Hand Pump", "Electric", "Diesel", "Solar", "Manual", "Other"),
    default=NA)

#functional at time of survey (y/n)# 
wp$functional <- recodeVar(wpilot$water_source_used_today_yn, c('yes', 'no'), c('Yes', 'No'), default="Don't Know")

#cause of breakdown# 
wp$breakdown <-
  ifelse(wpilot$water_source_used_today_yn == "yes",
         "Functional",
  ifelse(wpilot$reasons_not_used_pump_broken == TRUE | wpilot$reasons_not_used_lift_broken == TRUE,
    "Mechanical Issue", 
  ifelse(wpilot$reasons_not_used_no_diesel == TRUE,
    "Mechanical Issue",
  ifelse(wpilot$reasons_not_used_no_electricity == TRUE,
    "Mechanical Issue",
  ifelse(wpilot$reasons_not_used_missing_parts == TRUE,
    "Mechanical Issue",
  ifelse(wpilot$reasons_not_used_under_constreas == TRUE,
    "Under Construction",
  ifelse(wpilot$reasons_not_used_tap_broken == TRUE,
    "Mechanical Issue",
  ifelse(wpilot$reasons_not_used_bad_quality == TRUE,
    "Poor Water Quality",
  ifelse(wpilot$reasons_not_used_dry_well == TRUE,
    "Dry Well",
  ifelse(wpilot$reasons_not_used_dk == TRUE,
    "Don't Know",
    NA))))))))))

  
#fees for use
wp$pay_for_water_yn <- recodeVar(wpilot$pay_for_water_yn, c('yes', 'no'), c('Yes', 'No'), default="Don't Know")

#distribution type
wp$distribution_type <- recodeVar(wpilot$distribution_type,
    c("single_point","multiple_points_within_100m", "multiple_points_within_1000m", 
      "multiple_points_beyond_1000m", "multiple_points_ < 1km_reticulation", "multiple_points_ > 1km_reticulation"),
    c("Stand Alone Water Point", "Water Scheme, Source within 100m", "Water Scheme, Source within 1km",
      "Water Scheme, Source further than 1km", "Water Scheme, Source within 100m","Water Scheme, Source further than 1km"),
    default=NA)

#Adding distant to every facility
#combining calculated result back to original data
###113
wp <- lga_boudary_dist(wp, gps_col="gps")
wpilot <- merge_non_redundant(wp, wpilot, by="uuid")

#Delete all those have dist >= 35 km
water_p_nearby <- subset(wp, dist_fake <= 35 | is.na(dist_fake))
wpilot <- subset(wpilot, dist_fake <= 35 | is.na(dist_fake))


saveRDS(x_y_killa(water_p_nearby), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Water_pilot_NMIS_Facility.rds")
saveRDS(x_y_killa(wpilot), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Water_pilot_ALL_FACILITY_INDICATORS.rds")

