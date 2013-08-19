source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")

#script for Facility level water for PIlot data

# wpilot_raw <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Pilot_Water_cleaned_2011Aug29.csv")
# lgas <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv")
# num_zone <- read.csv("Zone_Nums.csv")
# lgas <- merge(lgas, num_zone, by="zone")
# wpilot <- merge(wpilot_raw,lgas, by="lga_id", all.x = T)
# wpilot <- rename(wpilot, c("lga.x"="lga", "state.x"="state", "number_zone"="num_zone"))
# wpilot$lga.y <- NULL
# wpilot$state.y <- NULL

wpilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Water_pilot_outliercleaned.csv")

wpilot <- subset(wpilot, subset=(geocodeoffacility != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
wpilot$uuid <- sapply(paste(wpilot$geocodeoffacility, wpilot$photo), FUN=digest)
wpilot <- subset(wpilot, !duplicated(wpilot$uuid))
wpilot <- boundary_clean(wpilot, "state", "geocodeoffacility")

# OUTPUT SHOULD BE 0
anyDuplicated(wpilot$uuid)


wp <- subset(wpilot, select=c("photo", "state", "lga", "lga_id", "uuid"))

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

#community#
wp$community <- wpilot$community

#ward name#
wp$ward <- wpilot$ward

#improved# 
wp$is_improved <-
  wpilot$water_source_type %in% c('borehole')

#lift mechanism#
wp$lift_mechanism <- 
        ifelse(wpilot$lift_mechanism == "hand_pump",
                       "Hand Pump",
                       ifelse(wpilot$lift_mechanism == "electric_motor_pump",
                                     "Electric",
                                     ifelse(wpilot$lift_mechanism == "diesel_pump",
                                            "Diesel",
                                            ifelse(wpilot$lift_mechanism == "solar_pump",
                                                          "Solar",
                                                          ifelse(wpilot$lift_mechanism == "rope_and_pulley",
                                                                 "Manual",
                                                                 NA)))))

#functional at time of survey (y/n)# 
wp$functional <- 
  ifelse(wpilot$water_source_used_today_yn == "yes",
         "Yes",
         "No")

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
wp$pay_for_water_yn <- 
  ifelse(wpilot$pay_for_water_yn == "yes",
         "Yes",
         "No")

#distribution type
wp$distribution_type <-
  ifelse(wpilot$distribution_type == "single_point",
         "Stand Alone Water Point",
         ifelse(wpilot$distribution_type %in% c('multiple_points_100m_reticulation','multiple_points_within_100m'),
                "Water Scheme, Source within 100m",
                ifelse(wpilot$distribution_type %in% c('multiple_points_ < 1km_reticulation','multiple_points_within_1000m'),
                       "Water Scheme, Source within 1km",
                       ifelse(wpilot$distribution_type %in% c('multiple_points_beyond_1000m','multiple_points_ > 1km_reticulation'),
                              "Water Scheme, Source further than 1km",
                              NA))))

write.csv(wp, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Water_pilot_NMIS_Facility.csv", row.names=F)
write.csv(cbind(wp, wpilot), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Water_pilot_ALL_FACILITY_INDICATORS.csv", row.names=F)

