#script for Facility level water for PIlot data from 113
#setwd("C:/Users/Brett/Dropbox/Nigeria 661 Baseline Data Cleaning/")
setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
source("scripts/InstallFormhub.R")
source("scripts/source_scripts/NMIS_Utils.R")


w113_raw <- read.csv("in_process_data/999cleaned/Water_113_999Cleaned.csv")
lgas <- read.csv("lgas.csv")
num_zone <- read.csv("Zone_Nums.csv")

#removing geographic outliers
w113_raw <- boundary_clean(w113_raw, "state", "geocodeoffacility")#
w113_raw <- subset(w113_raw, subset=(geocodeoffacility != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
w113_raw$uuid <- sapply(paste(w113_raw$geocodeoffacility, w113_raw$photo), FUN=digest)
w113_raw <- subset(w113_raw, !duplicated(w113_raw$uuid))
# OUTPUT SHOULD BE 0
anyDuplicated(w113_raw$uuid)

lgas <- merge(lgas, num_zone, by="zone")
w113 <- merge(w113_raw,lgas, by="lga_id", all.x = T)
w113 <- rename(w113, c("lga.y"="lga", "state.y"="state", "zone.y"="zone", "number_zone.y"="num_zone"))
w113$lga.x <- NULL
w113$state.x <- NULL
w113$zone.x <- NULL

w113 <- subset(w113, select=c("photo", "state", "lga", "lga_id", "uuid"))

## GENERAL ##
#type#
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

#community#
w113$community <- w113_raw$community

#ward name#
w113$ward <- w113_raw$ward

#improved# 
w113$is_improved <-
  w113$water_point_type %in% c('Borehole','Handpump','Tap')

#lift mechanism#
w113$lift_mechanism <- 
  ifelse(w113_raw$lift_mechanism == "hand_pump",
         "Hand Pump",
         ifelse(w113_raw$lift_mechanism == "electric",
                "Electric",
                ifelse(w113_raw$lift_mechanism == "diesel",
                       "Diesel",
                       ifelse(w113_raw$lift_mechanism == "solar",
                              "Solar",
                              ifelse(w113_raw$lift_mechanism == "rope_pulley",
                                     "Manual",
                                     ifelse(w113_raw$lift_mechanism %in% c('other_nonpowered','other_powered'),
                                            "Other",
                                            ifelse(w113_raw$lift_mechanism == "animal",
                                                  "Animal Traction",
                                     NA)))))))

#functional at time of survey (y/n)# 
w113$functional <- 
  ifelse(w113_raw$water_source_used_today_yn == "yes",
         "Yes",
         ifelse(w113_raw$water_source_used_today_yn == "no",
                "No",
         "Don't Know"))

#cause of breakdown# 
w113$breakdown <-
  ifelse(w113_raw$water_source_used_today_yn == "yes",
         "Functional",
         ifelse(w113_raw$reasons_not_used_pump_broken == TRUE | w113_raw$reasons_not_used_lift_broken == TRUE,
                "Mechanical Issue", 
                ifelse(w113_raw$reasons_not_used_no_diesel == TRUE,
                       "Mechanical Issue",
                       ifelse(w113_raw$reasons_not_used_no_electricity == TRUE,
                              "Mechanical Issue",
                              ifelse(w113_raw$reasons_not_used_missing_parts == TRUE,
                                     "Mechanical Issue",
                                     ifelse(w113_raw$reasons_not_used_under_constreas == TRUE,
                                            "Under Construction",
                                            ifelse(w113_raw$reasons_not_used_tap_broken == TRUE,
                                                   "Mechanical Issue",
                                                   ifelse(w113_raw$reasons_not_used_bad_quality == TRUE,
                                                          "Poor Water Quality",
                                                          ifelse(w113_raw$reasons_not_used_dry_well == TRUE,
                                                                 "Dry Well",
                                                                 ifelse(w113_raw$reasons_not_used_dk == TRUE,
                                                                        "Don't Know",
                                                                        NA))))))))))

#fees for use
w113$pay_for_water_yn <- 
  ifelse(w113_raw$pay_for_water_yn == "yes",
         "Yes",
         ifelse(w113_raw$pay_for_water_yn == "no",
                "No",
                "Don't Know"))

#distribution type
w113$distribution_type <-
  ifelse(w113_raw$distribution_type == "single_point",
         "Stand Alone Water Point",
         ifelse(w113_raw$distribution_type == "multiple_points_within_100m",
                "Water Scheme, Source within 100m",
                ifelse(w113_raw$distribution_type == "multiple_points_within_1000m",
                       "Water Scheme, Source within 1km",
                       ifelse(w113_raw$distribution_type == "multiple_points_beyond_1000m",
                              "Water Scheme, Source further than 1km",
         NA))))

write.csv(w113, "in_process_data/nmis/Water_113_NMIS_Facility.csv", row.names=F)
write.csv(cbind(w113, w113_raw), "in_process_data/nmis/Water_113_ALL_FACILITY_INDICATORS.csv", row.names=F)
