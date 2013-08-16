setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
source("scripts/InstallFormhub.R")
source("scripts/source_scripts/NMIS_Functions.R")


w <- read.csv("in_process_data/999cleaned/Water_661_999Cleaned_Reclassified.csv")
###THE INPUT FILE WAS CREATED FROM Water_reclassify_photos.R and includes all 661 LGAs, even though only 148 were reclassified

#create smaller dataset 
w$`_id` <- w$uuid
water <- subset(w, select=c("_id", "uuid", "lga", "state", "zone", "lga_id", "gps", "formhub_photo_id", "unique_lga", "photo"))

## GENERAL ##

#type#
water$water_point_type <- 
  ifelse(w$water_point_type == "handpump",
         "Handpump",
         ifelse(w$water_point_type %in% c('ten_thousand_overhead', 'ten_thousand_ovehead'),
                "Overhead Tank (10,000)",
                ifelse(w$water_point_type == "one_thousand_overhead",
                       "Overhead Tank (1,000)",
                ifelse(w$water_point_type == "unimproved",
                       "Unimproved",
                       ifelse(w$water_point_type == "tap",
                              "Tap",
                              ifelse(w$water_point_type == "rainwater",
                                     "Rainwater Harvesting System",
                                     ifelse(w$water_scheme_type =="outlet",
                                            "Tap",
                                            ifelse(w$water_scheme_type == "dk",
                                                   "Don't Know",
                                                   ifelse(w$water_source_type == "dk",
                                                          "Don't Know",
                                                          ifelse(w$water_source_type == "borehole_tube_well",
                                                                 "Borehole",
                                                                 ifelse(w$water_source_type == "rainwater_harvesting_scheme",
                                                                        "Unimproved",
                                                                        ifelse(w$water_source_type =="dam",
                                                                               "Untreated Surface Water",
                                                                               ifelse(w$water_source_type == "developed_protected_spring_water",
                                                                                      "Unprotected Spring",
                                                                                      ifelse(w$water_source_type %in% c('protected_dug_well', 'unprotected_dug_well'),
                                                                                             "Unimproved Well",
                                                                                             NA))))))))))))))

water$Classification <-w$Classification
water$water_source_type <- w$water_source_type
water$water_scheme_type <- w$water_scheme_type

#community#
water$community <- w$community

#ward name#
water$ward <- w$ward

#improved# 
water$is_improved <-
  water$water_point_type %in% c('Tap', 'Borehole', 'Handpump', 'Overhead Tank (1,000)', 'Overhead Tank (10,000)', 'Rainwater Harvesting System')

#lift mechanism#
water$lift_mechanism <- 
  ifelse(water$water_point_type == "Handpump",
         "Hand Pump",
      ifelse(w$water_scheme_type =="outlet",
         "Tap (unknown lift)",
      ifelse(w$lift_mechanism == "hand_pump" & !(water$water_point_type %in% c('Overhead Tank (1,000)', 'Overhead Tank (5,000)', 'Tap')),
         "Hand Pump",
      ifelse(w$lift_mechanism == "manual_power",
         "Manual",
      ifelse(w$lift_mechanism == "electricity_pump",
         "Electric",
      ifelse(w$lift_mechanism == "fuel_pump",
         "Diesel",
      ifelse(w$lift_mechanism == "animal_power",
         "Animal Traction",
      ifelse(w$lift_mechanism == "solar_pump",
         "Solar",
      ifelse(w$lift_mechanism == "wind_pump",
         "Wind",
      NA)))))))))

#functional at time of survey (y/n)
water$functional <- 
  ifelse(w$water_functional_yn == "yes",
         "Yes",
      ifelse(w$water_functional_yn == "dk",
         "Don't Know",
      "No"))

#cause of breakdown
water$breakdown <-
  ifelse(w$water_functional_yn == "yes",
         "Functional",
      ifelse(w$reason_not_used == "bad_quality",
         "Poor Water Quality",
      ifelse(w$reason_not_used == "dry_well",
         "Dry Well",
      ifelse(w$reason_not_used %in% c('lift_broken', 'missing_parts','tap_broken'),
         "Mechanical Issue",
      ifelse(w$reason_not_used == "poorly_const",
         "Poorly Constructed",
      ifelse(w$reason_not_used == "under_const",
         "Under Construction",
      ifelse(w$reason_not_used == "dk",
         "Don't Know",
      NA)))))))

#fees for use
water$pay_for_water_yn <- 
  ifelse(w$pay_for_water_yn == "yes",
         "Yes",
      ifelse(w$pay_for_water_yn == "no",
         "No",
      ifelse(w$pay_for_water_yn == "dk",
         "Don't Know",
      NA)))

#distribution type
water$distribution_type <-
  ifelse(w$water_scheme_type == "water_source",
         "Stand Alone Water Point",
      ifelse(w$water_outlet_connection == "outlet_within_100m",
         "Water Scheme, Source within 100m",
      ifelse(w$water_outlet_connection == "outlet_btw_100m_1km",
         "Water Scheme, Source within 1km",
      ifelse(w$water_outlet_connection == "outlet_more_1km",
         "Water Scheme, Source further than 1km",
      NA))))

write.csv(water, "in_process_data/nmis/data_661/Water_661_NMIS_Facility.csv", row.names=F)
water <- subset(water, select=c('water_point_type', 'is_improved', 'functional', 'lift_mechanism', 'breakdown', 'pay_for_water_yn','distribution_type'))
w$water_point_type <- NULL
w$pay_for_water_yn <- NULL
w_nmis <- cbind(water,w)


write.csv(boundary_clean(w_nmis, "mylga_state", gps_col="gps"), "in_process_data/nmis/data_661/Water_661_ALL_FACILITY_INDICATORS.csv", row.names=F)

