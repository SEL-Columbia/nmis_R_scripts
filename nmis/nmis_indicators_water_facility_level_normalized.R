#script for Facility level water for PIlot data from 113
source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")

water_774 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Normalized/Water_774_normalized_999clean.rds")
lgas <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv")

#removing geographic outliers
water_774 <- subset(water_774, subset=(gps != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
water_774$uuid <- sapply(paste(water_774$gps, water_774$photo), FUN=digest)

# put in uuids, and make sure there are no duplicates
water_774 <- subset(water_774, !duplicated(water_774$uuid))
stopifnot(!anyDuplicated(water_774$uuid))

w113 <- merge_non_redundant(lgas, water_774, by="lga_id")
stopifnot(nrow(w113) == nrow(water_774)) #otherwise calculations below will be wrong

w113 <- subset(w113, select=c("photo", "state", "lga", "lga_id", "uuid", "gps",
                              "community", "ward", "lift_mechanism"))

## GENERAL ##
w113$water_point_type <- 
  ifelse(water_774$lift_mechanism %in% c('electric', 'diesel', 'solar'),
         "Tap",
  ifelse(water_774$lift_mechanism == "hand_pump",
         "Handpump",
  ifelse(water_774$water_source_type %in% c('borehole', 'tube_well'),
         "Borehole",
  ifelse(water_774$water_source_type == "protected_dug_well",
         "Unimproved Large Diameter Well",
  ifelse(water_774$water_source_type %in% c('other_protected', 'other_unprotected'),
         "Unimproved",
  ifelse(water_774$water_source_type == "developed_protected_spring_water",
         "Unprotected Spring",
  ifelse(water_774$water_source_type =="rainwater_harvesting_scheme",
         "Unimproved Rainwater Harvesting System",
  ifelse(water_774$water_source_type =="developed_and_treated_surface_water",
         "Untreated Surface Water",
         NA))))))))


#improved# 
w113$is_improved <- w113$water_point_type %in% c('Borehole','Handpump','Tap',
                                                 'Overhead Tank (1,000)', 'Overhead Tank (10,000)', 
                                                 'Rainwater Harvesting System')
#lift mechanism#
w113$lift_mechanism <- recodeVar(water_774$lift_mechanism,
                                 c("hand_pump", "electricity_pump", "fuel_pump", "solar_pump", 
                                   "rope_pulley", "other_nonpowered", "other_powered", "animal_power",
                                   "manual_power", "outlet","wind_pump"),
                                 c("Hand Pump", "Electric", "Diesel", "Solar", 
                                   "Manual", "Other", "Other", "Animal",
                                   "Manual", "Tap (unknown lift)","Wind"),
                                 default=NA)




#functional at time of survey (y/n)# 
w113$functional <- recodeVar(water_774$water_functional_yn, 
                            c('yes', 'no'), 
                            c('Yes', 'No'), 
                            default="Don't Know")

#cause of breakdown# 
water$breakdown <-
      ifelse(w$water_functional_yn == "yes",
         "Functional",
      ifelse(w$reason_not_used == "bad_quality",
         "Poor Water Quality",
      ifelse(w$reason_not_used == "dry_well",
         "Dry Well",
      ifelse(w$reason_not_used %in% c('pump_broken', 'no_diesel', 'no_electricity',
                                      'lift_broken', 'missing_parts','tap_broken'),
         "Mechanical Issue",
      ifelse(w$reason_not_used == "poorly_const",
         "Poorly Constructed",
      ifelse(w$reason_not_used == "under_const",
         "Under Construction",
      ifelse(w$reason_not_used == "dk",
         "Don't Know",
      ifelse(w$reason_not_used == "other",
         "Other",
      NA))))))))

#fees for use
w113$pay_for_water_yn <- recodeVar(water_774$pay_for_water_yn, 
                                   c('yes', 'no', 'dk', 'do_not_know'), 
                                   c('Yes', 'No', "Don't Know", "Don't Know"))
  
#distribution type
w113$distribution_type <- recodeVar(water_774$distribution_type,
    c("single_point","multiple_points_within_100m", "multiple_points_within_1000m", 
      "multiple_points_beyond_1000m", "multiple_points_ < 1km_reticulation", "multiple_points_ > 1km_reticulation"),
    c("Stand Alone Water Point", "Water Scheme, Source within 100m", "Water Scheme, Source within 1km",
      "Water Scheme, Source further than 1km", "Water Scheme, Source within 100m","Water Scheme, Source further than 1km"))

#Adding distant to every facility
#combining calculated result back to original data
###113
w113 <- lga_boudary_dist(w113, gps_col="gps")
water_774 <- merge_non_redundant(w113, water_774, by="uuid")


#Delete all those have dist >= 35 km
w113_nearbypoints <- subset(w113, dist_fake <= 35 | is.na(dist_fake))
water_774 <- subset(water_774, dist_fake <= 35 | is.na(dist_fake))


saveRDS(x_y_killa(w113_nearbypoints), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Water_113_NMIS_Facility.rds")
saveRDS(x_y_killa(water_774), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Water_113_ALL_FACILITY_INDICATORS.rds")
