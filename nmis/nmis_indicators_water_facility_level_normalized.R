#script for Facility level water for PIlot data from 113
source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")

water_774 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Normalized/Water_774_normalized.rds")
water_774 <- rename(water_774, c("pop_2006" = "Population"))

# put in uuids, and make sure there are no duplicates
stopifnot(!anyDuplicated(water_774$uuid) | any(is.na(water_774$lga_id)))

water_sub <- subset(water_774, select=c("photo", "state", "lga", "lga_id", "uuid", "gps",
                                        "community", "ward", "lift_mechanism", "water_point_type",
                                        "water_functional_yn", "pay_for_water_yn", "src" ))

water_sub <- rename(water_sub, 
                            c("photo" = 'formhub_photo_id',
                              "water_functional_yn" = "functional"))

stopifnot(nrow(water_sub) == nrow(water_774)) #otherwise calculations below will be wrong

## GENERAL ##
#improved# 
water_sub$is_improved <- water_sub$water_point_type %in% c('Borehole','Handpump','Tap',"Overhead Tank (10,000)",
                                                 'Overhead Tank (1,000)','Rainwater Harvesting System')
#lift mechanism#
water_sub$lift_mechanism <- recodeVar(water_774$lift_mechanism,
                                 c("hand_pump", "electricity_pump", "fuel_pump", "solar_pump", 
                                   "rope_pulley", "other_nonpowered", "other_powered", "animal_power",
                                   "manual_power", "outlet","wind_pump"),
                                 c("Hand Pump", "Electric", "Diesel", "Solar", 
                                   "Manual", "Other", "Other", "Animal",
                                   "Manual", "Tap (unknown lift)","Wind"),
                                 default=NA)

#cause of breakdown# 
water_sub$breakdown <-
      ifelse(water_774$water_functional_yn == "yes",
         "Functional",
      ifelse(water_774$reason_not_used == "bad_quality",
         "Poor Water Quality",
      ifelse(water_774$reason_not_used == "dry_well",
         "Dry Well",
      ifelse(water_774$reason_not_used %in% c('pump_broken', 'no_diesel', 'no_electricity',
                                              'lift_broken', 'missing_parts','tap_broken'),
         "Mechanical Issue",
      ifelse(water_774$reason_not_used == "poorly_const",
         "Poorly Constructed",
      ifelse(water_774$reason_not_used == "under_const",
         "Under Construction",
      ifelse(water_774$reason_not_used == "dk",
         "Don't Know",
      ifelse(water_774$reason_not_used == "other",
         "Other",
      NA))))))))

#distribution type
water_sub$distribution_type <- recodeVar(water_774$distribution_type,
    c("single_point","multiple_points_within_100m", "multiple_points_within_1000m", 
      "multiple_points_beyond_1000m", "multiple_points_ < 1km_reticulation", "multiple_points_ > 1km_reticulation"),
    c("Stand Alone Water Point", "Water Scheme, Source within 100m", "Water Scheme, Source within 1km",
      "Water Scheme, Source further than 1km", "Water Scheme, Source within 100m","Water Scheme, Source further than 1km"))

#Adding distant to every facility
#combining calculated result back to original data
###113
water_sub <- lga_boudary_dist(water_sub, gps_col="gps")
water_sub$sector <- "water"
water_sub$facility_name <- "Water Point"

water_774 <- merge_non_redundant(water_sub, water_774, by="uuid")


#Delete all those have dist >= 35 km
water_sub <- subset(water_sub, dist_fake <= 35 | is.na(dist_fake))
water_774 <- subset(water_774, dist_fake <= 35 | is.na(dist_fake))


saveRDS(water_sub, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Water_774_NMIS_Facility.rds")
saveRDS(water_774, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Water_774_ALL_FACILITY_INDICATORS.rds")
