#script for Facility level water for PIlot data from 113
source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")

require(dplyr)

water_774 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Normalized/Water_774_normalized.rds")
# put in uuids, and make sure there are no duplicates
stopifnot(!anyDuplicated(water_774$uuid) | any(is.na(water_774$lga_id)))

water_774 <- water_774 %.%
    dplyr::select(Population = pop_2006,
                  formhub_photo_id = photo,
                  functional = water_functional_yn,
                  date_of_survey = start,
                  matches('.')) %.%
    dplyr::mutate(
        date_of_survey = as.character(as.Date(date_of_survey)),
        
        #improved# 
        is_improved = water_point_type %in% c('Borehole','Handpump','Tap','Overhead Tank',
                                                                   'Rainwater Harvesting System'),
        #lift mechanism#
        lift_mechanism = recodeVar(lift_mechanism,
                                              c("hand_pump", "electricity_pump", "fuel_pump", "solar_pump", 
                                                "rope_pulley", "other_nonpowered", "other_powered", "animal_power",
                                                "manual_power", "outlet","wind_pump"),
                                              c("Hand Pump", "Electric", "Diesel", "Solar", 
                                                "Manual", "Other", "Other", "Animal",
                                                "Manual", "Tap (unknown lift)","Wind"),
                                              default=NA),
        
        #cause of breakdown# 
        breakdown =
            ifelse(functional,
                   "Functional",
                   ifelse(reason_not_used == "bad_quality",
                          "Poor Water Quality",
                          ifelse(reason_not_used == "dry_well",
                                 "Dry Well",
                                 ifelse(reason_not_used %in% c('pump_broken', 'no_diesel', 'no_electricity',
                                                                         'lift_broken', 'missing_parts','tap_broken'),
                                        "Mechanical Issue",
                                        ifelse(reason_not_used == "poorly_const",
                                               "Poorly Constructed",
                                               ifelse(reason_not_used == "under_const",
                                                      "Under Construction",
                                                      ifelse(reason_not_used == "dk",
                                                             "Don't Know",
                                                             ifelse(reason_not_used == "other",
                                                                    "Other",
                                                                    NA)))))))),
        
        #distribution type
        distribution_type = recodeVar(distribution_type,
                c("single_point","multiple_points_within_100m", "multiple_points_within_1000m", 
                  "multiple_points_beyond_1000m", "multiple_points_ < 1km_reticulation", "multiple_points_ > 1km_reticulation"),
                c("Stand Alone Water Point", "Water Scheme, Source within 100m", "Water Scheme, Source within 1km",
                  "Water Scheme, Source further than 1km", "Water Scheme, Source within 100m","Water Scheme, Source further than 1km")),
        sector = "water",
        facility_name = "Water Point"
    )
        
#Adding distant to every facility
#combining calculated result back to original data
###113
water_774 <- lga_boudary_dist(water_774, gps_col="gps")

#Delete all those have dist >= 35 km
water_774 <- subset(water_774, dist_fake <= 35 | is.na(dist_fake))

water_sub <- water_774 %.%
    select(formhub_photo_id, state, lga, zone, lga_id, uuid, gps, community, ward, 
           lift_mechanism, water_point_type, functional, src, 
           pay_for_water_yn, facility_name, sector, distribution_type,
           breakdown, is_improved, date_of_survey, unique_lga)

saveRDS(water_sub, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Water_774_NMIS_Facility.rds")
saveRDS(water_774, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Water_774_ALL_FACILITY_INDICATORS.rds")
