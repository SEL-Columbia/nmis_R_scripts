######################################################################################################################
##Normalizing Water Data: 661, 113, Pilot 
source("source_scripts/Normailize_Functions.R")
source("source_scripts/NMIS_Functions.R")
source("source_scripts/NMIS_Utils.R")

#reading in data
water_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Water_661_Merged.csv", 
                    stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", ""))

water_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Water_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv", 
                      stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", ""))

water_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Water_cleaned_2011Aug29.csv", 
                      stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", ""))

reclassify <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/reclassify_final_148.csv", 
                       stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", ""))
reclassify <- subset(reclassify, select=c(uuid, Classification))    

#adding surveying source column
water_661$src <- "661"
water_113$src <- "113"
water_pilot$src <- "pilot"

#adding uuid to 113 + pilot
water_113$uuid <- sapply(paste(water_113$gps, water_113$photo), FUN=digest)
water_pilot$uuid <- sapply(paste(water_pilot$gps, water_pilot$photo), FUN=digest)

#adding photo urls
add_photo_url(water_661, 'formhub')
add_photo_url(water_pilot, 'nmisstatic')
add_photo_url(water_113, 'nmisstatic')

#merge reclassify data back to 661 data
water_661 <- merge(water_661, reclassify, by="uuid", all.x=T)
water_661 <- water_661[!duplicated(water_661$uuid),]

########################
##mapping names 

#113
water_113 <- rename(water_113, 
                    c("geocodeoffacility" = 'gps',
                      "water_source_used_today_yn" = "water_functional_yn"
                      ))

#pilot names
water_pilot <- rename(water_pilot, 
                      c("geocodeoffacility" = 'gps',
                        "water_source_used_today_yn" = "water_functional_yn"
                      ))


#########################
#standardize column values

water_113$water_source_type <- recodeVar(water_113$water_source_type, 
										c("borehole", "tube_well"),
										c("borehole_tube_well", "borehole_tube_well")
										)

water_pilot$water_source_type <- recodeVar(water_pilot$water_source_type, 
										c("borehole", "tube_well","rainwater", 
                                          "surface_water", "dug_well"),
										c("borehole_tube_well", "borehole_tube_well", "rainwater_harvesting_scheme",
                                          "developed_protected_spring_water", "protected_dug_well")
										)

water_113$lift_mechanism <- recodeVar(water_113$lift_mechanism, 
                                        c("animal", "diesel", "solar", 
                                          "electric", "do_not_know"),
                                        c("animal_power", "fuel_pump", "solar_pump", 
                                          "electricity_pump", NA)
                                        )

water_pilot$lift_mechanism <- recodeVar(water_pilot$lift_mechanism, 
                                        c("diesel_pump", "electric_motor_pump", "rope_and_pulley",
                                            "not_known"),
                                        c("fuel_pump", "electricity_pump", "rope_pulley", 
                                            NA)
                                        )

water_661$Classification <- recodeVar(water_661$Classification, 
                                        c('No Photo', 'No_photo', 'no_photo', 
                                          'ten_thousand_ovehead', 'unimproved '),
                                        c("no_photo", "no_photo", "no_photo", 
                                          'ten_thousand_overhead', "unimproved")
                                        )
##delete records with no_photo or remove in classification column
water_661 <- subset(water_661,! Classification %in% c("remove", "no_photo"))

##############################
#Adding/subtracting a few vars

water_661$water_point_type <- ifelse(!is.na(water_661$Classification), 
                                     water_661$Classification, 
                              ifelse(!is.na(water_661$water_source_type),
                                     water_661$water_source_type, 
                              ifelse(!is.na(water_661$water_scheme_type),
                                     water_661$water_scheme_type, 
                              NA)))

water_661$water_point_type <- 
        ifelse(water_661$water_point_type == "handpump",
            "Handpump",
        ifelse(water_661$water_point_type == 'ten_thousand_overhead',
            "Overhead Tank (10,000)",
        ifelse(water_661$water_point_type == "one_thousand_overhead",
            "Overhead Tank (1,000)",
        ifelse(water_661$water_point_type == "unimproved",
            "Unimproved",
        ifelse(water_661$water_point_type == "tap",
            "Tap",
        ifelse(water_661$water_point_type == "rainwater",
            "Rainwater Harvesting System",
        ifelse(water_661$water_scheme_type =="outlet",
            "Tap",
        ifelse(water_661$water_scheme_type == "dk",
            "Don't Know",
        ifelse(water_661$water_source_type == "dk",
            "Don't Know",
        ifelse(water_661$water_source_type == "borehole_tube_well",
            "Borehole",
        ifelse(water_661$water_source_type == "rainwater_harvesting_scheme",
            "Unimproved",
        ifelse(water_661$water_source_type =="dam",
            "Untreated Surface Water",
        ifelse(water_661$water_source_type == "developed_protected_spring_water",
            "Unprotected Spring",
        ifelse(water_661$water_source_type %in% c('protected_dug_well', 'unprotected_dug_well'),
            "Unimproved Well",
            NA))))))))))))))

water_113$water_point_type <- 
  		ifelse(water_113$lift_mechanism %in% c('electricity_pump', 'fuel_pump', 'solar_pump'),
        	 "Tap",
  		ifelse(water_113$lift_mechanism == "hand_pump",
        	 "Handpump",
  		ifelse(water_113$water_source_type %in% c('borehole_tube_well'),
        	 "Borehole",
  		ifelse(water_113$water_source_type == "protected_dug_well",
        	 "Unimproved Large Diameter Well",
  		ifelse(water_113$water_source_type %in% c('other_protected', 'other_unprotected'),
        	 "Unimproved",
  		ifelse(water_113$water_source_type == "developed_protected_spring_water",
        	 "Unprotected Spring",
  		ifelse(water_113$water_source_type =="rainwater_harvesting_scheme",
        	 "Unimproved Rainwater Harvesting System",
  		ifelse(water_113$water_source_type =="developed_and_treated_surface_water",
        	 "Untreated Surface Water",
         	NA))))))))

water_pilot$water_point_type <- 
 		ifelse(water_pilot$lift_mechanism %in% c('electricity_pump', 'fuel_pump', 'solar_pump'),
        	 "Tap",
  		ifelse(water_pilot$lift_mechanism == "hand_pump",
         	 "Handpump",
		ifelse(water_pilot$water_source_type %in% c('borehole_tube_well'),
        	 "Borehole",
  		ifelse(water_pilot$water_source_type == "protected_dug_well",
        	 "Unimproved Large Diameter Well",
  		ifelse(water_pilot$water_source_type %in% c(NA, 'other_protected', 'other_unprotected'),
        	 "Unimproved",
  		ifelse(water_pilot$water_source_type =="rainwater_harvesting_scheme",
        	 "Unimproved Rainwater Harvesting System",
  		ifelse(water_pilot$water_source_type =="developed_and_treated_surface_water",
        	 "Untreated Surface Water",
         	NA)))))))

water_661$distribution_type <-
  		ifelse(water_661$water_scheme_type == "water_source",
        	"Stand Alone Water Point",
      	ifelse(water_661$water_outlet_connection == "outlet_within_100m",
        	"Water Scheme, Source within 100m",
      	ifelse(water_661$water_outlet_connection == "outlet_btw_100m_1km",
        	"Water Scheme, Source within 1km",
      	ifelse(water_661$water_outlet_connection == "outlet_more_1km",
        	"Water Scheme, Source further than 1km",
      		NA))))  


##################################
##combining 661, 113 & pilot
water_total <- rbind.fill(water_661, water_113, water_pilot)

##########################################
##mapping values and standardize the type

water_total$reason_not_used <-ifelse(water_total$src == "661", 
									water_total$reason_not_used, 
									   ifelse(water_total$reasons_not_used_pump_broken,
		                                    "pump_broken",
		                               ifelse(water_total$reasons_not_used_lift_broken,
		                                	"lift_broken",
		                               ifelse(water_total$reasons_not_used_no_diesel,
		                               		"no_diesel",
		                               ifelse(water_total$reasons_not_used_no_electricity,
		                               		"no_electricity",
		                               ifelse(water_total$reasons_not_used_missing_parts,
		                               		"missing_parts",
		                               ifelse(water_total$reasons_not_used_tap_broken,
		                               		"tap_broken",
		                               ifelse(water_total$reasons_not_used_under_constreas,
		                               		"under_const",
		                               ifelse(water_total$reasons_not_used_bad_quality,
		                               		"bad_quality",
		                               ifelse(water_total$reasons_not_used_dry_well,
		                               		"dry_well",
		                               ifelse(water_total$reasons_not_used_other,
		                               		"other",
		                               ifelse(water_total$reasons_not_used_dk,
		                               		"dk",
		                               	NA))))))))))))

water_total$pay_for_water_yn <- as.logical(recodeVar(water_total$pay_for_water_yn, 
                                        c('yes', 'no', 'dk', 'do_not_know'), 
                                        c(TRUE, FALSE, NA, NA)))

water_total$water_functional_yn <- as.logical(recodeVar(water_total$water_functional_yn, 
                                          c('yes', 'no', 'dk', 'do_not_know'), 
                                          c(TRUE, FALSE, NA, NA)))


# Final Cleaning remove rows without lga_id or duplicated uuid
lgas <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv")
lgas <- subset(lgas, select=-c(latitude, longitude))
water_total <- merge_non_redundant(lgas, water_total, by="lga_id")
water_total <- subset(water_total, !(duplicated(water_total$uuid) | is.na(water_total$lga_id)))

saveRDS(water_total, '~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Normalized/Water_774_normalized.rds')

