setwd("~/work/r/nmis_R_scripts/")
source("source_scripts/Normailize_Functions.R")

# water_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Water_661_Merged.csv", 
#                     stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", ""))

water_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Water_661_999Cleaned_Reclassified.csv", 
                          stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", ""))

water_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Water_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv", 
                      stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", ""))

water_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Water_cleaned_2011Aug29.csv", 
                      stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", ""))


water_661$src <- "661"
water_113$src <- "113"
water_pilot$src <- "pilot"

water_113$uuid <- sapply(paste(water_113$gps, water_113$photo), FUN=digest)
water_pilot$uuid <- sapply(paste(water_pilot$gps, water_pilot$photo), FUN=digest)


####
common_slugs_113 <- names(water_661)[(which(names(water_661) %in% names(water_pilot)))]
water_common <- common_slug(c("water_113", "water_661", "water_pilot"))
water_class <- common_type(c("water_113", "water_661", "water_pilot"))
########################
########################
#### Mapping Names #####
########################

### 113 names

water_113 <- rename(water_113, c("geocodeoffacility" = 'gps'
                                 ))


water_pilot <- rename(water_pilot, c("geocodeoffacility" = 'gps'
                                ))



##########################
#Standardize column value#
##########################
# water_661$water_source_type <- recodeVar(water_661$water_source_type, 
# 										c(""),
# 										c(""))

water_113$water_source_type <- recodeVar(water_113$water_source_type, 
										c("borehole", "tube_well"),
										c("borehole_tube_well", "borehole_tube_well")
										)

water_pilot$water_source_type <- recodeVar(water_pilot$water_source_type, 
										c("borehole", "tube_well",
										 "rainwater", "surface_water",
                                          "dug_well"),
										c("borehole_tube_well", "borehole_tube_well", 
										 "rainwater_harvesting_scheme", "developed_protected_spring_water",
                                          "protected_dug_well")
										)



water_113$lift_mechanism <- recodeVar(water_113$lift_mechanism, 
                                         c("animal", "diesel", "solar", 
                                           "electric", "do_not_know"),
                                         c("animal_power", "fuel_pump", "solar_pump", 
                                           "electricity_pump", "dk")
)

water_pilot$lift_mechanism <- recodeVar(water_pilot$lift_mechanism, 
                                      c("diesel_pump", "electric_motor_pump", "rope_and_pulley",
                                        "not_known"),
                                      c("fuel_pump", "electricity_pump", "rope_pulley", 
                                        "dk")
)

water_total <- rbind.fill(water_661, water_113, water_pilot)
see("lift_mechanism", water_total)


w113$lift_mechanism <- recodeVar(w113_raw$lift_mechanism,
                                 c("hand_pump", "electricity_pump", "fuel_pump", "solar_pump", 
                                   "rope_pulley", "other_nonpowered", "other_powered", "animal_power",
                                   "manual_power", "outlet","wind_pump"),
                                 c("Hand Pump", "Electric", "Diesel", "Solar", 
                                   "Manual", "Other", "Other", "Animal",
                                   "Manual", "Tap (unknown lift)","Wind"),
                                 default=NA)





#######
#Adding Few vars before Combining
#######
water_661$water_point_type <- 
        ifelse(water_661$water_point_type == "handpump",
            "Handpump",
        ifelse(water_661$water_point_type %in% c('ten_thousand_overhead', 'ten_thousand_ovehead'),
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