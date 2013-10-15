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

water_113 <- rename(water_113, c("geocodeoffacility" = 'gps',
                                 )

water_661$mylga
water_113$lga
names(water_113)
water_113$geocodeoffacility
water_661$gps
water_pilot$geocodeoffacility
