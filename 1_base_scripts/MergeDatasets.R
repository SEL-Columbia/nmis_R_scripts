source("1_base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Utils.R")

extraSchema = setNames(data.frame(rbind(
                    c("mylga", "select one", "LGA"),
                    c("mylga_state", "select one", "State")),stringsAsFactors=FALSE),
              c("name", "type", "label"))
dropCols = c("mylga_.*_in_.*", ".*_calc") # cascading selects; calcs with propagated 999s
na.strings = c("999", "9999", "n/a")

raw_data <- function(basepath) {
  paste('~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data',basepath, sep='/')
}

###### READ #######
education <- formhubRead("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/Education_05_06_2012_2013_11_20_12_55_34.csv",
                         "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/json_schemas/Education_05_06_2012.json",
                         extraForm = extraSchema, dropCols=dropCols, keepGroupNames=F)
education2 <- formhubRead("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/Education_17_04_2012_2013_11_20_12_53_28.csv",
                         "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/json_schemas/Education_05_06_2012.json",
                          extraForm = extraSchema, dropCols=dropCols, keepGroupNames=F)
education3 <- formhubRead("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/Education_22_05_2012_2013_11_26_14_43_49.csv",
                         "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/json_schemas/Education_05_06_2012.json",
                          extraForm = extraSchema, dropCols=dropCols, keepGroupNames=F)
health <- formhubRead("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/Health_05_06_2012_2013_11_18_16_44_01.csv",
                      "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/json_schemas/Health_17_04_2012.json",
                      extraForm = extraSchema, dropCols=dropCols, keepGroupNames=F)
health2 <- formhubRead("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/Health_17_04_2012_2013_11_18_16_42_51.csv",
                      "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/json_schemas/Health_17_04_2012.json",
                       extraForm = extraSchema, dropCols=dropCols, keepGroupNames=F)
health3 <- formhubRead("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/Health_22_05_2012_2013_11_18_16_45_32.csv",
                      "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/json_schemas/Health_17_04_2012.json",
                       extraForm = extraSchema, dropCols=dropCols, keepGroupNames=F)
water <- formhubRead("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/Water_05_06_2012_2012_11_30_10_54_44.csv",
                     "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/json_schemas/Water_05_06_2012.json",
                     extraForm = extraSchema, dropCols=dropCols, keepGroupNames=F)
water2 <- formhubRead("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/Water_22_05_2012_2012_11_28_13_38_10.csv",
                     "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/json_schemas/Water_05_06_2012.json",
                      extraForm = extraSchema, dropCols=dropCols, keepGroupNames=F)
water3 <- formhubRead("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/Water_24_04_2012_2012_11_28_13_40_08.csv",
                     "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/json_schemas/Water_05_06_2012.json",
                      extraForm = extraSchema, dropCols=dropCols, keepGroupNames=F)
local <- formhubRead("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/Localities_05_06_2012_2013_05_24_12_25_37.csv",
                       "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/json_schemas/Localities_05_06_2012.json",
                     extraForm = extraSchema, dropCols=dropCols, na.strings=na.strings, keepGroupNames=F)
local2 <- formhubRead("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/Localities_16_04_2012_2013_03_08_11_29_53.csv",
                       "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/json_schemas/Localities_16_04_2012.json",
                      extraForm = extraSchema, dropCols=dropCols, na.strings=na.strings, keepGroupNames=F)
local3 <- formhubRead("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/Localities_22_05_2012_2013_03_08_11_45_37.csv",
                       "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/json_schemas/Localities_22_05_2012.json",
                      extraForm = extraSchema, dropCols=dropCols, na.strings=na.strings, keepGroupNames=F)

stopifnot((names(education) == names(education2)) && (names(education2) == names(education3)) && (names(education3) == names(education)))
stopifnot((names(health) == names(health2)) && (names(health2) == names(health3)) && (names(health3) == names(health)))
stopifnot((names(water) == names(water2)) && (names(water2) == names(water3)) && (names(water3) == names(water)))
stopifnot((names(local) == names(local2)) && (names(local2) == names(local3)) && (names(local3) == names(local)))

###### MERGE #######

merged_education <- rbind(data.frame(education), data.frame(education2), data.frame(education3))
merged_health <- rbind(data.frame(health), data.frame(health2), data.frame(health3))
merged_water <- rbind(data.frame(water), data.frame(water2), data.frame(water3))
merged_local <- rbind(data.frame(local), data.frame(local2), data.frame(local3))
rm(list=c("education", "education2", "education3", "health", "health2", "health3", "water", "water2", "water3", "local", "local2", "local3"))

############################
#### Remove UUID duplicates
############################
merged_education <- merged_education[!duplicated(merged_education$uuid), ]
merged_health <- merged_health[!duplicated(merged_health$uuid), ]
merged_water <- merged_water[!duplicated(merged_water$uuid), ]
merged_local <- merged_local[!duplicated(merged_local$uuid), ]

############################
#### Remove those without GPS points at all
############################
merged_education <- merged_education[!is.na(merged_education$gps), ]
merged_health <- merged_health[!is.na(merged_health$geocodeoffacility), ]
merged_water <- merged_water[!is.na(merged_water$gps), ]
merged_local <- merged_local[!(is.na(merged_local$gps)), ] #TODO: should be unnecessary; see why needed

############################
#### ADD lga_id
############################
merged_education <- add_lga_id(merged_education)
merged_health <- add_lga_id(merged_health)
merged_water <- add_lga_id(merged_water)
merged_local <- add_lga_id(merged_local)

############################
#### Spatially locate things where lga_id wasn't derived (ie, lga / state was missing originally)
############################
merged_education <- replace_lga_ids(merged_education)
merged_health <- replace_lga_ids(merged_health)
merged_water <- replace_lga_ids(merged_water)
merged_local <- replace_lga_ids(merged_local)

############################
#### WRITE EVERYTHING OUT
############################

write.csv(merged_education, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Education_661_Merged.csv")
write.csv(merged_health, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Health_661_Merged.csv")
write.csv(merged_water, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Water_661_Merged.csv")
write.csv(merged_local, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Local_661_Merged.csv")
rm(merged_education, merged_health, merged_water, merged_local, extraSchema, lga_corrections, nmis_lga_mapping)
