setwd("~/Code/nmis_R_scripts/")
source("~/Code/nmis_R_scripts/base_scripts/InstallFormhub.R")

extraSchema = setNames(data.frame(rbind(
                    c("mylga", "select one", "LGA"),
                    c("mylga_state", "select one", "State")),stringsAsFactors=FALSE),
              c("name", "type", "label"))
dropCols = c("mylga_.*_in_.*", ".*_calc") # cascading selects; calcs with propagated 999s
na.strings = c("999", "9999", "n/a")
###### READ #######
education <- formhubRead(raw_data("Education_05_06_2012_2013_05_15_12_00_14.csv"),
                         raw_data("json_schemas/Education_05_06_2012.json"),
                         extraForm = extraSchema, dropCols=dropCols)
education2 <- formhubRead(raw_data("Education_17_04_2012_2013_05_15_11_59_29.csv"),
                         raw_data("json_schemas/Education_05_06_2012.json"),
                          extraForm = extraSchema, dropCols=dropCols)
education3 <- formhubRead(raw_data("Education_22_05_2012_2013_05_14_13_41_23.csv"),
                         raw_data("json_schemas/Education_05_06_2012.json"),
                          extraForm = extraSchema, dropCols=dropCols)
health <- formhubRead(raw_data("Health_05_06_2012_2013_05_14_14_16_00.csv"),
                      raw_data("json_schemas/Health_17_04_2012.json"),
                      extraForm = extraSchema, dropCols=dropCols)
health2 <- formhubRead(raw_data("Health_17_04_2012_2013_05_15_11_45_47.csv"),
                      raw_data("json_schemas/Health_17_04_2012.json"),
                       extraForm = extraSchema, dropCols=dropCols)
health3 <- formhubRead(raw_data("Health_22_05_2012_2013_05_14_13_54_51.csv"),
                      raw_data("json_schemas/Health_17_04_2012.json"),
                       extraForm = extraSchema, dropCols=dropCols)
water <- formhubRead(raw_data("Water_05_06_2012_2012_11_30_10_54_44.csv"),
                     raw_data("json_schemas/Water_05_06_2012.json"),
                     extraForm = extraSchema, dropCols=dropCols)
water2 <- formhubRead(raw_data("Water_22_05_2012_2012_11_28_13_38_10.csv"),
                     raw_data("json_schemas/Water_05_06_2012.json"),
                      extraForm = extraSchema, dropCols=dropCols)
water3 <- formhubRead(raw_data("Water_24_04_2012_2012_11_28_13_40_08.csv"),
                     raw_data("json_schemas/Water_05_06_2012.json"),
                      extraForm = extraSchema, dropCols=dropCols)
local <- formhubRead(raw_data("Localities_05_06_2012_2013_05_24_12_25_37.csv"),
                       raw_data("json_schemas/Localities_05_06_2012.json"),
                     extraForm = extraSchema, dropCols=dropCols, na.strings=na.strings)
local2 <- formhubRead(raw_data("Localities_16_04_2012_2013_03_08_11_29_53.csv"),
                       raw_data("json_schemas/Localities_16_04_2012.json"),
                      extraForm = extraSchema, dropCols=dropCols, na.strings=na.strings)
local3 <- formhubRead(raw_data("Localities_22_05_2012_2013_03_08_11_45_37.csv"),
                       raw_data("json_schemas/Localities_22_05_2012.json"),
                      extraForm = extraSchema, dropCols=dropCols, na.strings=na.strings)

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

###### ELEMENTARY MANIPULATIONS ########
source("source_scripts/NMIS_Utils.R")
merged_education <- add_photo_url(merged_education)
merged_health <- add_photo_url(merged_health)
merged_water <- add_photo_url(merged_water)
merged_local <- add_photo_url(merged_local)

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

#######
#148
#######
#lga_148 <- read.csv("148_final_list.csv", colClasses="character")
#merged_education <- merge(merged_education, lga_148, by.x="lga_id", by.y="lga_id", all.y=T)

############################
#### WRITE EVERYTHING OUT
############################

write.csv(merged_education, in_process_data("merged/Education_661_Merged.csv"))
write.csv(merged_health, in_process_data("merged/Health_661_Merged.csv"))
write.csv(merged_water, in_process_data("merged/Water_661_Merged.csv"))
write.csv(merged_local, in_process_data("merged/Local_661_Merged.csv"))
rm(merged_education, merged_health, merged_water, merged_local, extraSchema, lga_corrections, nmis_lga_mapping)
