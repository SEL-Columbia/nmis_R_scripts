# source("source_scripts/NMIS_Utils.R")
# source("base_scripts/InstallFormhub.R")
require(plyr)
require(doBy)
require(digest)

edu_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Education_661_Merged.csv", 
                    stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999"))
edu_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Educ_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv",
                  stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999"))
edu_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Education_cleaned_2011Nov17.csv",
                    stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999"))

edu_661$src <- "661"
edu_113$src <- "113"
edu_pilot$src <- "pilot"

edu_113$uuid <- sapply(paste(edu_113$gps, edu_113$photo), FUN=digest)
edu_pilot$uuid <- sapply(paste(edu_pilot$gps, edu_pilot$photo), FUN=digest)


####
common_slugs_113 <- names(edu_661)[(which(names(edu_661) %in% names(edu_113)))]


slugsearch <- function(nm, df=edu_661){
    names(df)[grep(nm, names(df), ignore.case=T)]
}
see <- function(nm, df=edu_113)
{
    table(df[,nm],exclude=NULL)
}

na_num <- function(vec) length(which(is.na(vec)))

na_prop <- function(vec) {
    print(class(vec)) 
    na_num(vec)/length(vec)
}

# length(which(names(edu_113) %in% names(edu_661)))
# length(which(names(edu_pilot) %in% names(edu_113)))
# # edu_113$num_students_male


#### adding new variable to 661
# after cleaning999
edu_661$chalkboard_each_classroom_yn <- edu_661$num_classrms_total <= edu_661$num_classrm_w_chalkboard
# after cleaning999
edu_661$num_textbooks <-  
    ifelse(edu_661$level_of_education %in% c('primary_only', 'preprimary_and_primary'),  
        edu_661$num_math_textbook_pry + edu_661$num_english_textbook_pry + 
        edu_661$num_soc_science_textbook_pry + edu_661$num_science_textbook_pry,
    ifelse(edu_661$level_of_education %in% c('junior_and_senior_sec', 'juniors_sec_only'),
        edu_661$num_math_textbook_js + edu_661$num_english_textbook_js + 
        edu_661$num_soc_science_textbook_js + edu_661$num_science_textbook_js,
    ifelse(edu_661$level_of_education %in% c('primary_and_junior_sec', 'primary_junior_and_senior_sec'),
        edu_661$num_math_textbook_pry + edu_661$num_english_textbook_pry + 
        edu_661$num_soc_science_textbook_pry + edu_661$num_science_textbook_pry +
        edu_661$num_math_textbook_js + edu_661$num_english_textbook_js + 
        edu_661$num_soc_science_textbook_js + edu_661$num_science_textbook_js,
    0)))


# edu_661$num_students_total_gender.num_students_total / 
#     (edu_661$num_toilet.num_toilet_boy + edu_661$num_toilet.num_toilet_girl + edu_661$num_toilet.num_toilet_both)

####
### mapping 113 names
# after cleaning999
edu_113 <- rename(edu_113, c("days_no_potable_water_pastmth" = "days_no_potable_water",
                  "num_unattached_desks" = "num_desks",
                  "num_textbooks_pry_sci" = "num_science_textbook_pry",
                  "lga" = "mylga",
                  "state" = "mylga_state", 
                  "zone"= "mylga_zone",
                  "water_pipe_water" = "water.pipe_water", 
                  "water_tube_well" = "water.tube_well", 
                  "toilet_flush_or_pour_flush" = "toilet.flush_or_pour_flush_improved", 
                  "toilet_ventilated_improved" = "toilet.ventilated_improved", 
                  "toilet_pit_latrine_with_slab" = "toilet.pit_latrine_with_slab",
                  "power_generator" = "power_sources.generator", 
                  "power_solar_system" = "power_sources.solar_system", 
                  "power_grid_connection" = "power_sources.grid", 
                  "funtioning_library_yn" = "functioning_library_yn"))

mapped_113 <- c("days_no_potable_water", "num_desks", "num_science_textbook_pry", "water.pipe_water",
                "water.tube_well", "toilet.flush_or_pour_flush_improved", "toilet.ventilated_improved",
                "toilet.pit_latrine_with_slab", "power_sources.generator", "power_sources.solar_system",
                "power_sources.grid", "functioning_library_yn")


newname_113 <- c("days_no_electricity", "days_no_water_pastmth", "flush_toilet_number",
                 "flush_toilet_not_working", "vip_latrine_number", "vip_latrine_not_working",
                 "slab_pit_latrine_number", "slab_pit_latrine_not_working", "open_pit_latrine_number",
                 "open_pit_latrine_not_working", "bucket_system_not_working", "bucket_system_number",
                 "other_toilets_number", "other_toilets_not_working", "times_building_cleaned_lastmth",
                 "times_trash_disposed_lastmth", "num_tchrs_male_full_time", "num_tchrs_female_full_time",
                 "num_tchrs_female_part_time", "tchrs_male_below_ssce", "tchrs_female_below_ssce", 
                 "tchrs_male_ssce_wasc", "tchrs_female_ssce_wasc", "tchrs_male_grade2", 
                 "tchrs_female_grade2", "tchrs_male_ond", "tchrs_female_ond",
                 "tchrs_male_nce", "tchrs_female_nce", "tchrs_male_other_w_nce", "tchrs_female_other_w_nce",
                 "tchrs_male_pgde", "tchrs_female_pgde", "tchrs_male_b_ed", "tchrs_female_b_ed", 
                  "tchrs_male_other_wo_nce", "tchrs_female_other_wo_nce", "num_sr_staff_male",
                 "num_sr_staff_female", "num_jr_staff_male", "num_jr_staff_female",
                 "days_school_understaffed", "school_max_num_students", "admit_more_num_students",
                 "materials_fee", "uniforms_fee", "sports_fee", "sports_fee_exempt_yn",
                 "num_students_exempt", "in_kind_fees_yn", "booklist_per_class_yn", 
                 "annual_budget_amt_received", "num_tchrs_paid_fed_gov", "num_tchrs_payrl_st_gov",
                 "num_tchrs_othr_payrl_st_gov", "num_tchrs_paid_loc_gov", "num_tchrs_paid_prvt_for_profit", 
                 "num_tchrs_paid_prvt_non_profit", "num_tchrs_paid_other_src", "num_tchrs_no_salary",
                 "num_students_scholarship", "scholarship_amt", "num_library_materials",
                 "times_new_materials_added", "num_classrms_unused", "num_classrooms_multiple_use",
                 "num_other_rooms", "num_sections_pry1", "num_sections_pry2", "num_sections_pry3", 
                 "num_sections_pry4", "num_sections_pry5", "num_sections_pry6", 
                 "num_sections_js1", "num_sections_js2", "num_sections_js3", 
                 "num_attached_benches","num_attached_benches_unused", "num_unattached_benches",
                 "num_unattached_benches_unused", "num_unattached_desks_unused", "num_textbooks_english",
                 "num_textbooks_math", "num_textbooks_social_sci", "num_exercise_books",
                 "num_tchrs_attended_last_day", "grid_months_broken" )


# Creating new variables in 113
edu_113$num_tchrs_male <- apply(cbind(edu_113$num_tchrs_male_full_time, edu_113$num_tchrs_male_part_time), 1,
                                 sum, na.rm=T)

edu_113$num_tchrs_female <- apply(cbind(edu_113$num_tchrs_female_full_time, edu_113$num_tchrs_female_part_time), 1,
                                sum, na.rm=T)

edu_113$num_tchrs_w_nce <- apply(cbind(edu_113$tchrs_male_nce, 
                                       edu_113$tchrs_female_nce, 
                                       edu_113$tchrs_male_other_w_nce,
                                       edu_113$tchrs_female_other_w_nce), 1, sum, na.rm=T)

edu_113$num_benches <- apply(cbind(edu_113$num_attached_benches, 
                                   edu_113$num_unattached_benches), 1, sum, na.rm=T)

edu_113$num_classrms_total <- apply(cbind(edu_113$num_classrms_good_cond, 
                                          edu_113$num_classrms_need_min_repairs, 
                                          edu_113$num_classrms_need_maj_repairs), 1, sum, na.rm=T)

edu_113$ratio_students_to_benches <- replace(edu_113$num_students_total, is.na(edu_113$num_students_total), 0) / 
                                                    replace(edu_113$num_benches, is.na(edu_113$num_benches), 0) 

edu_113$unique_lga <- paste(edu_113$mylga_state, edu_113$mylga, sep='_')

edu_113$school_managed <- ifelse(edu_113$school_managed_fed_gov, "fed_gov",
                             ifelse(edu_113$school_managed_st_gov, "st_gov",
                                ifelse(edu_113$school_managed_loc_gov, "loc_gov",
                                   ifelse(edu_113$school_managed_priv_profit, "priv_profit",
                                      ifelse(edu_113$school_managed_priv_noprofit, "priv_noprofit",
                                        ifelse(edu_113$school_managed_other | !is.na(edu_113$school_managed_other_specify), "other",
                                            NA))))))

edu_113$grid_proximity[edu_113$power_grid_connection == T] <- "connected_to_grid"

slugsearch("textbook", edu_113)


edu_113$num_textbooks <- apply(cbind(edu_113$num_textbooks_english, 
                                     edu_113$num_textbooks_math, 
                                     edu_113$num_textbooks_social_sci,
                                     edu_113$num_textbooks_pry_sci), 1, sum, na.rm=T)


# edu_113$improved_sanitation <- (edu_113$toilet.flush_or_pour_flush_improved | 
#                                 edu_113$toilet.ventilated_improved | 
#                                 edu_113$toilet.pit_latrine_with_slab)

edu_113$pupil_toilet_ratio_facility <- (ifelse(edu_113$flush_toilet_drain_to == "improved", 
                                          apply(cbind(edu_113$flush_toilet_number, 
                                                      edu_113$vip_latrine_number,
                                                      edu_113$slab_pit_latrine_number), 1, sum, na.rm=T),
                                          apply(cbind(edu_113$vip_latrine_number,
                                                      edu_113$slab_pit_latrine_number,
                                                      edu_113$education_improved_sanitation), 1, sum, na.rm=T)) / edu_113$num_students_total)


edu_113$fees.admission_new <- as.logical(edu_113$new_stdnts_enroll_fee > 0)
edu_113$fees.tuition_cont <- as.logical(edu_113$cont_stdnts_enroll_fee > 0)
edu_113$fees.textbook <- as.logical(edu_113$textbooks_fee > 0)
edu_113$fees.transport <- as.logical(edu_113$transport_fee > 0)
edu_113$fees.exam_fee <- as.logical(edu_113$exams_fee > 0)
edu_113$fees.pta_fee <- as.logical(edu_113$pta_fee > 0)



# "cont_stdnts_enroll_fee" = "fees.tuition_cont", 
# "textbooks_fee" = "fees.textbook", 
# "transport_fee" = "fees.transport", 
# "exams_fee" = "fees.exam_fee", 
# "pta_fee" = "fees.pta_fee", 


ed$covered_roof_good_condi <- edu_113$covered_roof_yn %in% c("roof_fence_good_condition", 'yes')






#### Pilot 

edu_pilot <- rename(edu_pilot, c("num_total_classrooms" = "num_classrms_total",
                                 "lga" = "mylga",
                                 "state" = "mylga_state", 
                                 "zone"= "mylga_zone",
                                 "X_p_num_total_desk" = "num_desks",
                                 "X_p_num_benches_chairs" = "num_benches",
                                 "water_pipe_water" = "water.pipe_water", 
                                 "water_tube_well" = "water.tube_well", 
                                 "toilet_flush_or_pour_flush" = "toilet.flush_or_pour_flush_improved", 
                                 "toilet_ventilated_improved" = "toilet.ventilated_improved", 
                                 "toilet_pit_latrine_with_slab" = "toilet.pit_latrine_with_slab",
                                 "days_no_water_any_source" = "days_no_water_pastmth"
                                 
                                 ))


edu_pilot$days_no_water_any_source
edu_113$days_no_water_pastmth

### Pilot new indicator
edu_pilot$grid_proximity[edu_pilot$power_grid_connection == T] <- "connected_to_grid"

edu_pilot$num_textbooks <- apply(cbind(edu_pilot$num_textbooks_english, 
                                       edu_pilot$num_textbooks_math, 
                                       edu_pilot$num_textbooks_social_sci,
                                       edu_pilot$num_textbooks_pry_sci), 1, sum, na.rm=T)

edu_pilot$fees.transport <- as.logical(edu_pilot$transport_fee > 0)

###### All 3 new indicator:
ed$potable_water <- ((e_p$days_no_potable_water < 7) & (e_p$water_none == FALSE))




slugsearch("manage", edu_113)
slugsearch("manage", edu_pilot)
slugsearch("manage", edu_661)
common_slugs

see("X_p_managed_by", edu_pilot)
                    

#### combining 661, 113 & pilot
edu_total <- rbind.fill(edu_661, edu_113, edu_pilot)
saveRDS(edu_total, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Normalized/Edu774.rds")



##########
##Health##
##########



health_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Health_661_Merged.csv", stringsAsFactors=F)
health_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Health_PhII_RoundI&II&III_Clean_2011.10.21.csv",
                    stringsAsFactors=F, na.strings = c("NA", "n/a"))
health_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Data_Health_Clean_2011.11.18.csv",
                      stringsAsFactors=F, na.strings = c("NA", "n/a"))
