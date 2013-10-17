# source("source_scripts/NMIS_Utils.R")
# source("base_scripts/InstallFormhub.R")
setwd("~/work/r/nmis_R_scripts/")
source("source_scripts/Normailize_Functions.R")
source("source_scripts/NMIS_Functions.R")


edu_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Education_661_Merged.csv", 
                    stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", "99999", "999999", ""))
edu_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Educ_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv",
                  stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", "99999", "999999", ""))
edu_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Education_cleaned_2011Nov17.csv",
                    stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", "99999", "999999", ""))

edu_661$src <- "661"
edu_113$src <- "113"
edu_pilot$src <- "pilot"

edu_113$uuid <- sapply(paste(edu_113$gps, edu_113$photo), FUN=digest)
edu_pilot$uuid <- sapply(paste(edu_pilot$gps, edu_pilot$photo), FUN=digest)


####
common_slugs_113 <- names(edu_661)[(which(names(edu_661) %in% names(edu_113)))]
edu_common <- common_slug(c("edu_113", "edu_661", "edu_pilot"))
edu_class <- common_type(c("edu_113", "edu_661", "edu_pilot"))
########################
#### Mapping Names #####
########################

### 113 names
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
                             "funtioning_library_yn" = "functioning_library_yn",
                             "toilet_none" = "toilet.none",
                             "water_none" = "water.none"))

## Pilot
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
                                 "days_no_water_any_source" = "days_no_water_pastmth",
                                 "toilet_none" = "toilet.none",
                                 "X_p_num_improved_sanitation" = "num_toilet_total",
                                 "num_tchrs_male_full_time" = "num_tchrs_male",
                                 "num_tchrs_female_full_time" = "num_tchrs_female",
                                 "water_none" = "water.none",
                                 "power_grid_connection" = "power_sources.grid",
                                 "funtioning_library_yn" = "functioning_library_yn"))

mapped_113 <- c("days_no_potable_water", "num_desks", "num_science_textbook_pry", "water.pipe_water",
                "water.tube_well", "toilet.flush_or_pour_flush_improved", "toilet.ventilated_improved",
                "toilet.pit_latrine_with_slab", "power_sources.generator", "power_sources.solar_system",
                "power_sources.grid", "functioning_library_yn", "toilet.none")


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

#######
#Adding Few vars before cleaning 999
#######

edu_113$school_managed <- ifelse(edu_113$school_managed_fed_gov, 
                                 "fed_gov",
                          ifelse(edu_113$school_managed_st_gov, 
                                 "st_gov",
                          ifelse(edu_113$school_managed_loc_gov, 
                                 "loc_gov",
                          ifelse(edu_113$school_managed_priv_profit, 
                                 "priv_profit",
                          ifelse(edu_113$school_managed_priv_noprofit, 
                                 "priv_noprofit",
                          ifelse(edu_113$school_managed_other | !is.na(edu_113$school_managed_other_specify),
                                 "other",
                                NA))))))

edu_113$fees.admission_new <- as.logical(edu_113$new_stdnts_enroll_fee > 0)
edu_113$fees.tuition_cont <- as.logical(edu_113$cont_stdnts_enroll_fee > 0)
edu_113$fees.textbook <- as.logical(edu_113$textbooks_fee > 0)
edu_113$fees.transport <- as.logical(edu_113$transport_fee > 0)
edu_113$fees.exam_fee <- as.logical(edu_113$exams_fee > 0)
edu_113$fees.pta_fee <- as.logical(edu_113$pta_fee > 0)

edu_661$covered_roof_good_condi <- edu_661$covered_roof_yn == "yes_good_condition"
edu_113$covered_roof_good_condi <- edu_113$covered_roof_yn %in% c("roof_fence_good_condition", 'yes')
edu_pilot$covered_roof_good_condi <- edu_pilot$covered_roof_yn %in% c("roof_fence_good_condition", 'yes')

edu_113$multigrade_teaching_yn <- NA
edu_113$times_tchr_pay_delay_pastyr <- as.integer(edu_113$times_tchr_pay_delay_pastyr)
edu_113$times_tchr_pay_miss_pastyr <- as.integer(edu_113$times_tchr_pay_miss_pastyr)

                
##################################
#### combining 661, 113 & pilot
#################################
edu_total <- rbind.fill(edu_661, edu_113, edu_pilot)


###############################################
####mapping values and standardize the type####
###############################################

edu_113$vip_latrine_number <- as.numeric(edu_113$vip_latrine_number)
edu_113$slab_pit_latrine_number <- as.numeric(edu_113$slab_pit_latrine_number)

#### Total
edu_total$toilet.none <- as.logical(edu_total$toilet.none)
edu_total$water.none <- as.logical(edu_total$water.none)

edu_113$times_tchr_pay_delay_pastyr <- as.integer(edu_113$times_tchr_pay_delay_pastyr)
edu_113$times_tchr_pay_miss_pastyr <- as.integer(edu_113$times_tchr_pay_miss_pastyr)

edu_total$borehole_tubewell_repair_time <- as.logical(recodeVar(edu_total$borehole_tubewell_repair_time,
                                                                c('yes', 'fixed_more_than_month', 'fixed_within_day',
                                                                  'fixed_within_month', 'fixed_within_week', 'never_broken',
                                                                  'not_fixed', 'no'),
                                                                c(TRUE, TRUE, TRUE, TRUE,TRUE, TRUE,
                                                                  FALSE, FALSE)))

# edu_total$natl_curriculum_yn <- as.logical(recodeVar(edu_total$natl_curriculum_yn,
#                                           c('yes', 'no'),
#                                           c(TRUE, FALSE)))
# 
# edu_total$sports_fee_exempt_yn <- as.logical(recodeVar(edu_total$sports_fee_exempt_yn,
#                                                      c('yes', 'no'),
#                                                      c(TRUE, FALSE)))
# 
# edu_total$in_kind_fees_yn <- as.logical(recodeVar(edu_total$in_kind_fees_yn,
#                                                        c('yes', 'no'),
#                                                        c(TRUE, FALSE)))
# 
# edu_total$booklist_per_class_yn <- as.logical(recodeVar(edu_total$booklist_per_class_yn,
#                                                   c('yes', 'no'),
#                                                   c(TRUE, FALSE)))
# edu_total$generator_funct_yn <- as.logical(recodeVar(edu_total$generator_funct_yn,
#                                                         c('yes', 'no'),
#                                                         c(TRUE, FALSE)))
# edu_total$solar_funct_yn <- as.logical(recodeVar(edu_total$solar_funct_yn,
#                                                      c('yes', 'no'),
#                                                      c(TRUE, FALSE)))
# edu_total$grid_funct_yn <- as.logical(recodeVar(edu_total$grid_funct_yn,
#                                                      c('yes', 'no'),
#                                                      c(TRUE, FALSE)))

# edu_total$classes_outside_yn <- as.logical(recodeVar(edu_total$classes_outside_yn,
#                                                 c('yes', 'no'),
#                                                 c(TRUE, FALSE)))
# 
# edu_total$two_shifts_yn <- as.logical(recodeVar(edu_total$two_shifts_yn,
#                                                      c('yes', 'no'),
#                                                      c(TRUE, FALSE)))
# 
# edu_total$provide_exercise_books_yn <- as.logical(recodeVar(edu_total$provide_exercise_books_yn,
#                                                 c('yes', 'no'),
#                                                 c(TRUE, FALSE)))
# 
# edu_total$provide_pens_yn <- as.logical(recodeVar(edu_total$provide_pens_yn,
#                                                 c('yes', 'no'),
#                                                 c(TRUE, FALSE)))
# 
# edu_total$teacher_guide_yn <- as.logical(recodeVar(edu_total$teacher_guide_yn,
#                                                   c('yes', 'no'),
#                                                   c(TRUE, FALSE)))
# 
# edu_total$functioning_library_yn <- as.logical(recodeVar(edu_total$functioning_library_yn,
#                                                   c('yes', 'no'),
#                                                   c(TRUE, FALSE)))

yes_no_columns <- c("functioning_library_yn", "teacher_guide_yn", "provide_pens_yn",
                    "provide_exercise_books_yn", "two_shifts_yn", "classes_outside_yn",
                    "grid_funct_yn", "solar_funct_yn", "generator_funct_yn", 
                    "booklist_per_class_yn", "in_kind_fees_yn", "sports_fee_exempt_yn",
                    "natl_curriculum_yn")

# numeric type conversion and ASSERTION(sort of)
edu_total <- yes_no_batch(edu_total, yes_no_columns)
check_type <- batch_type(edu_total, yes_no_columns)
stopifnot(all(check_type %in% c("logical")))

################
#### output ####
################
# Final cleaning remove lga_id = NA and duplicated UUID rows
edu_total <- subset(edu_total, !(duplicated(edu_total$uuid) | is.na(edu_total$lga_id)))

lgas <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv")
lgas <- subset(lgas, select=-c(latitude, longitude))

edu_total <- merge_non_redundant(lgas, edu_total, by="lga_id")
saveRDS(edu_total, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Normalized/Edu774.rds")
################################
##### Added in outlier & 999####
################################

# Creating Num_tchrs_male & female for 113 source
edu_113$num_tchrs_male <- apply(cbind(edu_113$num_tchrs_male_full_time, edu_113$num_tchrs_male_part_time), 1,
                                sum, na.rm=T)
edu_113$num_tchrs_female <- apply(cbind(edu_113$num_tchrs_female_full_time, edu_113$num_tchrs_female_part_time), 1,
                                  sum, na.rm=T)

edu_113$num_tchrs_w_nce <- apply(cbind(edu_113$tchrs_male_nce, 
                                       edu_113$tchrs_female_nce, 
                                       edu_113$tchrs_male_other_w_nce,
                                       edu_113$tchrs_female_other_w_nce), 1, sum, na.rm=T)


edu_113$num_classrms_total <- apply(cbind(edu_113$num_classrms_good_cond, 
                                          edu_113$num_classrms_need_min_repairs, 
                                          edu_113$num_classrms_need_maj_repairs), 1, sum, na.rm=T)

edu_113$num_toilet_total <- apply(cbind(edu_113$vip_latrine_number, 
                                        edu_113$slab_pit_latrine_number), 
                                  1, sum, na.rm=T)

edu_113$num_benches <- apply(cbind(edu_113$num_attached_benches, 
                                   edu_113$num_unattached_benches), 1, sum, na.rm=T)


edu_113$ratio_students_to_benches <- replace(edu_113$num_students_total, is.na(edu_113$num_students_total), 0) / 
    replace(edu_113$num_benches, is.na(edu_113$num_benches), 0) 

edu_661$chalkboard_each_classroom_yn <- edu_661$num_classrms_total <= edu_661$num_classrm_w_chalkboard

########################
###Needed to be added###
########################

#~^ 661

# after cleaning999

# after outlier
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

#~^ 113
edu_113$num_textbooks <- apply(cbind(edu_113$num_textbooks_english, 
                                     edu_113$num_textbooks_math, 
                                     edu_113$num_textbooks_social_sci,
                                     edu_113$num_textbooks_pry_sci), 1, sum, na.rm=T)

edu_113$pupil_toilet_ratio_facility <- (ifelse(edu_113$flush_toilet_drain_to == "improved", 
                                               apply(cbind(edu_113$flush_toilet_number, 
                                                           edu_113$vip_latrine_number,
                                                           edu_113$slab_pit_latrine_number), 1, sum, na.rm=T),
                                               apply(cbind(edu_113$vip_latrine_number,
                                                           edu_113$slab_pit_latrine_number,
                                                           edu_113$education_improved_sanitation), 1, sum, na.rm=T)) / edu_113$num_students_total)

edu_113$improved_sanitation <- (edu_113$toilet.flush_or_pour_flush_improved | 
                                    edu_113$toilet.ventilated_improved | 
                                    edu_113$toilet.pit_latrine_with_slab)


#~^ Pilot
edu_pilot$num_textbooks <- apply(cbind(edu_pilot$num_textbooks_english, 
                                       edu_pilot$num_textbooks_math, 
                                       edu_pilot$num_textbooks_social_sci,
                                       edu_pilot$num_textbooks_pry_sci), 1, sum, na.rm=T)

