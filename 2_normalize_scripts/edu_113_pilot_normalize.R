######################################################################################################################
##Normalizing Education Data: 113, Pilot 

source("source_scripts/Normailize_Functions.R")
source("source_scripts/NMIS_Functions.R")
source("source_scripts/NMIS_Utils.R")

#reading in data
edu_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/pre_normalize_archive/Educ_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv",
                    stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", "99999", "999999", "-8"))
edu_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/pre_normalize_archive/Pilot_Education_cleaned_2011Nov17.csv",
                      stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", "99999", "999999", "-8"))

#adding uuid to 113 + pilot
edu_113$uuid <- sapply(paste(edu_113$gps, edu_113$photo), FUN=digest)
edu_pilot$uuid <- sapply(paste(edu_pilot$gps, edu_pilot$photo), FUN=digest)


########################
##Mapping Names 

#113 names
edu_113 <- rename(edu_113, c("days_no_potable_water_pastmth" = "days_no_potable_water",
                             "num_unattached_desks" = "num_desks",
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
                             "water_none" = "water.none",
                             "start_time" = "start",
                             "end_time" = "end"))

#pilot
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
                                 "X_p_num_improved_sanitation" = "num_toilets_total",
                                 "num_tchrs_male_full_time" = "num_tchrs_male",
                                 "num_tchrs_female_full_time" = "num_tchrs_female",
                                 "water_none" = "water.none",
                                 "power_grid_connection" = "power_sources.grid",
                                 "funtioning_library_yn" = "functioning_library_yn",
                                 "start_time" = "start",
                                 "end_time" = "end"))
### Pilot cleanup
edu_pilot <- dplyr::mutate(edu_pilot,
     covered_roof_good_condi = covered_roof_yn %in% c("roof_fence_good_condition", 'yes'),
     natl_curriculum_yn = education_type %in% c("formal", "integrated"),
     num_tchrs_w_nce = qualified_female_teachers + qualified_male_teachers
)

######################################
##Adding variables before 999 cleaning

edu_113$school_managed <- ifelse(edu_113$school_managed_fed_gov, 
                                 "federal_gov",
                                 ifelse(edu_113$school_managed_st_gov, 
                                 "state_gov",
                                 ifelse(edu_113$school_managed_loc_gov, 
                                 "local_gov",
                                 ifelse(edu_113$school_managed_priv_profit, 
                                 "private_profit",
                                 ifelse(edu_113$school_managed_priv_noprofit, 
                                 "private_non_profit",
                                 ifelse(edu_113$school_managed_other | !is.na(edu_113$school_managed_other_specify),
                                 "none",
                                 NA))))))

edu_113$fees.admission_new <- as.logical(edu_113$new_stdnts_enroll_fee > 0)
edu_113$fees.tuition_cont <- as.logical(edu_113$cont_stdnts_enroll_fee > 0)
edu_113$fees.textbook <- as.logical(edu_113$textbooks_fee > 0)
edu_113$fees.transport <- as.logical(edu_113$transport_fee > 0)
edu_113$fees.exam_fee <- as.logical(edu_113$exams_fee > 0)
edu_113$fees.pta_fee <- as.logical(edu_113$pta_fee > 0)

edu_113$covered_roof_good_condi <- edu_113$covered_roof_yn %in% c("roof_fence_good_condition", 'yes')


edu_113$multigrade_teaching_yn <- NA
edu_113$times_tchr_pay_delay_pastyr <- as.integer(edu_113$times_tchr_pay_delay_pastyr)
edu_113$times_tchr_pay_miss_pastyr <- as.integer(edu_113$times_tchr_pay_miss_pastyr)

edu_113$num_students_frthr_than_3km <- ifelse(edu_113$num_students_frthr_than_3km < 0,
                                              edu_113$num_students_frthr_than_3km == 8,
                                              edu_113$num_students_frthr_than_3km)

edu_113$num_toilets_total <- rowSums(cbind(as.numeric(edu_113$vip_latrine_number), 
                                           as.numeric(edu_113$slab_pit_latrine_number)), 
                                     na.rm=T)

edu_113$num_tchrs_male <- rowSums(cbind(edu_113$num_tchrs_male_full_time, 
                                        edu_113$num_tchrs_male_part_time), 
                                  na.rm=T)

edu_113$num_tchrs_female <- rowSums(cbind(edu_113$num_tchrs_female_full_time, 
                                          edu_113$num_tchrs_female_part_time), 
                                    na.rm=T)

edu_113$num_tchrs_w_nce <- rowSums(cbind(edu_113$tchrs_male_nce, 
                                         edu_113$tchrs_female_nce, 
                                         edu_113$tchrs_male_other_w_nce,
                                         edu_113$tchrs_female_other_w_nce),
                                   na.rm=T)

edu_113$num_classrms_total <- rowSums(cbind(edu_113$num_classrms_good_cond, 
                                            edu_113$num_classrms_need_min_repairs, 
                                            edu_113$num_classrms_need_maj_repairs),
                                      na.rm=T)

edu_113$num_benches <- rowSums(cbind(edu_113$num_attached_benches, 
                                     edu_113$num_unattached_benches),
                               na.rm=T)

# add full_time_teacher
edu_113$num_tchr_full_time <- rowSums(cbind(edu_113$num_tchrs_male_full_time, 
                                            edu_113$num_tchrs_female_full_time),
                                        na.rm=T)




# Type conversion
##################
yes_no_columns <- c("toilet.none", "water.none","functioning_library_yn", "teacher_guide_yn", "provide_pens_yn", 
                    "provide_exercise_books_yn", "two_shifts_yn", "classes_outside_yn", 
                    "grid_funct_yn", "solar_funct_yn", "generator_funct_yn", "booklist_per_class_yn", 
                    "in_kind_fees_yn", "sports_fee_exempt_yn", "natl_curriculum_yn", 
                    "nomadic_school_yn", "road_yn", "all_weather_road_yn", "flush_improved_functional_yn", 
                    "flush_unimproved_functional_yn", "vip_latrine_functional_yn", 
                    "slab_pit_latrine_functional_yn", "open_pit_latrine_functional_yn", 
                    "bucket_system_functional_yn", "school_provide_meals_yn", "school_code_yn", 
                    "boarding_school_yn", "daily_public_transport_yn", "other_funct_yn", 
                    "temporary_structure_yn", "gender_separated_toilets_yn", "landline_funct_yn", 
                    "mobile_phone_funct_yn", "mobile_signal_funct_yn", "computer_funct_yn", 
                    "computer_student_use_funct_yn", "internet_funct_yn", "printer_funct_yn", 
                    "info_tech_other_funct_yn", "tchrs_quarters_yn", "tchrs_quarters_sufficient_yn", 
                    "tchrs_on_time_yn", "admit_more_students_yn", "new_stdnts_enroll_fee_exempt_yn", 
                    "cont_stdnts_enroll_fee_exempt_yn", "textbooks_fee_exempt_yn", 
                    "materials_fee_exempt_yn", "uniforms_fee_exempt_yn", "transport_fee_exempt_yn", 
                    "exams_fee_exempt_yn", "pta_fee_exempt_yn", "ube_cash_direct_yn", 
                    "etf_cash_direct_yn", "lga_cash_direct_yn", "ngo_cash_direct_yn", 
                    "school_fees_cash_direct_yn", "other_cash_direct_yn", "sports_area_yn", 
                    "unattached_benches_yn", "chalkboard_each_classroom_yn", "enroll_data_by_age_yn", 
                    "X_p_water_nearby_yn", "provide_notebooks_yn")

numeric_column_list <- c("slab_pit_latrine_number", "vip_latrine_not_working", "vip_latrine_number", 
                         "flush_toilet_not_working", "flush_toilet_number", "days_no_water_pastmth", 
                         "days_no_electricity", "slab_pit_latrine_not_working", "open_pit_latrine_number", 
                         "open_pit_latrine_not_working", "bucket_system_number", "bucket_system_not_working", 
                         "other_toilets_number", "other_toilets_not_working", "times_building_cleaned_lastmth", 
                         "times_trash_disposed_lastmth", "num_tchrs_male_full_time", "num_tchrs_male_part_time", 
                         "num_tchrs_female_full_time", "num_tchrs_female_part_time", "tchrs_male_below_ssce", 
                         "tchrs_female_below_ssce", "tchrs_male_ssce_wasc", "tchrs_female_ssce_wasc", 
                         "tchrs_male_grade2", "tchrs_female_grade2", "tchrs_male_ond", 
                         "tchrs_female_ond", "tchrs_male_nce", "tchrs_female_nce", "tchrs_male_pgde", 
                         "tchrs_female_pgde", "tchrs_male_b_ed", "tchrs_female_b_ed", 
                         "tchrs_male_other_w_nce", "tchrs_female_other_w_nce", "tchrs_female_other_wo_nce", 
                         "num_sr_staff_male", "num_sr_staff_female", "num_jr_staff_male", 
                         "num_jr_staff_female", "days_school_understaffed", "days_school_understaffed_closed", 
                         "school_max_num_students", "admit_more_num_students", "materials_fee", 
                         "uniforms_fee", "sports_fee", "num_students_exempt", "annual_budget_amt_received", 
                         "num_tchrs_paid_fed_gov", "num_tchrs_payrl_st_gov", "num_tchrs_othr_payrl_st_gov", 
                         "num_tchrs_paid_loc_gov", "num_tchrs_paid_prvt_for_profit", "num_tchrs_paid_prvt_non_profit", 
                         "num_tchrs_paid_other_src", "num_tchrs_no_salary", "num_students_scholarship", 
                         "scholarship_amt", "num_library_materials", "times_new_materials_added", 
                         "num_classrms_unused", "num_classrooms_multiple_use", "num_other_rooms", 
                         "num_sections_pry1", "num_sections_pry2", "num_sections_pry3", 
                         "num_sections_pry4", "num_sections_pry5", "num_sections_pry6", 
                         "num_sections_js1", "num_sections_js2", "num_sections_js3", "num_attached_benches", 
                         "num_attached_benches_unused", "num_unattached_benches", "num_unattached_benches_unused", 
                         "num_unattached_desks_unused", "num_textbooks_english", "num_textbooks_math", 
                         "num_textbooks_social_sci", "num_exercise_books", "tchrs_male_other_wo_nce", 
                         "num_tchrs_attended_training", "num_tchrs_attended_last_day", 
                         "grid_months_broken", "new_stdnts_enroll_fee", "cont_stdnts_enroll_fee", 
                         "textbooks_fee", "transport_fee", "exams_fee", "pta_fee", "days_no_potable_water", 
                         "num_desks", "num_science_textbook_pry", "num_students_frthr_than_3km", 
                         "num_students_male", "num_students_female", "num_students_total", 
                         "times_tchr_pay_delay_pastyr", "times_tchr_pay_miss_pastyr", 
                         "num_pry_female", "num_pry_male", "num_pry_total", "num_js_female", 
                         "num_js_male", "num_js_total", "num_ss_female", "num_ss_male", 
                         "num_ss_total", "num_toilet_boy", "num_toilet_girl", "num_toilet_both", 
                         "num_toilets_total", "num_tchrs_male", "num_tchrs_female", "num_tchrs_total", 
                         "num_tchrs_w_nce", "num_tchrs_w_nce_plus", "num_sr_staff_total", 
                         "num_jr_staff_total", "num_tchrs_trained_new_curricula", "num_classrms_total", 
                         "num_classrms_good_cond", "num_classrms_need_min_repairs", "num_classrms_need_maj_repairs", 
                         "num_benches", "num_classrm_w_chalkboard", "num_math_textbook_pry", 
                         "num_english_textbook_pry", "num_soc_science_textbook_pry", "num_math_textbook_js", 
                         "num_english_textbook_js", "num_soc_science_textbook_js", "num_science_textbook_js", 
                         "num_exercise_books_per_student_pry", "num_exercise_books_per_student_jss", 
                         "num_textbooks_pry_sci", "num_notebooks", "num_tchr_full_time")

# logical type conversion and ASSERTION(sort of)
edu_113 <- yes_no_batch(edu_113, yes_no_columns)
check_type <- batch_type(edu_113, yes_no_columns)
stopifnot(all(check_type %in% c("logical")))

edu_pilot <- yes_no_batch(edu_pilot, yes_no_columns)
check_type <- batch_type(edu_pilot, yes_no_columns)
stopifnot(all(check_type %in% c("logical")))

# numeric type conversion and ASSERTION(sort of)
edu_113 <- numeric_batch(edu_113, numeric_column_list)
check_type <- batch_type(edu_113, numeric_column_list)
stopifnot(all(check_type %in% c("integer", "numeric")))

edu_pilot <- numeric_batch(edu_pilot, numeric_column_list)
check_type <- batch_type(edu_pilot, numeric_column_list)
stopifnot(all(check_type %in% c("integer", "numeric")))

################
##output 

#final cleaning remove lga_id = NA and duplicated UUID rows
edu_113 <- subset(edu_113, !(duplicated(edu_113$uuid) | is.na(edu_113$lga_id)))
edu_pilot <- subset(edu_pilot, !(duplicated(edu_pilot$uuid) | is.na(edu_pilot$lga_id)))

write.csv(edu_113, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Educ_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv", row.names=F)
write.csv(edu_pilot, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Education_cleaned_2011Nov17.csv", row.names=F)
