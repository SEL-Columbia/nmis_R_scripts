#Education 113: lga level

source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")
################################################################################################
##113###########################################################################################
################################################################################################
ed <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Education_113_ALL_FACILITY_INDICATORS.csv", stringsAsFactor=F)
e <- ed

e$is_primary <- e$level_of_education %in% 
  c('primary', 'preprimary', 'primary_js',
    'primary_js_ss', 'preprimary_primary')
e$is_junior_secondary <- e$level_of_education %in% 
  c('js', 'js_ss')
e$pj <- e$is_primary | e$is_junior_secondary
#serious outliers below
e$num_classrms_total <- rowSums(e[, c("num_classrms_good_cond",
                                      "num_classrms_need_min_repairs",
                                      "num_classrms_need_maj_repairs")], na.rm=T) 
e$num_attached_benches <- as.numeric(e$num_attached_benches)   
e$num_unattached_benches <- as.numeric(e$num_unattached_benches)   
e$num_attached_benches_unused <- as.numeric(e$num_attached_benches_unused)
e$num_unattached_benches_unused <- as.numeric(e$num_unattached_benches_unused)
#serious outliers below
e$num_benches <- rowSums(e[, c("num_attached_benches", "num_unattached_benches", 
                               "num_attached_benches_unused", "num_unattached_benches_unused")], na.rm=T)
e$num_unattached_desks <- as.numeric(e$num_unattached_desks)
e$num_unattached_desks_unused <- as.numeric(e$num_unattached_desks_unused)
e$num_desks <- rowSums(e[, c("num_unattached_desks", "num_unattached_desks_unused")], na.rm=T)
#serious outliers below
e$slab_pit_latrine_number <- as.numeric(e$slab_pit_latrine_number)
e$vip_latrine_number <- as.numeric(e$vip_latrine_number)
e$num_toilet <- rowSums(e[, c("vip_latrine_number", "slab_pit_latrine_number")], 
                        na.rm=T)


ie <- idata.frame(e)

lga_edu_data <- ddply(ie, .(lga_id), function(df) {
  data.frame(
    schools_pri_junsec_total = icount(df$pj),
    classrooms_total = sum(df$num_classrms_total, na.rm = TRUE),    
    schools_improved_water_supply = bool_proportion(df$improved_water_supply, TRUE), 
    schools_improved_sanitation = bool_proportion(df$improved_sanitation, TRUE),
    benches_school_ratio = ratio(df$num_benches, df$pj),
    benches_pupil_ratio = ratio(df$num_benches, df$num_students_total, df$pj),
    desks_school_ratio = ratio(df$num_desks, df$pj),
    desks_pupil_ratio = ratio(df$num_desks, df$num_students_total,  df$pj),    
#     classrooms_num_chalkboards = sum(df$num_classrm_w_chalkboard, na.rm = TRUE),
    teachers_total = sum(df$num_tchrs_total, na.rm = TRUE),
    teachers_qualified = sum(df$num_tchrs_with_nce, na.rm = TRUE),
    teachers_attended_training = sum(df$num_tchrs_attended_training, na.rm = TRUE),    
    txt_pupil_ratio = ratio(df$num_textbooks, df$num_students_total, df$pj),    
    percent_teaching_guides = 100 * bool_proportion(df$teacher_guide_yn, TRUE),
    schools_use_teaching_aids = icount(df$teacher_guide_yn == 'yes'),    
    num_primary_schools = icount(df$is_primary),
    num_junior_secondary_schools = icount(df$is_junior_secondary),    
    num_senior_secondary_schools = icount(df$level_of_education == 'ss'),
    num_schools = length(df$uuid),
    proportion_schools_1kmplus_catchment_primary =
      bool_proportion(df$km_to_catchment_area > 1, df$is_primary),
    proportion_schools_1kmplus_catchment_juniorsec =
      bool_proportion(df$km_to_catchment_area > 1, df$is_junior_secondary),
    proportion_schools_1kmplus_ss =
      bool_proportion(df$km_to_secondary_school > 1, df$is_primary),
    proportion_students_3kmplus_primary =
      bool_proportion(df$num_students_frthr_than_3km > 0, df$is_primary),
    proportion_students_3kmplus_juniorsec =
      bool_proportion(df$num_students_frthr_than_3km > 0, df$is_junior_secondary),     
    proportion_schools_improved_water_supply_primary =
      bool_proportion(df$improved_water_supply, df$is_primary),
    proportion_schools_improved_water_supply_juniorsec =
      bool_proportion(df$improved_water_supply, df$is_junior_secondary),    
    proportion_schools_improved_sanitation_primary =
      bool_proportion(df$improved_sanitation, df$is_primary),
    proportion_schools_improved_sanitation_juniorsec =
      bool_proportion(df$improved_sanitation, df$is_junior_secondary),    
    proportion_schools_gender_sep_toilet_primary =
      bool_proportion(df$gender_separated_toilets_yn, df$is_primary),    
    proportion_schools_gender_sep_toilet_juniorsec =
      bool_proportion(df$gender_separated_toilets_yn, df$is_junior_secondary),
    pupil_toilet_ratio_primary = 
      ratio(df$num_students_total, df$num_toilet, df$is_primary),
    pupil_toilet_ratio_secondary = 
      ratio(df$num_students_total, df$num_toilet, df$is_junior_secondary),     
    proportion_schools_power_access_primary =
      bool_proportion(df$power_access, df$is_primary),
    proportion_schools_power_access_juniorsec =
      bool_proportion(df$power_access, df$is_junior_secondary),
    proportion_classrooms_need_major_repair_primary =
      ratio(df$num_classrms_need_maj_repairs, df$num_classrms_total, df$is_primary),    
    proportion_classrooms_need_major_repair_juniorsec =
      ratio(df$num_classrms_need_maj_repairs, df$num_classrms_total, df$is_junior_secondary),
    proportion_classrooms_need_minor_repair_primary =
      ratio(df$num_classrms_need_min_repairs, df$num_classrms_total, df$is_primary),
    proportion_classrooms_need_minor_repair_juniorsec =
      ratio(df$num_classrms_need_min_repairs, df$num_classrms_total, df$is_junior_secondary),
    proportion_schools_covered_roof_good_cond_primary =
      bool_proportion(df$covered_roof_good_condi, df$is_primary),
    proportion_schools_covered_roof_good_cond_juniorsec =
      bool_proportion(df$covered_roof_good_condi, df$is_junior_secondary),       
    proportion_schools_with_clinic_dispensary_primary =
      bool_proportion(df$access_clinic_dispensary, df$is_primary),
    proportion_schools_with_clinic_dispensary_juniorsec =
      bool_proportion(df$access_clinic_dispensary, df$is_junior_secondary),
    proportion_schools_with_first_aid_kit_primary = 
      bool_proportion(df$access_first_aid, df$is_primary),
    proportion_schools_with_first_aid_kit_juniorsec = 
      bool_proportion(df$access_first_aid, df$is_junior_secondary),  
    proportion_schools_fence_good_cond_primary = 
      bool_proportion(df$wall_fence_good_condi, df$is_primary),
    proportion_schools_fence_good_cond_juniorsec = 
      bool_proportion(df$wall_fence_good_condi, df$is_junior_secondary),   
    student_classroom_ratio_lga_primary =
      ratio(df$num_students_total, df$num_classrms_total, df$is_primary),
    student_classroom_ratio_lga_juniorsec =
      ratio(df$num_students_total, df$num_classrms_total, df$is_junior_secondary),
    proportion_schools_hold_classes_outside_primary =
      bool_proportion(df$classes_outside_yn, df$is_primary),
    proportion_schools_hold_classes_outside_juniorsec =
      bool_proportion(df$classes_outside_yn, df$is_junior_secondary),
    proportion_schools_two_shifts_primary =
      bool_proportion(df$two_shifts_yn, df$is_primary),
    proportion_schools_two_shifts_juniorsec=
      bool_proportion(df$two_shifts_yn, df$is_junior_secondary),  
    proportion_schools_multigrade_classrooms_primary = 
      bool_proportion(df$multigrade_classrms, df$is_primary),
    proportion_schools_multigrade_classrooms_juniorsec = 
      bool_proportion(df$multigrade_classrms, df$is_junior_secondary),    
    proportion_schools_chalkboard_all_rooms_primary = 
      bool_proportion(df$chalkboard_each_classroom_yn, df$is_primary),                              
    proportion_schools_chalkboard_all_rooms_juniorsec = 
      bool_proportion(df$chalkboard_each_classroom_yn, df$is_junior_secondary),                                
    pupil_bench_ratio_lga_primary = 
      ratio(df$num_students_total, df$num_benches, df$is_primary),                                     
    pupil_bench_ratio_lga_juniorsec =
      ratio(df$num_students_total, df$num_benches, df$is_junior_secondary),                            
    pupil_desk_ratio_lga_primary =
      ratio(df$num_students_total, df$num_desks, df$is_primary),      
    pupil_desk_ratio_lga_juniorsec = 
      ratio(df$num_students_total, df$num_desks, df$is_junior_secondary),    
    primary_school_pupil_teachers_ratio_lga =
      ratio(df$num_students_total, df$num_tchrs_total, df$is_primary), 
    junior_secondary_school_pupil_teachers_ratio_lga =
      ratio(df$num_students_total, df$num_tchrs_total, df$is_junior_secondary),
    teacher_nonteachingstaff_ratio_lga_primary =
      ratio(df$num_tchrs_total, df$num_sr_staff_total + df$num_jr_staff_total, df$is_primary),
    teacher_nonteachingstaff_ratio_lga_juniorsec =
      ratio(df$num_tchrs_total, df$num_sr_staff_total + df$num_jr_staff_total, df$is_junior_secondary),  
    proportion_teachers_nce_primary =
      ratio(df$num_tchrs_with_nce, df$num_tchrs_total, df$is_primary),
    proportion_teachers_nce_juniorsec =
      ratio(df$num_tchrs_with_nce, df$num_tchrs_total, df$is_junior_secondary),
    proportion_teachers_training_last_year_primary = 
      ratio(df$num_tchrs_attended_training, df$num_tchrs_total, df$is_primary),
    proportion_teachers_training_last_year_juniorsec = 
      ratio(df$num_tchrs_attended_training, df$num_tchrs_total, df$is_junior_secondary),
    proportion_schools_delay_pay_primary =
      bool_proportion(df$tchr_pay_delay, df$is_primary),
    proportion_schools_delay_pay_juniorsec =
      bool_proportion(df$tchr_pay_delay, df$is_junior_secondary),
    proportion_schools_missed_pay_primary =
      bool_proportion(df$tchr_pay_miss, df$is_primary),
    proportion_schools_missed_pay_juniorsec =
      bool_proportion(df$tchr_pay_miss, df$is_junior_secondary),
    num_textbooks_per_pupil_primary =
      ratio(df$num_textbooks, df$num_students_total,  df$is_primary),
    num_textbooks_per_pupil_juniorsec =
      ratio(df$num_textbooks, df$num_students_total, df$is_junior_secondary),
    proportion_provide_exercise_books_primary =
      bool_proportion(df$provide_exercise_books_yn, df$is_primary),
    proportion_provide_exercise_books_juniorsec =
      bool_proportion(df$provide_exercise_books_yn, df$is_junior_secondary),
    proportion_provide_pens_pencils_primary =
      bool_proportion(df$provide_pens_yn, df$is_primary),
    proportion_provide_pens_pencils_juniorsec =
      bool_proportion(df$provide_pens_yn, df$is_junior_secondary),
    proportion_natl_curriculum_primary = 
      bool_proportion(df$natl_curriculum_yn, df$is_primary),
    proportion_natl_curriculum_juniorsec = 
      bool_proportion(df$natl_curriculum_yn, df$is_junior_secondary),    
    proportion_teachers_with_teacher_guide_primary = 
      bool_proportion(df$teacher_guide_yn, df$is_primary),  
    proportion_teachers_with_teacher_guide_juniorsec = 
      bool_proportion(df$teacher_guide_yn, df$is_junior_secondary),  
    proportion_schools_functioning_library_primary = 
      bool_proportion(df$funtioning_library_yn, df$is_primary),    
    proportion_schools_functioning_library_juniorsec = 
      bool_proportion(df$funtioning_library_yn, df$is_junior_secondary),                                
    num_preprimary_level =
      icount(df$level_of_education == 'preprimary'),              
    num_preprimary_primary_level = 
      icount(df$level_of_education == 'preprimary_primary'),              
    num_primary_level =
      icount(df$level_of_education == 'primary'),              
    num_primary_js_level =  
      icount(df$level_of_education == 'primary_js'),              
    num_js_level =
      icount(df$level_of_education == 'js'),              
    num_js_ss_level = 
      icount(df$level_of_education == 'js_ss'),              
    num_ss_level = 
      icount(df$level_of_education == 'ss'),            
    num_primary_js_ss_level = 
      icount(df$level_of_education == 'primary_js_ss'),
    num_other_level = icount(df$level_of_education %in%
                               c('adult_lit', 'adult_ed', 'adult_vocational',
                                 'vocational_post_primary', 'vocational_post_secondary')),
    student_teacher_ratio_lga = 
      ratio(df$num_students_total, df$num_tchrs_total),              
    proportion_teachers_nce = 
      ratio(df$num_tchrs_with_nce, df$num_tchrs_total),
    number_classrooms_need_major_repair = sum(df$num_classrms_need_maj_repairs, na.rm = TRUE)
    
  )})                   


###### SUMMING UP #########
lga_education_all <- lga_edu_data


lgas <- subset(read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv"), select=c("lga_id", "lga", "state", "zone"))
lga_education_all <- merge(lga_education_all, lgas, by="lga_id")

#writing out
write.csv(x_y_killa(lga_education_all), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Education_LGA_level_113.csv", row.names=F)

