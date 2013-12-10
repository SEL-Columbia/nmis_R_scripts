## ALIASES / PREP ##
source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")

# sector slugs are at https://github.com/mvpdev/nmis/blob/develop/uis_r_us/indicators/overview.json
# overview page slugs are at https://github.com/mvpdev/nmis/blob/develop/uis_r_us/indicators/facility.py

edu774 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Education_774_ALL_FACILITY_INDICATORS.rds")

#education data request (TODO: rename to something better)
ie <- idata.frame(edu774)

lga_edu_data <- ddply(ie, .(lga_id), function(df) {
   data.frame(
              schools_pri_junsec_total = 
                  icount(df$pj),
              classrooms_total =
                  sum(df$num_classrms_total, na.rm = TRUE),
              schools_improved_water_supply = 
                  bool_proportion(df$improved_water_supply, TRUE), 
              schools_improved_sanitation =
                  bool_proportion(df$improved_sanitation, TRUE),
              benches_school_ratio =
                  ratio(df$num_benches, df$pj),
              benches_pupil_ratio =
                  ratio(df$num_benches, df$num_students_total, df$pj),
              desks_school_ratio = 
                  ratio(df$num_desks, df$pj),
              desks_pupil_ratio = 
                  ratio(df$num_desks, df$num_students_total,  df$pj),
              classrooms_num_chalkboards =
                  sum(df$num_classrm_w_chalkboard, na.rm = TRUE),
              teachers_total =
                  sum(df$num_tchrs_total, na.rm = TRUE),
              teachers_qualified =
                  sum(df$num_tchrs_w_nce, na.rm = TRUE),
              teachers_attended_training =
                  sum(df$num_tchrs_attended_training, na.rm = TRUE),
              txt_pupil_ratio = 
                  ratio(df$num_textbooks, df$num_students_total,df$pj),
              percent_teaching_guides = 
                  bool_proportion(df$teacher_guide_yn, TRUE),
              schools_use_teaching_aids = 
                  icount(df$teacher_guide_yn),
              num_primary_schools = 
                  icount(df$is_primary),
              num_junior_secondary_schools = 
                  icount(df$is_junior_secondary),
              num_senior_secondary_schools = 
                  icount(df$level_of_education %in% c('senior_sec_only','ss')),
              num_schools = 
                  length(df$uuid),
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
                  ratio(df$num_students_total, df$num_toilets_total, df$is_primary),
              pupil_toilet_ratio_secondary = 
                  ratio(df$num_students_total, df$num_toilets_total, df$is_junior_secondary),              
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
                  bool_proportion(df$class_held_outside, df$is_primary),
              proportion_schools_hold_classes_outside_juniorsec =
                  bool_proportion(df$class_held_outside, df$is_junior_secondary),
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
                  bool_proportion(df$functioning_library_yn, df$is_primary),
              proportion_schools_functioning_library_juniorsec = 
                  bool_proportion(df$functioning_library_yn, df$is_junior_secondary),                            
              num_preprimary_level =
                  icount(df$level_of_education %in% c('preprimary_only', 'preprimary')),              
              num_preprimary_primary_level = 
                  icount(df$level_of_education %in% c('preprimary_and_primary', 'preprimary_primary')),              
              num_primary_level =
                  icount(df$level_of_education %in% c('primary_only', 'primary')),              
              num_primary_js_level =  
                  icount(df$level_of_education %in% c('primary_and_junior_sec', 'primary_js')),              
              num_js_level =
                  icount(df$level_of_education %in% c('juniors_sec_only', 'js')),              
              num_js_ss_level = 
                  icount(df$level_of_education %in% c('junior_and_senior_sec', 'js_ss')),              
              num_ss_level = 
                  icount(df$level_of_education %in% c('senior_sec_only', 'ss')),            
              num_primary_js_ss_level = 
                  icount(df$level_of_education %in% c('primary_junior_and_senior_sec', 'primary_js_ss')),
              num_other_level = icount(df$level_of_education %in% c('science_technical', 'vocational', 
                                                                      'adult_lit','adult_ed', 'adult_vocational',
                                                                      'vocational_post_primary', 'vocational_post_secondary')),
              student_teacher_ratio_lga = 
                  ratio(df$num_students_total, df$num_tchrs_total),              
              proportion_teachers_nce = 
                  ratio(df$num_tchrs_with_nce, df$num_tchrs_total),
              number_classrooms_need_major_repair = 
                  sum(df$num_classrms_need_maj_repairs, na.rm = TRUE)
              )})                   
                  
###### core indicator calculations
lga_edu_data_core <- ddply(ie, .(lga_id), function(df) {
  data.frame(   
      # Facilities
        percent_management_public = 
          icount(df$management == 'public')/length(df$management),
        percent_natl_curriculum =
          bool_proportion(df$natl_curriculum_yn, TRUE),
      # General information for Primary Schols
        percent_management_public_primary = 
          bool_proportion(df$management == 'public', df$is_primary),
        percent_natl_curriculum_primary =
          bool_proportion(df$natl_curriculum_yn, df$is_primary),
        num_school_1kmplus_secondary_school = 
          bool_proportion(df$school_1kmplus_secondary_school, df$is_primary),
        avg_num_students_primary =
          ratio(df$num_students_total,  df$is_primary),
        avg_num_tchrs_primary = 
          ratio(df$num_tchrs_total,  df$is_primary),
        avg_num_classrms_primary = 
          ratio(df$num_classrms_total,  df$is_primary),
        avg_num_toilets_primary = 
          ratio(df$num_toilets_total,  df$is_primary),
      # General information for Junior Secondary Schools
        percent_management_public_js = 
          bool_proportion(df$management == 'public', df$is_junior_secondary),
        percent_natl_curriculum_js =
          bool_proportion(df$natl_curriculum_yn, df$is_junior_secondary),
        avg_num_students_js =
          ratio(df$num_students_total,  df$is_junior_secondary),
        avg_num_tchrs_js = 
          ratio(df$num_tchrs_total,  df$is_junior_secondary),
        avg_num_classrms_js = 
          ratio(df$num_classrms_total,  df$is_junior_secondary),
        avg_num_toilets_js = 
          ratio(df$num_toilets_total,  df$is_junior_secondary),
      # Infrastructure in Primary Schools
        percent_functional_water_primary = 
          ratio(df$functional_water,  df$is_primary),
        percent_improved_sanitation_primary = 
          ratio(df$improved_sanitation,  df$is_primary),
        percent_phcn_electricity_primary = 
          ratio(df$phcn_electricity,  df$is_primary),
      # Infrastructure in Junior Secondary Schools
        percent_functional_water_js = 
          ratio(df$functional_water,  df$is_junior_secondary),
        percent_improved_sanitation_js = 
          ratio(df$improved_sanitation,  df$is_junior_secondary),
        percent_phcn_electricity_js = 
          ratio(df$phcn_electricity,  df$is_junior_secondary)
)}) 

# Education Facilities summary on the "All sectors" page (narrow table)
edu774_allsectors <- edu774
edu774_allsectors$is_js_multiplecount <- 
        edu774_allsectors$level_of_education %in% c('js_ss', 'js', 'primary_js',
                                                    'primary_and_junior_sec', 
                                                    'junior_and_senior_sec',
                                                    'junior_sec_only', 'primary_js_ss',
                                                    'primary_junior_and_senior_sec')

edu774_allsectors$is_ss_multiplecount <- 
        edu774_allsectors$level_of_education %in% c('junior_and_senior_sec', 'ss',
                                                      'js_ss', 'senior_sec_only',
                                                    'primary_junior_and_senior_sec',
                                                    'primary_js_ss')
edu774_allsectors$gov_managed <- 
        edu774_allsectors$management == 'public'
  
ie2 <- idata.frame(edu774_allsectors)

core_allsectors <- ddply(ie2, .(lga_id), function(df) {
                    data.frame(   
                      num_js_schools_multiplecount = 
                        icount(df$is_js_multiplecount),
                      num_ss_schools_multiplecount = 
                        icount(df$is_ss_multiplecount),
                      percent_teachers_nce_primary = 
                        ratio(df$num_tchrs_with_nce, df$num_tchrs_total, df$management)                        
)}) 

lga_edu_data <- merge(lga_edu_data, lga_edu_data_core, by = "lga_id")
lga_edu_data <- merge(lga_edu_data, core_allsectors, by = "lga_id")

#writing out
saveRDS(lga_edu_data, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Education_LGA_level_774.rds")



