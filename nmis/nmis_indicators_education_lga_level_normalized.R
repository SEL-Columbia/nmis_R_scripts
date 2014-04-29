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
                  sum(df$pj, na.rm = TRUE),
              classrooms_total =
                  sum(df$num_classrms_total, na.rm = TRUE),
              schools_improved_water_supply = 
                  mean(df$improved_water_supply, na.rm=T), 
              schools_improved_sanitation =
                  mean(df$improved_sanitation, na.rm=T),
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
                  sum(df$num_tchr_full_time, na.rm = TRUE),
              teachers_qualified =
                  sum(df$num_tchrs_w_nce, na.rm = TRUE),
              teachers_attended_training =
                  sum(df$num_tchrs_attended_training, na.rm = TRUE),
              txt_pupil_ratio = 
                  ratio(df$num_textbooks, df$num_students_total,df$pj),
              percent_teaching_guides = 
                  mean(df$teacher_guide_yn, na.rm=T),
              schools_use_teaching_aids = 
                  sum(df$teacher_guide_yn, na.rm = TRUE),
              num_primary_schools = 
                  sum(df$is_primary, na.rm = TRUE),
              num_junior_secondary_schools = 
                  sum(df$is_junior_secondary, na.rm = TRUE),
              num_senior_secondary_schools = 
                  sum(df$level_of_education %in% c('senior_sec_only','ss'), na.rm = TRUE ),
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
              # XXX output reduceable columns
              pupil_toilet_ratio_primary_num = 
                  ratio_num(df$num_students_total, df$num_toilets_total, df$is_primary),
              pupil_toilet_ratio_primary_den = 
                  ratio_den(df$num_students_total, df$num_toilets_total, df$is_primary),
              # XXX 
        
              pupil_toilet_ratio_js= 
                  ratio(df$num_students_total, df$num_toilets_total, df$is_junior_secondary),              
              # XXX output reduceable columns
              pupil_toilet_ratio_js_num = 
                  ratio_num(df$num_students_total, df$num_toilets_total, df$is_junior_secondary),
              pupil_toilet_ratio_js_den = 
                  ratio_den(df$num_students_total, df$num_toilets_total, df$is_junior_secondary),
              # XXX 
        
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
              # XXX output reduceable columns
              student_classroom_ratio_lga_primary_num = 
                  ratio_num(df$num_students_total, df$num_classrms_total, df$is_primary),
              student_classroom_ratio_lga_primary_den = 
                  ratio_den(df$num_students_total, df$num_classrms_total, df$is_primary),
              # XXX 
        
              student_classroom_ratio_lga_js =
                  ratio(df$num_students_total, df$num_classrms_total, df$is_junior_secondary),
              # XXX output reduceable columns
              student_classroom_ratio_lga_js_num = 
                  ratio_num(df$num_students_total, df$num_classrms_total, df$is_junior_secondary),
              student_classroom_ratio_lga_js_den = 
                  ratio_den(df$num_students_total, df$num_classrms_total, df$is_junior_secondary),
              # XXX 

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
              pupil_teachers_ratio_lga_primary =
                  ratio(df$num_students_total, df$num_tchr_full_time, df$is_primary),
                #XXX
              pupil_teachers_ratio_lga_primary_num =
                  ratio_num(df$num_students_total, df$num_tchr_full_time, df$is_primary),
              pupil_teachers_ratio_lga_primary_den =
                  ratio_den(df$num_students_total, df$num_tchr_full_time, df$is_primary),
                #XXX
              pupil_teachers_ratio_lga_js =
                  ratio(df$num_students_total, df$num_tchr_full_time, df$is_junior_secondary),
                #XXX
                #XXX
              pupil_teachers_ratio_lga_js_num =
                  ratio_num(df$num_students_total, df$num_tchr_full_time, df$is_junior_secondary),
              pupil_teachers_ratio_lga_js_den =
                  ratio_den(df$num_students_total, df$num_tchr_full_time, df$is_junior_secondary),
                #XXX
              teacher_nonteachingstaff_ratio_lga_primary =
                  ratio(df$num_tchr_full_time, df$num_sr_staff_total + df$num_jr_staff_total, df$is_primary),
              teacher_nonteachingstaff_ratio_lga_juniorsec =
                  ratio(df$num_tchr_full_time, df$num_sr_staff_total + df$num_jr_staff_total, df$is_junior_secondary),
              proportion_teachers_nce_primary =
                  ratio(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_primary),
                #XXX
              proportion_teachers_nce_primary_num =
                  ratio_num(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_primary),
              proportion_teachers_nce_primary_den =
                  ratio_den(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_primary),
                #XXX
              proportion_teachers_nce_js =
                  ratio(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_junior_secondary),
                #XXX
              proportion_teachers_nce_js_num =
                  ratio_num(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_junior_secondary),
              proportion_teachers_nce_js_den =
                #XXX
                #XXX
              teacher_nonteachingstaff_ratio_lga_primary =
                  ratio(df$num_tchr_full_time, df$num_sr_staff_total + df$num_jr_staff_total, df$is_primary),
              teacher_nonteachingstaff_ratio_lga_juniorsec =
                  ratio(df$num_tchr_full_time, df$num_sr_staff_total + df$num_jr_staff_total, df$is_junior_secondary),
              proportion_teachers_nce_primary =
                  ratio(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_primary),
                #XXX
              proportion_teachers_nce_primary_num =
                  ratio_num(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_primary),
              proportion_teachers_nce_primary_den =
                  ratio_den(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_primary),
                #XXX
              proportion_teachers_nce_js =
                  ratio(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_junior_secondary),
                #XXX
              proportion_teachers_nce_js_num =
                  ratio_num(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_junior_secondary),
              proportion_teachers_nce_js_den =
                #XXX
              teacher_nonteachingstaff_ratio_lga_primary =
                  ratio(df$num_tchr_full_time, df$num_sr_staff_total + df$num_jr_staff_total, df$is_primary),
              teacher_nonteachingstaff_ratio_lga_juniorsec =
                  ratio(df$num_tchr_full_time, df$num_sr_staff_total + df$num_jr_staff_total, df$is_junior_secondary),
              proportion_teachers_nce_primary =
                  ratio(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_primary),
                #XXX
              proportion_teachers_nce_primary_num =
                  ratio_num(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_primary),
              proportion_teachers_nce_primary_den =
                  ratio_den(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_primary),
                #XXX
              proportion_teachers_nce_js =
                  ratio(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_junior_secondary),
                #XXX
              proportion_teachers_nce_js_num =
                  ratio_num(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_junior_secondary),
              proportion_teachers_nce_js_den =
                  ratio_den(df$num_tchrs_with_nce, df$num_tchr_full_time, df$is_junior_secondary),
                #XXX
              proportion_teachers_training_last_year_primary = 
                  ratio(df$num_tchrs_attended_training, df$num_tchr_full_time, df$is_primary),
              proportion_teachers_training_last_year_juniorsec = 
                  ratio(df$num_tchrs_attended_training, df$num_tchr_full_time, df$is_junior_secondary),
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
                  sum(df$level_of_education %in% c('preprimary_only', 'preprimary'), na.rm = TRUE),              
              num_preprimary_primary_level = 
                  sum(df$level_of_education %in% c('preprimary_and_primary', 'preprimary_primary'), na.rm = TRUE),              
              num_primary_level =
                  sum(df$level_of_education %in% c('primary_only', 'primary'), na.rm = TRUE),              
              num_primary_js_level =  
                  sum(df$level_of_education %in% c('primary_and_junior_sec', 'primary_js'), na.rm = TRUE),              
              num_js_level =
                  sum(df$level_of_education %in% c('juniors_sec_only', 'js'), na.rm = TRUE),              
              num_js_ss_level = 
                  sum(df$level_of_education %in% c('junior_and_senior_sec', 'js_ss'), na.rm = TRUE),              
              num_ss_level = 
                  sum(df$level_of_education %in% c('senior_sec_only', 'ss'), na.rm = TRUE),            
              num_primary_js_ss_level = 
                  sum(df$level_of_education %in% c('primary_junior_and_senior_sec', 'primary_js_ss'), na.rm = TRUE),
              num_other_level = sum(df$level_of_education %in% c('science_technical', 'vocational', 
                                                                      'adult_lit','adult_ed', 'adult_vocational',
                                                                      'vocational_post_primary', 'vocational_post_secondary'), na.rm = TRUE),
              student_teacher_ratio_lga = 
                  ratio(df$num_students_total, df$num_tchr_full_time),              
              proportion_teachers_nce = 
                  ratio(df$num_tchrs_with_nce, df$num_tchr_full_time),
              number_classrooms_need_major_repair = 
                  sum(df$num_classrms_need_maj_repairs, na.rm = TRUE)
              )})                   
                  
###### core indicator calculations
lga_edu_data_core <- ddply(ie, .(lga_id), function(df) {
  data.frame(   
      # Facilities
        percent_management_public = 
          sum(df$management == 'public', na.rm = TRUE) / count_na_rm(df$management),
        percent_natl_curriculum =
          mean(df$natl_curriculum_yn, na.rm=T),
      # General information for Primary Schols
        percent_management_public_primary = 
          bool_proportion(df$management == 'public', df$is_primary),
        # XXX output reduceable columns
        percent_management_public_primary_sum = 
            sum(df[df$is_primary,]$management == 'public', na.rm = TRUE),
        percent_management_public_primary_cnt = count_na_rm(df$management[df$is_primary]),
        # XXX 
        
        percent_natl_curriculum_primary =
          bool_proportion(df$natl_curriculum_yn, df$is_primary),
        # XXX output reduceable columns
        percent_natl_curriculum_primary_sum = 
            sum(df[df$is_primary,]$natl_curriculum_yn, na.rm=T),
        percent_natl_curriculum_primary_cnt = count_na_rm(df$natl_curriculum_yn[df$is_primary]),
        # XXX 
        
        num_school_1kmplus_secondary_school = 
          bool_proportion(df$school_1kmplus_secondary_school, df$is_primary),
        avg_num_students_primary =
          ratio(df$num_students_total,  df$is_primary),
        # xxx output reduceable columns
        avg_num_students_primary_sum = 
            sum(df$num_students_total[df$is_primary], na.rm=t),
        avg_num_students_primary_cnt = count_na_rm(df$num_students_total[df$is_primary]),
        # xxx 
      
        avg_num_tchrs_primary = 
          ratio(df$num_tchr_full_time,  df$is_primary),
        # XXX output reduceable columns
        avg_num_tchrs_primary_sum = 
              sum(df$num_tchr_full_time[df$is_primary], na.rm=T),
        avg_num_tchrs_primary_cnt = count_na_rm(df$num_tchr_full_time[df$is_primary]),
        # XXX 
      
        avg_num_classrms_primary = 
          ratio(df$num_classrms_total,  df$is_primary),
        # XXX output reduceable columns
        avg_num_classrms_primary_sum = 
              sum(df$num_classrms_total[df$is_primary], na.rm=T),
        avg_num_classrms_primary_cnt = count_na_rm(df$num_classrms_total[df$is_primary]),
        # XXX 
        
        avg_num_toilets_primary = 
          ratio(df$num_toilets_total,  df$is_primary),
        # XXX output reduceable columns
        avg_num_toilets_primary_sum = 
              sum(df$num_toilets_total[df$is_primary], na.rm=T),
        avg_num_toilets_primary_cnt = count_na_rm(df$num_toilets_total[df$is_primary]),
        # XXX 

      # General information for Junior Secondary Schools
        percent_management_public_js = 
          bool_proportion(df$management == 'public', df$is_junior_secondary),
        # XXX output reduceable columns
        percent_management_public_js_sum = 
            sum(df[df$is_junior_secondary,]$management == 'public', na.rm = TRUE),
        percent_management_public_js_cnt = count_na_rm(df$management[df$is_junior_secondary]),
        # XXX 
        
        percent_natl_curriculum_js =
          bool_proportion(df$natl_curriculum_yn, df$is_junior_secondary),
        # XXX output reduceable columns
        percent_natl_curriculum_js_sum = 
            sum(df[df$is_junior_secondary,]$natl_curriculum_yn, na.rm=T),
        percent_natl_curriculum_js_cnt = count_na_rm(df$natl_curriculum_yn[df$is_junior_secondary]),
        # XXX 
        
        avg_num_students_js =
          ratio(df$num_students_total,  df$is_junior_secondary),
        # xxx output reduceable columns
        avg_num_students_js_sum = 
            sum(df$num_students_total[df$is_junior_secondary], na.rm=t),
        avg_num_students_js_cnt = count_na_rm(df$num_students_total[df$is_junior_secondary]),
        # xxx 
      
        avg_num_tchrs_js = 
          ratio(df$num_tchr_full_time,  df$is_junior_secondary),
        # XXX output reduceable columns
        avg_num_tchrs_js_sum = 
              sum(df$num_tchr_full_time[df$is_junior_secondary], na.rm=T),
        avg_num_tchrs_js_cnt = count_na_rm(df$num_tchr_full_time[df$is_junior_secondary]),
        # XXX 
      
        avg_num_classrms_js = 
          ratio(df$num_classrms_total,  df$is_junior_secondary),
        # XXX output reduceable columns
        avg_num_classrms_js_sum = 
              sum(df$num_classrms_total[df$is_junior_secondary], na.rm=T),
        avg_num_classrms_js_cnt = count_na_rm(df$num_classrms_total[df$is_junior_secondary]),
        # XXX 
        
        avg_num_toilets_js = 
          ratio(df$num_toilets_total,  df$is_junior_secondary),
        # XXX output reduceable columns
        avg_num_toilets_js_sum = 
              sum(df$num_toilets_total[df$is_junior_secondary], na.rm=T),
        avg_num_toilets_js_cnt = count_na_rm(df$num_toilets_total[df$is_junior_secondary]),
        # XXX 

      # Infrastructure in Primary Schools
        percent_functional_water_primary = 
          bool_proportion(df$functional_water,  df$is_primary),
        # XXX output reduceable columns
        percent_functional_water_primary_sum = 
              sum(df$functional_water[df$is_primary], na.rm=T),
        percent_functional_water_primary_cnt = count_na_rm(df$functional_water[df$is_primary]),
        # XXX 
      
        percent_improved_sanitation_primary = 
          bool_proportion(df$improved_sanitation,  df$is_primary),
        # XXX output reduceable columns
        percent_improved_samitation_primary_sum = 
              sum(df$improved_samitation[df$is_primary], na.rm=T),
        percent_improved_samitation_primary_cnt = count_na_rm(df$improved_samitation[df$is_primary]),
        # XXX 
      
        percent_phcn_electricity_primary = 
          bool_proportion(df$phcn_electricity,  df$is_primary),
        # XXX output reduceable columns
        percent_phcn_electricity_primary_sum = 
              sum(df$phcn_electricity[df$is_primary], na.rm=T),
        percent_phcn_electricity_primary_cnt = count_na_rm(df$phcn_electricity[df$is_primary]),
        # XXX 
      
      # Infrastructure in Junior Secondary Schools
        percent_functional_water_js = 
          bool_proportion(df$functional_water,  df$is_junior_secondary),
        # XXX output reduceable columns
        percent_functional_water_js_sum = 
              sum(df$functional_water[df$is_junior_secondary], na.rm=T),
        percent_functional_water_js_cnt = count_na_rm(df$functional_water[df$is_junior_secondary]),
        # XXX 
      
        percent_improved_sanitation_js = 
          bool_proportion(df$improved_sanitation,  df$is_junior_secondary),
        # XXX output reduceable columns
        percent_improved_samitation_js_sum = 
              sum(df$improved_samitation[df$is_junior_secondary], na.rm=T),
        percent_improved_samitation_js_cnt = count_na_rm(df$improved_samitation[df$is_junior_secondary]),
        # XXX 
      
        percent_phcn_electricity_js = 
          bool_proportion(df$phcn_electricity,  df$is_junior_secondary)
        # XXX output reduceable columns
        percent_phcn_electricity_js_sum = 
              sum(df$phcn_electricity[df$is_junior_secondary], na.rm=T),
        percent_phcn_electricity_js_cnt = count_na_rm(df$phcn_electricity[df$is_junior_secondary]),
        # XXX 
      
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

edu774_allsectors$gov_managed <- edu774_allsectors$management == 'public'
  
ie2 <- idata.frame(edu774_allsectors)

core_allsectors <- ddply(ie2, .(lga_id), function(df) {
                    data.frame(   
                      num_js_schools_multiplecount = 
                        sum(df$is_js_multiplecount, na.rm = TRUE),
                      num_ss_schools_multiplecount = 
                        sum(df$is_ss_multiplecount, na.rm = TRUE),
                      percent_teachers_nce_primary = 
                        ratio(df$num_tchrs_with_nce, df$num_tchr_full_time, df$management)                        
)}) 

lga_edu_data <- merge(lga_edu_data, lga_edu_data_core, by = "lga_id")
lga_edu_data <- merge(lga_edu_data, core_allsectors, by = "lga_id")

#writing out
saveRDS(lga_edu_data, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Education_LGA_level_774.rds")



