source('base_scripts/InstallFormhub.R')
source('./cleaning_999s/999_functions.R')

merged_education <- edu_total


merged_education$days_no_electricity <- as.numeric(merged_education$days_no_electricity)

merged_education$days_no_water_pastmth <- as.numeric(merged_education$days_no_water_pastmth)

merged_education$flush_toilet_number<- as.numeric(merged_education$flush_toilet_number)

merged_education$flush_toilet_not_working <- as.numeric(merged_education$flush_toilet_not_working)

merged_education$vip_latrine_number <- as.numeric(merged_education$vip_latrine_number)

merged_education$vip_latrine_not_working <- as.numeric(merged_education$vip_latrine_not_working)

merged_education$slab_pit_latrine_number <- as.numeric(merged_education$slab_pit_latrine_number)

cellst(merged_education, 'slab_pit_latrine_number',
       which(merged_education$slab_pit_latrine_number >= 9999), NA_integer_)

merged_education$slab_pit_latrine_not_working <- as.numeric(merged_education$slab_pit_latrine_not_working)

merged_education$open_pit_latrine_number <- as.numeric(merged_education$open_pit_latrine_number)

merged_education$open_pit_latrine_not_working <- as.numeric(merged_education$open_pit_latrine_not_working)

merged_education$bucket_system_number <- as.numeric(merged_education$bucket_system_number)

merged_education$bucket_system_not_working <- as.numeric(merged_education$bucket_system_not_working)

merged_education$other_toilets_number <- as.numeric(merged_education$other_toilets_number)

merged_education$other_toilets_not_working <- as.numeric(merged_education$other_toilets_not_working)

merged_education$times_building_cleaned_lastmth <- as.numeric(merged_education$times_building_cleaned_lastmth)

merged_education$times_trash_disposed_lastmth <- as.numeric(merged_education$times_trash_disposed_lastmth)

merged_education$num_tchrs_male_full_time <- as.numeric(merged_education$num_tchrs_male_full_time)

merged_education$num_tchrs_male_part_time <- as.numeric(merged_education$num_tchrs_male_part_time)

merged_education$num_tchrs_female_full_time <- as.numeric(merged_education$num_tchrs_female_full_time)

merged_education$num_tchrs_female_part_time <- as.numeric(merged_education$num_tchrs_female_part_time)

cellst(merged_education, 'num_tchrs_female_part_time',
       which(merged_education$num_tchrs_female_part_time >= 999), NA_integer_)

merged_education$tchrs_male_below_ssce <- as.numeric(merged_education$tchrs_male_below_ssce)
cellst(merged_education, 'tchrs_male_below_ssce',which(merged_education$tchrs_male_below_ssce >900), NA_integer_) 

merged_education$tchrs_female_below_ssce <- as.numeric(merged_education$tchrs_female_below_ssce)
cellst(merged_education, 'tchrs_female_below_ssce',which(merged_education$tchrs_female_below_ssce >900), NA_integer_) 

merged_education$tchrs_male_ssce_wasc <- as.numeric(merged_education$tchrs_male_ssce_wasc)
cellst(merged_education, 'tchrs_male_ssce_wasc',which(merged_education$tchrs_male_ssce_wasc == 99), NA_integer_) #one data entry was 99  

merged_education$tchrs_female_ssce_wasc <- as.numeric(merged_education$tchrs_female_ssce_wasc)
cellst(merged_education, 'tchrs_female_ssce_wasc',which(merged_education$tchrs_female_ssce_wasc == 999), NA_integer_) #one data entry was 999

merged_education$tchrs_male_grade2 <- as.numeric(merged_education$tchrs_male_grade2)
cellst(merged_education, 'tchrs_male_grade2',which(merged_education$tchrs_male_grade2 == 9999), NA_integer_) #one data entry was 9999

merged_education$tchrs_female_grade2 <- as.numeric(merged_education$tchrs_female_grade2)
cellst(merged_education, 'tchrs_female_grade2',which(merged_education$tchrs_female_grade2 == 9999), NA_integer_) #one data entry was 9999

merged_education$tchrs_male_ond <- as.numeric(merged_education$tchrs_male_ond)
cellst(merged_education, 'tchrs_male_ond',which(merged_education$tchrs_male_ond == 999), NA_integer_) #one data entry was 999

merged_education$tchrs_female_ond <- as.numeric(merged_education$tchrs_female_ond)
cellst(merged_education, 'tchrs_female_ond',which(merged_education$tchrs_female_ond == 999), NA_integer_) #one data entry was 999

merged_education$tchrs_male_nce <- as.numeric(merged_education$tchrs_male_nce)
cellst(merged_education, 'tchrs_male_nce',which(merged_education$tchrs_male_nce == 999), NA_integer_)
cellst(merged_education, 'tchrs_male_nce',which(merged_education$tchrs_male_nce < 0), NA_integer_)

merged_education$tchrs_female_nce <- as.numeric(merged_education$tchrs_female_nce)
cellst(merged_education, 'tchrs_female_nce',which(merged_education$tchrs_female_nce == 9999), NA_integer_)
cellst(merged_education, 'tchrs_female_nce',which(merged_education$tchrs_female_nce < 0), NA_integer_)

merged_education$tchrs_male_pgde <- as.numeric(merged_education$tchrs_male_pgde)
cellst(merged_education, 'tchrs_male_pgde',which(merged_education$tchrs_male_pgde == 999), NA_integer_)

merged_education$tchrs_female_pgde <- as.numeric(merged_education$tchrs_female_pgde)
cellst(merged_education, 'tchrs_female_pgde', which(merged_education$tchrs_female_pgde == 9999), NA_integer_)

merged_education$tchrs_male_b_ed <- as.numeric(merged_education$tchrs_male_b_ed)
cellst(merged_education, 'tchrs_male_b_ed', which(merged_education$tchrs_male_b_ed == 998), NA_integer_)

merged_education$tchrs_female_b_ed <- as.numeric(merged_education$tchrs_female_b_ed)
cellst(merged_education, 'tchrs_female_b_ed', which(merged_education$tchrs_female_b_ed == 999), NA_integer_)

merged_education$tchrs_male_other_w_nce <- as.numeric(merged_education$tchrs_male_other_w_nce)
cellst(merged_education, 'tchrs_male_other_w_nce', which(merged_education$tchrs_male_other_w_nce == 99), NA_integer_) #99 considered as outlier

merged_education$tchrs_female_other_w_nce <- as.numeric(merged_education$tchrs_female_other_w_nce)
cellst(merged_education, 'tchrs_female_other_w_nce', which(merged_education$tchrs_female_other_w_nce == 999), NA_integer_)

merged_education$tchrs_female_other_wo_nce <- as.numeric(merged_education$tchrs_female_other_wo_nce)
cellst(merged_education, 'tchrs_female_other_wo_nce', which(merged_education$tchrs_female_other_wo_nce == 999), NA_integer_)

merged_education$num_sr_staff_male <-as.numeric(merged_education$num_sr_staff_male)

merged_education$num_sr_staff_female <-as.numeric(merged_education$num_sr_staff_female)

merged_education$num_jr_staff_male <-as.numeric(merged_education$num_jr_staff_male)

merged_education$num_jr_staff_female <-as.numeric(merged_education$num_jr_staff_female)

merged_education$days_school_understaffed <- as.numeric(merged_education$days_school_understaffed)

merged_education$days_school_understaffed_closed <- as.numeric(merged_education$days_school_understaffed_closed)

merged_education$school_max_num_students <- as.numeric(merged_education$school_max_num_students) #"I don't know" has eliminated
cellst(merged_education, 'school_max_num_students', which(merged_education$school_max_num_students == 9999), NA_integer_) #9999 has eliminated (there is 10800)

merged_education$admit_more_num_students <- as.numeric(merged_education$admit_more_num_students)
cellst(merged_education, 'admit_more_num_students', which(merged_education$admit_more_num_students == 9999), NA_integer_)

merged_education$materials_fee <- as.numeric(merged_education$materials_fee)

merged_education$uniforms_fee <- as.numeric(merged_education$uniforms_fee)
cellst(merged_education, 'uniforms_fee', which(merged_education$uniforms_fee > 900000), NA_integer_) #one extreme value

merged_education$sports_fee <- as.numeric(merged_education$sports_fee)

merged_education$sports_fee_exempt_yn[merged_education$sports_fee_exempt_yn != "yes" & merged_education$sports_fee_exempt_yn != "no"] <- NA #unless yes or no, eliminate

merged_education$num_students_exempt <- as.numeric(merged_education$num_students_exempt)  # answer of "all" and "o" eliminated
cellst(merged_education, 'num_students_exempt', which(merged_education$num_students_exempt > 8000), NA_integer_) #20 of "9999" has eliminated

merged_education$in_kind_fees_yn[merged_education$in_kind_fees_yn != "yes" & merged_education$in_kind_fees_yn != "no"] <- NA

merged_education$booklist_per_class_yn[merged_education$booklist_per_class_yn != "yes" & merged_education$booklist_per_class_yn != "no"] <- NA

merged_education$annual_budget_amt_received <- as.numeric(merged_education$annual_budget_amt_received)

merged_education$num_tchrs_paid_fed_gov <- as.numeric(merged_education$num_tchrs_paid_fed_gov)

cellst(merged_education, 'num_tchrs_paid_fed_gov',
       which(merged_education$num_tchrs_paid_fed_gov >= 9999), NA_integer_)

merged_education$num_tchrs_payrl_st_gov <- as.numeric(merged_education$num_tchrs_payrl_st_gov)
cellst(merged_education, 'num_tchrs_payrl_st_gov', which(merged_education$num_tchrs_payrl_st_gov >= 9999), NA_integer_)

merged_education$num_tchrs_othr_payrl_st_gov <- as.numeric(merged_education$num_tchrs_othr_payrl_st_gov)
cellst(merged_education, 'num_tchrs_othr_payrl_st_gov',
       which(merged_education$num_tchrs_othr_payrl_st_gov >= 999), NA_integer_)

merged_education$num_tchrs_paid_loc_gov <- as.numeric(merged_education$num_tchrs_paid_loc_gov)
cellst(merged_education, 'num_tchrs_paid_loc_gov',
       which(merged_education$num_tchrs_paid_loc_gov >= 999), NA_integer_)

merged_education$num_tchrs_paid_prvt_for_profit <- as.numeric(merged_education$num_tchrs_paid_prvt_for_profit)
cellst(merged_education, 'num_tchrs_paid_prvt_for_profit',
       which(merged_education$num_tchrs_paid_prvt_for_profit >= 999), NA_integer_)

merged_education$num_tchrs_paid_prvt_non_profit <- as.numeric(merged_education$num_tchrs_paid_prvt_non_profit)

merged_education$num_tchrs_paid_other_src <- as.numeric(merged_education$num_tchrs_paid_other_src)
cellst(merged_education, 'num_tchrs_paid_other_src', which(merged_education$num_tchrs_paid_other_src == 999|merged_education$num_tchrs_paid_other_src == 9999), NA_integer_) #999 and 9999

merged_education$num_tchrs_no_salary <- as.numeric(merged_education$num_tchrs_no_salary)
cellst(merged_education, 'num_tchrs_no_salary', which(merged_education$num_tchrs_no_salary >= 999), NA_integer_)

merged_education$num_students_scholarship <- as.numeric(merged_education$num_students_scholarship)
cellst(merged_education, 'num_students_scholarship', which(merged_education$num_students_scholarship > 9000), NA_integer_)

merged_education$scholarship_amt <- as.numeric(merged_education$scholarship_amt)

merged_education$num_library_materials <- as.numeric(merged_education$num_library_materials) # there were 999s, but reasonable data

merged_education$times_new_materials_added <- as.numeric(merged_education$times_new_materials_added)
cellst(merged_education, 'times_new_materials_added', which(merged_education$times_new_materials_added == 999), NA_integer_)


merged_education$num_classrms_unused <- as.numeric(merged_education$num_classrms_unused)
cellst(merged_education, 'num_classrms_unused', which(merged_education$num_classrms_unused >= 999), NA_integer_)

merged_education$num_classrooms_multiple_use <- as.numeric(merged_education$num_classrooms_multiple_use)
cellst(merged_education, 'num_classrooms_multiple_use', which(merged_education$num_classrooms_multiple_use >= 999), NA_integer_)

merged_education$num_other_rooms <- as.numeric(merged_education$num_other_rooms)
cellst(merged_education,'num_other_rooms', which(merged_education$num_other_rooms >= 999), NA_integer_)

merged_education$num_sections_pry1 <- as.numeric(merged_education$num_sections_pry1)
cellst(merged_education,'num_sections_pry1', which(merged_education$num_sections_pry1 >= 999), NA_integer_)

merged_education$num_sections_pry2 <- as.numeric(merged_education$num_sections_pry2)
cellst(merged_education,'num_sections_pry2', which(merged_education$num_sections_pry2 >= 999), NA_integer_)

merged_education$num_sections_pry3 <- as.numeric(merged_education$num_sections_pry3)
cellst(merged_education,'num_sections_pry3', which(merged_education$num_sections_pry3 >= 999), NA_integer_)

merged_education$num_sections_pry4 <- as.numeric(merged_education$num_sections_pry4)
cellst(merged_education,'num_sections_pry4', which(merged_education$num_sections_pry4 >= 999), NA_integer_)

merged_education$num_sections_pry5 <- as.numeric(merged_education$num_sections_pry5)
cellst(merged_education,'num_sections_pry5', which(merged_education$num_sections_pry5 >= 999), NA_integer_)

merged_education$num_sections_pry6 <- as.numeric(merged_education$num_sections_pry6)
cellst(merged_education,'num_sections_pry6', which(merged_education$num_sections_pry6 >= 999), NA_integer_)

merged_education$num_sections_js1 <- as.numeric(merged_education$num_sections_js1)
cellst(merged_education,'num_sections_js1', which(merged_education$num_sections_js1 >= 999), NA_integer_) # 8999 considered as typo of 9999

merged_education$num_sections_js2 <- as.numeric(merged_education$num_sections_js2)
cellst(merged_education,'num_sections_js2', which(merged_education$num_sections_js2 >= 899), NA_integer_) # 899 considered as typo of 999

merged_education$num_sections_js3 <- as.numeric(merged_education$num_sections_js3)
cellst(merged_education,'num_sections_js3', which(merged_education$num_sections_js3 >= 899), NA_integer_) # 899 considered as typo of 999

merged_education$num_attached_benches <- as.numeric(merged_education$num_attached_benches)
cellst(merged_education,'num_attached_benches', which(merged_education$num_attached_benches < 0), NA_integer_)
cellst(merged_education,'num_attached_benches', which(merged_education$num_attached_benches == 9999), NA_integer_) #considered 999 as reasonable number

merged_education$num_attached_benches_unused <- as.numeric(merged_education$num_attached_benches_unused)
cellst(merged_education,'num_attached_benches_unused', which(merged_education$num_attached_benches_unused >= 999), NA_integer_) #considered 99 as reasonable number

merged_education$num_unattached_benches <- as.numeric(merged_education$num_unattached_benches)

merged_education$num_unattached_benches_unused <- as.numeric(merged_education$num_unattached_benches_unused)

merged_education$num_unattached_desks_unused <- as.numeric(merged_education$num_unattached_desks_unused)

merged_education$num_textbooks_english <- as.numeric(merged_education$num_textbooks_english)

merged_education$num_textbooks_math <- as.numeric(merged_education$num_textbooks_math)

merged_education$num_textbooks_social_sci <- as.numeric(merged_education$num_textbooks_social_sci)

merged_education$num_exercise_books <- as.numeric(merged_education$num_exercise_books) # 2 of "dont know"

#extra

merged_education$tchrs_male_other_wo_nce <- as.numeric(merged_education$tchrs_male_other_wo_nce)
cellst(merged_education,'tchrs_male_other_wo_nce', which(merged_education$tchrs_male_other_wo_nce == 999), NA_integer_)

merged_education$num_tchrs_attended_training <- as.numeric(merged_education$num_tchrs_attended_training)
cellst(merged_education,'num_tchrs_attended_training', which(merged_education$num_tchrs_attended_training < 0), NA_integer_)

merged_education$num_tchrs_attended_last_day <- as.numeric(merged_education$num_tchrs_attended_last_day)
cellst(merged_education,'num_tchrs_attended_last_day', which(merged_education$num_tchrs_attended_last_day < 0), NA_integer_)

merged_education$grid_months_broken <- as.numeric(merged_education$grid_months_broken)
cellst(merged_education, 'grid_months_broken', which(merged_education$grid_months_broken >= 9999), NA_integer_)




####
merged_education$new_stdnts_enroll_fee <- as.numeric(merged_education$new_stdnts_enroll_fee)
merged_education$days_no_potable_water <- as.numeric(merged_education$days_no_potable_water)

merged_education$cont_stdnts_enroll_fee <- as.numeric(merged_education$cont_stdnts_enroll_fee)
cellst(merged_education, 'cont_stdnts_enroll_fee', which(merged_education$cont_stdnts_enroll_fee < 0), NA_integer_)

merged_education$textbooks_fee <- as.numeric(merged_education$textbooks_fee)
merged_education$transport_fee <- as.numeric(merged_education$transport_fee)
merged_education$exams_fee <- as.numeric(merged_education$exams_fee)
merged_education$pta_fee <- as.numeric(merged_education$pta_fee)

merged_education$num_desks <- as.numeric(merged_education$num_desks)
merged_education$num_science_textbook_pry <- as.numeric(merged_education$num_science_textbook_pry)


merged_education$num_students_frthr_than_3km <- as.numeric(merged_education$num_students_frthr_than_3km)

merged_education$num_students_male <- as.numeric(merged_education$num_students_male)

merged_education$num_students_female <- as.numeric(merged_education$num_students_female)

merged_education$num_students_total <- as.numeric(merged_education$num_students_total)

merged_education$times_tchr_pay_delay_pastyr <- as.numeric(merged_education$times_tchr_pay_delay_pastyr)

merged_education$times_tchr_pay_miss_pastyr <- as.numeric(merged_education$times_tchr_pay_miss_pastyr)















##knocking out 999 values


cellst(merged_education,
       c('num_students_male','num_students_female',
         'num_students_total'),
       which(merged_education$num_students_male %in% c(9991, 99919, 9999)), NA_integer_) # 10300 stays

cellst(merged_education,
       c('num_students_male','num_students_female',
         'num_students_total'),
       which(merged_education$num_students_female %in% c(9999, 999999)), NA_integer_)

cellst(merged_education, 'num_pry_female',
       which(merged_education$num_pry_female > 9999), NA_integer_)
cellst(merged_education, 'num_pry_male',
       which(merged_education$num_pry_male > 9990), NA_integer_)
cellst(merged_education, 'num_js_male',
       which(merged_education$num_js_male >= 9989), NA_integer_)     
cellst(merged_education, 'num_js_female',
       which(merged_education$num_js_female >= 9989), NA_integer_)     
cellst(merged_education, 'num_toilet_boy',
       which(merged_education$num_toilet_boy >= 999), NA_integer_)
cellst(merged_education, 'num_toilet_girl',
       which(merged_education$num_toilet_girl >= 999), NA_integer_)
cellst(merged_education, 'num_toilet_both',
       which(merged_education$num_toilet_both >= 990), NA_integer_)
cellst(merged_education, 'num_tchrs_male',
       which(merged_education$num_tchrs_male %in% c(999, 9993, 9996, 9999, 99922)), NA_integer_)
cellst(merged_education, 'num_tchrs_female',
       which(merged_education$num_tchrs_female %in% c(999, 9991, 9999)), NA_integer_)
cellst(merged_education, 'num_math_textbook_js',
       which(merged_education$num_math_textbook_js >= 9999), NA_integer_)
cellst(merged_education, 'num_sr_staff_total',
       which(merged_education$num_sr_staff_total >= 999 | merged_education$num_sr_staff_total <0), NA_integer_)
cellst(merged_education, 'num_jr_staff_total',
       which(merged_education$num_jr_staff_total >= 999 | merged_education$num_jr_staff_total <0), NA_integer_)
cellst(merged_education, 'num_tchrs_total',
       which(merged_education$num_tchrs_total %in% c(999, 19998, 1998) |  merged_education$num_tchrs_total <0), NA_integer_)
cellst(merged_education, 'num_classrms_need_min_repairs',
       which(merged_education$num_classrms_need_min_repairs >= 999), NA_integer_)
cellst(merged_education, 'num_classrms_need_maj_repairs',
       which(merged_education$num_classrms_need_maj_repairs >= 990), NA_integer_)
cellst(merged_education, 'days_no_potable_water',
       which(merged_education$days_no_potable_water < 0 |  merged_education$days_no_potable_water >=999), NA_integer_)
cellst(merged_education, 'num_ss_female',
       which(merged_education$num_ss_female >= 9990 ), NA_integer_)
cellst(merged_education, c('num_ss_female', 'num_ss_male', 
                           'num_ss_total'),
       which(merged_education$num_ss_total > 19000 ), NA_integer_)
cellst(merged_education, 'km_to_secondary_school',
       which(merged_education$km_to_secondary_school > 800 ), NA_integer_)
cellst(merged_education, 'km_to_catchment_area',
       which(merged_education$km_to_catchment_area >= 999 ), NA_integer_)
cellst(merged_education, 'num_students_frthr_than_3km', 
       which(merged_education$num_students_frthr_than_3km == 999 | merged_education$num_students_frthr_than_3km >= 9000), 
       NA_integer_) # 999 considered as outlier

#individual cells
cellst(merged_education, 'num_tchrs_qualification.num_tchrs_w_nce',
       which(merged_education$uuid =='6c7dd1bf-82c7-46e1-8ab0-ad85baada470' | merged_education$uuid =='41dff099-4e14-4fdf-9f49-300b7c1f5c8f' ), NA_integer_)
cellst(merged_education, c('num_tchrs_total', 'num_tchrs_female', 'num_students_total', 'num_students_male', 'num_tchrs_male', 'num_students_female'),
       which(merged_education$uuid =='ab52fe42-1525-46dd-945a-f5fef4566c16'), NA_integer_)
cellst(merged_education, 'num_classrms_total',
       which(merged_education$uuid =='fcdbb827-943b-40b5-bc59-19106759bf2c' | merged_education$uuid =='d17bde2e-a6ce-4b0d-87fe-ab39e61bfe8e'), NA_integer_)
cellst(merged_education, 'num_students_total',
       which(merged_education$uuid =='cad7e54b-b5cc-4768-9c56-18a8f3f1023f'), NA_integer_)
cellst(merged_education, 'num_students_total',
       which(merged_education$num_students_total =='9699'), NA_integer_)
##total
saveRDS(merged_education, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Education_661_999Cleaned.rds")






