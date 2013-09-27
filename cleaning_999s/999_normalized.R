source('base_scripts/InstallFormhub.R')
source('./cleaning_999s/999_functions.R')

e <- edu_total


e$days_no_electricity <- as.numeric(e$days_no_electricity)

e$days_no_water_pastmth <- as.numeric(e$days_no_water_pastmth)

e$flush_toilet_number<- as.numeric(e$flush_toilet_number)

e$flush_toilet_not_working <- as.numeric(e$flush_toilet_not_working)

e$vip_latrine_number <- as.numeric(e$vip_latrine_number)

e$vip_latrine_not_working <- as.numeric(e$vip_latrine_not_working)

e$slab_pit_latrine_number <- as.numeric(e$slab_pit_latrine_number)

cellst(e, 'slab_pit_latrine_number',
       which(e$slab_pit_latrine_number >= 9999), NA_integer_)

e$slab_pit_latrine_not_working <- as.numeric(e$slab_pit_latrine_not_working)

e$open_pit_latrine_number <- as.numeric(e$open_pit_latrine_number)

e$open_pit_latrine_not_working <- as.numeric(e$open_pit_latrine_not_working)

e$bucket_system_number <- as.numeric(e$bucket_system_number)

e$bucket_system_not_working <- as.numeric(e$bucket_system_not_working)

e$other_toilets_number <- as.numeric(e$other_toilets_number)

e$other_toilets_not_working <- as.numeric(e$other_toilets_not_working)

e$times_building_cleaned_lastmth <- as.numeric(e$times_building_cleaned_lastmth)

e$times_trash_disposed_lastmth <- as.numeric(e$times_trash_disposed_lastmth)

e$num_tchrs_male_full_time <- as.numeric(e$num_tchrs_male_full_time)

e$num_tchrs_male_part_time <- as.numeric(e$num_tchrs_male_part_time)

e$num_tchrs_female_full_time <- as.numeric(e$num_tchrs_female_full_time)

e$num_tchrs_female_part_time <- as.numeric(e$num_tchrs_female_part_time)

cellst(e, 'num_tchrs_female_part_time',
       which(e$num_tchrs_female_part_time >= 999), NA_integer_)

e$tchrs_male_below_ssce <- as.numeric(e$tchrs_male_below_ssce)
cellst(e, 'tchrs_male_below_ssce',which(e$tchrs_male_below_ssce >900), NA_integer_) 

e$tchrs_female_below_ssce <- as.numeric(e$tchrs_female_below_ssce)
cellst(e, 'tchrs_female_below_ssce',which(e$tchrs_female_below_ssce >900), NA_integer_) 

e$tchrs_male_ssce_wasc <- as.numeric(e$tchrs_male_ssce_wasc)
cellst(e, 'tchrs_male_ssce_wasc',which(e$tchrs_male_ssce_wasc == 99), NA_integer_) #one data entry was 99  

e$tchrs_female_ssce_wasc <- as.numeric(e$tchrs_female_ssce_wasc)
cellst(e, 'tchrs_female_ssce_wasc',which(e$tchrs_female_ssce_wasc == 999), NA_integer_) #one data entry was 999

e$tchrs_male_grade2 <- as.numeric(e$tchrs_male_grade2)
cellst(e, 'tchrs_male_grade2',which(e$tchrs_male_grade2 == 9999), NA_integer_) #one data entry was 9999

e$tchrs_female_grade2 <- as.numeric(e$tchrs_female_grade2)
cellst(e, 'tchrs_female_grade2',which(e$tchrs_female_grade2 == 9999), NA_integer_) #one data entry was 9999

e$tchrs_male_ond <- as.numeric(e$tchrs_male_ond)
cellst(e, 'tchrs_male_ond',which(e$tchrs_male_ond == 999), NA_integer_) #one data entry was 999

e$tchrs_female_ond <- as.numeric(e$tchrs_female_ond)
cellst(e, 'tchrs_female_ond',which(e$tchrs_female_ond == 999), NA_integer_) #one data entry was 999

e$tchrs_male_nce <- as.numeric(e$tchrs_male_nce)
cellst(e, 'tchrs_male_nce',which(e$tchrs_male_nce == 999), NA_integer_)
cellst(e, 'tchrs_male_nce',which(e$tchrs_male_nce < 0), NA_integer_)

e$tchrs_female_nce <- as.numeric(e$tchrs_female_nce)
cellst(e, 'tchrs_female_nce',which(e$tchrs_female_nce == 9999), NA_integer_)
cellst(e, 'tchrs_female_nce',which(e$tchrs_female_nce < 0), NA_integer_)

e$tchrs_male_pgde <- as.numeric(e$tchrs_male_pgde)
cellst(e, 'tchrs_male_pgde',which(e$tchrs_male_pgde == 999), NA_integer_)

e$tchrs_female_pgde <- as.numeric(e$tchrs_female_pgde)
cellst(e, 'tchrs_female_pgde', which(e$tchrs_female_pgde == 9999), NA_integer_)

e$tchrs_male_b_ed <- as.numeric(e$tchrs_male_b_ed)
cellst(e, 'tchrs_male_b_ed', which(e$tchrs_male_b_ed == 998), NA_integer_)

e$tchrs_female_b_ed <- as.numeric(e$tchrs_female_b_ed)
cellst(e, 'tchrs_female_b_ed', which(e$tchrs_female_b_ed == 999), NA_integer_)

e$tchrs_male_other_w_nce <- as.numeric(e$tchrs_male_other_w_nce)
cellst(e, 'tchrs_male_other_w_nce', which(e$tchrs_male_other_w_nce == 99), NA_integer_) #99 considered as outlier

e$tchrs_female_other_w_nce <- as.numeric(e$tchrs_female_other_w_nce)
cellst(e, 'tchrs_female_other_w_nce', which(e$tchrs_female_other_w_nce == 999), NA_integer_)

e$tchrs_female_other_wo_nce <- as.numeric(e$tchrs_female_other_wo_nce)
cellst(e, 'tchrs_female_other_wo_nce', which(e$tchrs_female_other_wo_nce == 999), NA_integer_)

e$num_sr_staff_male <-as.numeric(e$num_sr_staff_male)

e$num_sr_staff_female <-as.numeric(e$num_sr_staff_female)

e$num_jr_staff_male <-as.numeric(e$num_jr_staff_male)

e$num_jr_staff_female <-as.numeric(e$num_jr_staff_female)

e$days_school_understaffed <- as.numeric(e$days_school_understaffed)

e$days_school_understaffed_closed <- as.numeric(e$days_school_understaffed_closed)

e$school_max_num_students <- as.numeric(e$school_max_num_students) #"I don't know" has eliminated
cellst(e, 'school_max_num_students', which(e$school_max_num_students == 9999), NA_integer_) #9999 has eliminated (there is 10800)

e$admit_more_num_students <- as.numeric(e$admit_more_num_students)
cellst(e, 'admit_more_num_students', which(e$admit_more_num_students == 9999), NA_integer_)

e$materials_fee <- as.numeric(e$materials_fee)

e$uniforms_fee <- as.numeric(e$uniforms_fee)
cellst(e, 'uniforms_fee', which(e$uniforms_fee > 900000), NA_integer_) #one extreme value

e$sports_fee <- as.numeric(e$sports_fee)

e$sports_fee_exempt_yn[e$sports_fee_exempt_yn != "yes" & e$sports_fee_exempt_yn != "no"] <- NA #unless yes or no, eliminate

e$num_students_exempt <- as.numeric(e$num_students_exempt)  # answer of "all" and "o" eliminated
cellst(e, 'num_students_exempt', which(e$num_students_exempt > 8000), NA_integer_) #20 of "9999" has eliminated

e$in_kind_fees_yn[e$in_kind_fees_yn != "yes" & e$in_kind_fees_yn != "no"] <- NA

e$booklist_per_class_yn[e$booklist_per_class_yn != "yes" & e$booklist_per_class_yn != "no"] <- NA

e$annual_budget_amt_received <- as.numeric(e$annual_budget_amt_received)

e$num_tchrs_paid_fed_gov <- as.numeric(e$num_tchrs_paid_fed_gov)

cellst(e, 'num_tchrs_paid_fed_gov',
       which(e$num_tchrs_paid_fed_gov >= 9999), NA_integer_)

e$num_tchrs_payrl_st_gov <- as.numeric(e$num_tchrs_payrl_st_gov)
cellst(e, 'num_tchrs_payrl_st_gov', which(e$num_tchrs_payrl_st_gov >= 9999), NA_integer_)

e$num_tchrs_othr_payrl_st_gov <- as.numeric(e$num_tchrs_othr_payrl_st_gov)
cellst(e, 'num_tchrs_othr_payrl_st_gov',
       which(e$num_tchrs_othr_payrl_st_gov >= 999), NA_integer_)

e$num_tchrs_paid_loc_gov <- as.numeric(e$num_tchrs_paid_loc_gov)
cellst(e, 'num_tchrs_paid_loc_gov',
       which(e$num_tchrs_paid_loc_gov >= 999), NA_integer_)

e$num_tchrs_paid_prvt_for_profit <- as.numeric(e$num_tchrs_paid_prvt_for_profit)
cellst(e, 'num_tchrs_paid_prvt_for_profit',
       which(e$num_tchrs_paid_prvt_for_profit >= 999), NA_integer_)

e$num_tchrs_paid_prvt_non_profit <- as.numeric(e$num_tchrs_paid_prvt_non_profit)

e$num_tchrs_paid_other_src <- as.numeric(e$num_tchrs_paid_other_src)
cellst(e, 'num_tchrs_paid_other_src', which(e$num_tchrs_paid_other_src == 999|e$num_tchrs_paid_other_src == 9999), NA_integer_) #999 and 9999

e$num_tchrs_no_salary <- as.numeric(e$num_tchrs_no_salary)
cellst(e, 'num_tchrs_no_salary', which(e$num_tchrs_no_salary >= 999), NA_integer_)

e$num_students_scholarship <- as.numeric(e$num_students_scholarship)
cellst(e, 'num_students_scholarship', which(e$num_students_scholarship > 9000), NA_integer_)

e$scholarship_amt <- as.numeric(e$scholarship_amt)

e$num_library_materials <- as.numeric(e$num_library_materials) # there were 999s, but reasonable data

e$times_new_materials_added <- as.numeric(e$times_new_materials_added)
cellst(e, 'times_new_materials_added', which(e$times_new_materials_added == 999), NA_integer_)


e$num_classrms_unused <- as.numeric(e$num_classrms_unused)
cellst(e, 'num_classrms_unused', which(e$num_classrms_unused >= 999), NA_integer_)

e$num_classrooms_multiple_use <- as.numeric(e$num_classrooms_multiple_use)
cellst(e, 'num_classrooms_multiple_use', which(e$num_classrooms_multiple_use >= 999), NA_integer_)

e$num_other_rooms <- as.numeric(e$num_other_rooms)
cellst(e,'num_other_rooms', which(e$num_other_rooms >= 999), NA_integer_)

e$num_sections_pry1 <- as.numeric(e$num_sections_pry1)
cellst(e,'num_sections_pry1', which(e$num_sections_pry1 >= 999), NA_integer_)

e$num_sections_pry2 <- as.numeric(e$num_sections_pry2)
cellst(e,'num_sections_pry2', which(e$num_sections_pry2 >= 999), NA_integer_)

e$num_sections_pry3 <- as.numeric(e$num_sections_pry3)
cellst(e,'num_sections_pry3', which(e$num_sections_pry3 >= 999), NA_integer_)

e$num_sections_pry4 <- as.numeric(e$num_sections_pry4)
cellst(e,'num_sections_pry4', which(e$num_sections_pry4 >= 999), NA_integer_)

e$num_sections_pry5 <- as.numeric(e$num_sections_pry5)
cellst(e,'num_sections_pry5', which(e$num_sections_pry5 >= 999), NA_integer_)

e$num_sections_pry6 <- as.numeric(e$num_sections_pry6)
cellst(e,'num_sections_pry6', which(e$num_sections_pry6 >= 999), NA_integer_)

e$num_sections_js1 <- as.numeric(e$num_sections_js1)
cellst(e,'num_sections_js1', which(e$num_sections_js1 >= 999), NA_integer_) # 8999 considered as typo of 9999

e$num_sections_js2 <- as.numeric(e$num_sections_js2)
cellst(e,'num_sections_js2', which(e$num_sections_js2 >= 899), NA_integer_) # 899 considered as typo of 999

e$num_sections_js3 <- as.numeric(e$num_sections_js3)
cellst(e,'num_sections_js3', which(e$num_sections_js3 >= 899), NA_integer_) # 899 considered as typo of 999

e$num_attached_benches <- as.numeric(e$num_attached_benches)
cellst(e,'num_attached_benches', which(e$num_attached_benches < 0), NA_integer_)
cellst(e,'num_attached_benches', which(e$num_attached_benches == 9999), NA_integer_) #considered 999 as reasonable number

e$num_attached_benches_unused <- as.numeric(e$num_attached_benches_unused)
cellst(e,'num_attached_benches_unused', which(e$num_attached_benches_unused >= 999), NA_integer_) #considered 99 as reasonable number

e$num_unattached_benches <- as.numeric(e$num_unattached_benches)

e$num_unattached_benches_unused <- as.numeric(e$num_unattached_benches_unused)

e$num_unattached_desks_unused <- as.numeric(e$num_unattached_desks_unused)

e$num_textbooks_english <- as.numeric(e$num_textbooks_english)

e$num_textbooks_math <- as.numeric(e$num_textbooks_math)

e$num_textbooks_social_sci <- as.numeric(e$num_textbooks_social_sci)

e$num_exercise_books <- as.numeric(e$num_exercise_books) # 2 of "dont know"

#extra

e$tchrs_male_other_wo_nce <- as.numeric(e$tchrs_male_other_wo_nce)
cellst(e,'tchrs_male_other_wo_nce', which(e$tchrs_male_other_wo_nce == 999), NA_integer_)

e$num_tchrs_attended_training <- as.numeric(e$num_tchrs_attended_training)
cellst(e,'num_tchrs_attended_training', which(e$num_tchrs_attended_training < 0), NA_integer_)

e$num_tchrs_attended_last_day <- as.numeric(e$num_tchrs_attended_last_day)
cellst(e,'num_tchrs_attended_last_day', which(e$num_tchrs_attended_last_day < 0), NA_integer_)

e$grid_months_broken <- as.numeric(e$grid_months_broken)
cellst(e, 'grid_months_broken', which(e$grid_months_broken >= 9999), NA_integer_)




####
e$new_stdnts_enroll_fee <- as.numeric(e$new_stdnts_enroll_fee)
e$days_no_potable_water <- as.numeric(e$days_no_potable_water)

e$cont_stdnts_enroll_fee <- as.numeric(e$cont_stdnts_enroll_fee)
cellst(e, 'cont_stdnts_enroll_fee', which(e$cont_stdnts_enroll_fee < 0), NA_integer_)

e$textbooks_fee <- as.numeric(e$textbooks_fee)
e$transport_fee <- as.numeric(e$transport_fee)
e$exams_fee <- as.numeric(e$exams_fee)
e$pta_fee <- as.numeric(e$pta_fee)

e$num_desks <- as.numeric(e$num_desks)
e$num_science_textbook_pry <- as.numeric(e$num_science_textbook_pry)


e$num_students_frthr_than_3km <- as.numeric(e$num_students_frthr_than_3km)

e$num_students_male <- as.numeric(e$num_students_male)

e$num_students_female <- as.numeric(e$num_students_female)

e$num_students_total <- as.numeric(e$num_students_total)

e$times_tchr_pay_delay_pastyr <- as.numeric(e$times_tchr_pay_delay_pastyr)

e$times_tchr_pay_miss_pastyr <- as.numeric(e$times_tchr_pay_miss_pastyr)















##knocking out 999 values
merged_education <- e

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
