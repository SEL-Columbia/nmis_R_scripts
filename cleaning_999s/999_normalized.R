source('base_scripts/InstallFormhub.R')
source('./cleaning_999s/999_functions.R')
source("source_scripts/Normailize_Functions.R")

merged_education <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Normalized/Edu774.rds")

#Here starts 999 cleaning
cellst(merged_education, 'tchrs_male_ssce_wasc',
       which(merged_education$tchrs_male_ssce_wasc == 99), NA_integer_) #one data entry was 99  

cellst(merged_education, 'tchrs_male_nce',
       which(merged_education$tchrs_male_nce < 0), NA_integer_)

cellst(merged_education, 'tchrs_female_nce',
       which(merged_education$tchrs_female_nce < 0), NA_integer_)

cellst(merged_education, 'tchrs_male_b_ed',
       which(merged_education$tchrs_male_b_ed == 998), NA_integer_)

cellst(merged_education, 'tchrs_male_other_w_nce',
       which(merged_education$tchrs_male_other_w_nce == 99), NA_integer_) #99 considered as outlier

cellst(merged_education, 'uniforms_fee',
       which(merged_education$uniforms_fee > 6000), NA_integer_) #one extreme value

cellst(merged_education, 'num_students_exempt',
       which(merged_education$num_students_exempt > 8000), NA_integer_) #20 of "9999" has eliminated

cellst(merged_education, 'num_tchrs_paid_fed_gov',
       which(merged_education$num_tchrs_paid_fed_gov >= 1000), NA_integer_)

cellst(merged_education, 'num_classrms_unused', 
       which(merged_education$num_classrms_unused > 999), NA_integer_)

cellst(merged_education,'num_sections_pry3',
       which(merged_education$num_sections_pry3 >= 999), NA_integer_)

cellst(merged_education,'num_sections_pry4', 
       which(merged_education$num_sections_pry4 >= 999), NA_integer_)

cellst(merged_education,'num_sections_js1', 
       which(merged_education$num_sections_js1 >= 899), NA_integer_) # 8999 considered as typo of 9999

cellst(merged_education,'num_sections_js2', 
       which(merged_education$num_sections_js2 >= 899), NA_integer_) # 899 considered as typo of 999

cellst(merged_education,'num_sections_js3', 
       which(merged_education$num_sections_js3 >= 899), NA_integer_) # 899 considered as typo of 999

cellst(merged_education,'num_attached_benches', 
       which(merged_education$num_attached_benches < 0), NA_integer_)
#extra

cellst(merged_education,'num_tchrs_attended_training', 
       which(merged_education$num_tchrs_attended_training < 0), NA_integer_)

cellst(merged_education,'num_tchrs_attended_last_day', 
       which(merged_education$num_tchrs_attended_last_day < 0), NA_integer_)

cellst(merged_education, 'cont_stdnts_enroll_fee', 
       which(merged_education$cont_stdnts_enroll_fee < 0), NA_integer_)


#### all 3 sources

cellst(merged_education,
       c('num_students_male','num_students_female',
         'num_students_total'),
       which(merged_education$num_students_male >= 9991), NA_integer_) 

cellst(merged_education,
       c('num_students_male','num_students_female',
         'num_students_total'),
       which(merged_education$num_students_female >= 9064), NA_integer_)

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
       which(merged_education$num_tchrs_male >= 9993), NA_integer_)

cellst(merged_education, 'num_tchrs_female',
       which(merged_education$num_tchrs_female >= 9991), NA_integer_)

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
       which(merged_education$num_students_frthr_than_3km >= 9000), 
       NA_integer_) # 999 considered as outlier

cellst(merged_education, 'num_students_total', 
       which(merged_education$num_students_total < 0), NA_integer_)

cellst(merged_education, 'days_no_potable_water', 
       which(merged_education$days_no_potable_water < 0), NA_integer_)

cellst(merged_education, 'num_toilet_boy', 
       which(merged_education$num_toilet_boy < 0 | merged_education$num_toilet_boy >= 999), NA_integer_)

cellst(merged_education, 'num_toilet_girl', 
       which(merged_education$num_toilet_girl < 0 | merged_education$num_toilet_boy >= 999), NA_integer_)

#individual cells
cellst(merged_education, 'num_tchrs_w_nce',
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
saveRDS(merged_education, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Education_774_999Cleaned.rds")






