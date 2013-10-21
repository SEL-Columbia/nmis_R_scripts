source('base_scripts/InstallFormhub.R')
source('source_scripts/NMIS_Functions.R')
source('cleaning_outliers/outlier_functions.R')

edu_999 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Education_774_999Cleaned.rds")

#Adding necessary indicators:

edu_999$chalkboard_each_classroom_yn[edu_999$src == '661'] <- (edu_999$num_classrms_total[edu_999$src == '661'] <= 
                                                                edu_999$num_classrm_w_chalkboard[edu_999$src == '661'])

edu_999$num_toilet_total[edu_999$src == "661"] <- apply(cbind(edu_999$num_toilet_boy[edu_999$src == "661"], 
                                                              edu_999$num_toilet_girl[edu_999$src == "661"], 
                                                              edu_999$num_toilet_both[edu_999$src == "661"]),
                                                        1, sum, na.rm=T)

edu_999$num_toilet_total[edu_999$src == "113"] <- apply(cbind(edu_999$vip_latrine_number[edu_999$src == "113"], 
                                                              edu_999$slab_pit_latrine_number[edu_999$src == "113"]), 
                                                        1, sum, na.rm=T)

edu_999$num_tchrs_male[edu_999$src == "113"] <- apply(cbind(edu_999$num_tchrs_male_full_time[edu_999$src == "113"], 
                                                            edu_999$num_tchrs_male_part_time[edu_999$src == "113"]), 
                                                      1, sum, na.rm=T)

edu_999$num_tchrs_female[edu_999$src == "113"] <- apply(cbind(edu_999$num_tchrs_female_full_time[edu_999$src == "113"], 
                                                              edu_999$num_tchrs_female_part_time[edu_999$src == "113"]), 
                                                        1, sum, na.rm=T)

edu_999$num_tchrs_w_nce[edu_999$src == "113"] <- apply(cbind(edu_999$tchrs_male_nce[edu_999$src == "113"], 
                                                             edu_999$tchrs_female_nce[edu_999$src == "113"], 
                                                             edu_999$tchrs_male_other_w_nce[edu_999$src == "113"],
                                                             edu_999$tchrs_female_other_w_nce[edu_999$src == "113"]),
                                                       1, sum, na.rm=T)

edu_999$num_classrms_total[edu_999$src == "113"] <- apply(cbind(edu_999$num_classrms_good_cond[edu_999$src == "113"], 
                                                                edu_999$num_classrms_need_min_repairs[edu_999$src == "113"], 
                                                                edu_999$num_classrms_need_maj_repairs[edu_999$src == "113"]),
                                                          1, sum, na.rm=T)

edu_999$num_benches[edu_999$src == "113"] <- apply(cbind(edu_999$num_attached_benches[edu_999$src == "113"], 
                                                         edu_999$num_unattached_benches[edu_999$src == "113"]),
                                                   1, sum, na.rm=T)

edu_999$potable_water <- ((edu_999$days_no_potable_water < 7) & (edu_999$water.none == FALSE))


######################################
#### Outlier Cleaning stars here: ####
######################################
edu_999$num_toilet_total <- ifelse(edu_999$toilet.none == F & edu_999$num_toilet_total == 0,
                                   NA, edu_999$num_toilet_total)

edu_999$num_students_total <- apply(cbind(edu_999$num_students_female, 
                                          edu_999$num_students_male), 
                                           1, sum, na.rm=T)

#dealing with total students problem
edu_999$test_f <- apply(cbind(edu_999$num_pry_female, 
                              edu_999$num_js_female,
                              edu_999$num_ss_female), 
                              1, sum, na.rm=T)

edu_999$test_m <- apply(cbind(edu_999$num_pry_male, 
                              edu_999$num_js_male,
                              edu_999$num_ss_male), 
                              1, sum, na.rm=T)

edu_999$total <- edu_999$test_f + edu_999$test_m

edu_999[which(edu_999$total > edu_999$num_students_total),"num_students_total"] <- 
                                        edu_999[which(edu_999$total > edu_999$num_students_total),"total"]

#logic checks
edu_999 <- outlierreplace(edu_999, 'num_tchrs_male', 
                    (edu_999$num_tchrs_male > edu_999$num_tchrs_total))

edu_999 <- outlierreplace(edu_999, 'num_tchrs_female', 
                    (edu_999$num_tchrs_female > edu_999$num_tchrs_total))

edu_999 <- outlierreplace(edu_999, 'num_tchrs_w_nce',
                    (edu_999$num_tchrs_w_nce > edu_999$num_tchrs_total))

edu_999 <- outlierreplace(edu_999, 'num_tchrs_attended_training',
                    (edu_999$num_tchrs_attended_training > edu_999$num_tchrs_total))

edu_999 <- outlierreplace(edu_999, 'num_tchrs_total',
                    (edu_999$num_tchrs_total > 20 & 
                         edu_999$num_students_total == 0))

edu_999 <- outlierreplace(edu_999, 'num_tchrs_total',
                    (edu_999$num_tchrs_total > e$num_tchrs_male + 
                         edu_999$num_tchrs_female))

edu_999 <- outlierreplace(edu_999, 'num_classrms_need_maj_repairs',
                    (edu_999$num_classrms_need_maj_repairs > edu_999$num_classrms_total))

edu_999 <- outlierreplace(edu_999, 'num_classrms_need_min_repairs',
                    (edu_999$num_classrms_need_min_repairs > edu_999$num_classrms_total))

edu_999 <- outlierreplace(edu_999, 'num_classrms_good_cond',
                    (edu_999$num_classrms_good_cond > edu_999$num_classrms_total))

edu_999 <- outlierreplace(edu_999, 'num_students_frthr_than_3km',
                    (edu_999$num_classrms_good_cond > edu_999$num_students_total))

edu_999 <- outlierreplace(edu_999, 'num_classrms_good_cond',
                    (edu_999$num_classrms_good_cond + edu_999$num_classrms_need_min_repairs + 
                         edu_999$num_classrms_need_maj_repairs > edu_999$num_classrms_total))

edu_999 <- outlierreplace(edu_999, 'num_classrms_need_min_repairs',
                    (edu_999$num_classrms_good_cond + edu_999$num_classrms_need_min_repairs + 
                         edu_999$num_classrms_need_maj_repairs > edu_999$num_classrms_total))

edu_999 <- outlierreplace(edu_999, 'num_classrms_need_maj_repairs',
                    (edu_999$num_classrms_good_cond + edu_999$num_classrms_need_min_repairs + 
                         edu_999$num_classrms_need_maj_repairs > edu_999$num_classrms_total))

edu_999 <- outlierreplace(edu_999, 'num_classrms_total',
                    (edu_999$num_classrms_good_cond + edu_999$num_classrms_need_min_repairs + 
                         edu_999$num_classrms_need_maj_repairs > edu_999$num_classrms_total))

edu_999 <- outlierreplace(edu_999, 'num_students_total',
                    (edu_999$num_tchrs_total > 20 & edu_999$num_students_total == 0))

edu_999 <- outlierreplace(edu_999, 'num_students_total', edu_999$num_students_total == 0)    

edu_999 <- outlierreplace(edu_999, 'num_students_total', (edu_999$num_students_total == 0))


##ratios
edu_999$ratio_students_to_toilet <- replace(edu_999$num_students_total, 
                                            is.na(edu_999$num_students_total), 0) /
                                    replace(edu_999$num_toilet_total, 
                                            is.na(edu_999$num_toilet_total), 0) 

edu_999$ratio_students_to_desks <-   replace(edu_999$num_students_total, 
                                       is.na(edu_999$num_students_total), 0) / 
                                    replace(edu_999$num_desks, 
                                        is.na(edu_999$num_desks), 0) 

edu_999$ratio_students_to_benches <-   replace(edu_999$num_students_total, 
                                         is.na(edu_999$num_students_total), 0) /
                                       replace(edu_999$num_benches, 
                                         is.na(edu_999$num_benches), 0) 

##textbooks
################## OCt 9th 

edu_999$ratio_pupil_math_pry_textbook <- replace(edu_999$num_math_textbook_pry, 
                                           is.na(edu_999$num_math_textbook_pry), 0) / 
                                        replace(edu_999$num_pry_total,
                                            is.na(edu_999$num_pry_total), 0)

edu_999$ratio_pupil_english_pry_textbook <- replace(edu_999$num_english_textbook_pry, 
                                                is.na(edu_999$num_english_textbook_pry), 0) / 
                                            replace(edu_999$num_pry_total,
                                                is.na(edu_999$num_pry_total), 0) 

edu_999$ratio_pupil_socscience_pry_textbook <- replace(edu_999$num_soc_science_textbook_pry, 
                                                    is.na(edu_999$num_soc_science_textbook_pry), 0) / 
                                                replace(edu_999$num_pry_total,
                                                    is.na(edu_999$num_pry_total), 0)

edu_999$ratio_pupil_science_pry_textbook <- replace(edu_999$num_science_textbook_pry, 
                                                is.na(edu_999$num_science_textbook_pry), 0) / 
                                            replace(edu_999$num_pry_total,
                                                is.na(edu_999$num_pry_total), 0)

edu_999$ratio_pupil_math_js_textbook <- replace(edu_999$num_math_textbook_js, 
                                            is.na(edu_999$num_math_textbook_js), 0) / 
                                        replace(edu_999$num_js_total,
                                            is.na(edu_999$num_js_total), 0)

edu_999$ratio_pupil_english_js_textbook <- replace(edu_999$num_english_textbook_js, 
                                                is.na(edu_999$num_english_textbook_js), 0) / 
                                            replace(edu_999$num_js_total,
                                                is.na(edu_999$num_js_total), 0)

edu_999$ratio_pupil_socscience_js_textbook <- replace(edu_999$num_soc_science_textbook_js, 
                                                  is.na(edu_999$num_soc_science_textbook_js), 0) / 
                                              replace(edu_999$num_js_total,
                                                  is.na(edu_999$num_js_total), 0)

edu_999$ratio_pupil_science_js_textbook <- replace(edu_999$num_science_textbook_js, 
                                                is.na(edu_999$num_science_textbook_js), 0) / 
                                            replace(edu_999$num_js_total,
                                                is.na(edu_999$num_js_total), 0)  

edu_999 <- outlierreplace(edu_999, 'num_math_textbook_pry', 
                    between(edu_999$ratio_pupil_math_pry_textbook, 10, Inf))

edu_999 <- outlierreplace(edu_999, 'num_english_textbook_pry', 
                    between(edu_999$ratio_pupil_english_pry_textbook, 10, Inf))

edu_999 <- outlierreplace(edu_999, 'num_soc_science_textbook_pry', 
                    between(edu_999$ratio_pupil_socscience_pry_textbook, 10, Inf))

edu_999 <- outlierreplace(edu_999, 'num_science_textbook_pry', 
                    between(edu_999$ratio_pupil_science_pry_textbook, 10, Inf))

edu_999 <- outlierreplace(edu_999, 'num_math_textbook_js', 
                    between(edu_999$ratio_pupil_math_js_textbook, 10, Inf))

edu_999 <- outlierreplace(edu_999, 'num_english_textbook_js', 
                    between(edu_999$ratio_pupil_english_js_textbook, 10, Inf))

edu_999 <- outlierreplace(edu_999, 'num_soc_science_textbook_js', 
                    between(edu_999$ratio_pupil_socscience_js_textbook, 10, Inf))

edu_999 <- outlierreplace(edu_999, 'num_science_textbook_js', 
                    between(edu_999$ratio_pupil_science_js_textbook, 10, Inf))

#new data points

#edu_999$pupil_class_ratio <- edu_999$num_students_total/edu_999$num_classrms_total
#edu_999 <- outlierreplace(edu_999, 'num_students_total', 
#                  (edu_999$pupil_class_ratio < 5 | edu_999$pupil_class_ratio > 150))
#edu_999 <- outlierreplace(edu_999, 'num_classrms_total', 
#                   (edu_999$pupil_class_ratio < 5 | edu_999$pupil_class_ratio > 150))

edu_999 <- outlierreplace(edu_999, 'num_students_female',
                                (edu_999$num_students_female > 3000))

edu_999 <- outlierreplace(edu_999, 'num_students_male',
                                (edu_999$num_students_male > 2500 &
                                 edu_999$num_classrms_total < 25))

edu_999 <- outlierreplace(edu_999, 'num_students_total',
                                (edu_999$num_students_total > 2000 & 
                                 edu_999$num_classrms_total < 25 &
                                 edu_999$num_tchrs_total < 10))

edu_999 <- outlierreplace(edu_999, 'num_pry_female',
                                (edu_999$num_pry_female > 2000 & 
                                 edu_999$num_classrms_total < 25 &
                                 edu_999$num_tchrs_total < 10))

edu_999 <- outlierreplace(edu_999, 'num_pry_male',
                                (edu_999$num_pry_male > 2000 & 
                                edu_999$num_classrms_total < 25 &
                                edu_999$num_tchrs_total < 10))

edu_999 <- outlierreplace(edu_999, 'num_pry_total',
                                (edu_999$num_pry_total > 2500 & 
                                 edu_999$num_classrms_total < 25 &
                                 edu_999$num_tchrs_total < 10))

edu_999 <- outlierreplace(edu_999, 'num_js_female',
                                (edu_999$num_js_female > 1250 & 
                                 edu_999$num_classrms_total < 25 &
                                 edu_999$num_tchrs_total < 10))

edu_999 <- outlierreplace(edu_999, 'num_js_male',
                                (edu_999$num_js_male > 1250 & 
                                 edu_999$num_classrms_total < 25 &
                                 edu_999$num_tchrs_total < 10))

edu_999 <- outlierreplace(edu_999, 'num_js_total',
                                (edu_999$num_js_total > 2500 & 
                                 edu_999$num_classrms_total < 25 &
                                 edu_999$num_tchrs_total < 10))

edu_999 <- outlierreplace(edu_999, 'num_ss_female',
                                (edu_999$num_ss_female > 1250 &
                                 edu_999$num_classrms_total < 25 &
                                 edu_999$num_tchrs_total < 10))

edu_999 <- outlierreplace(edu_999, 'num_ss_male',
                                (edu_999$num_ss_male > 1250 & 
                                 edu_999$num_classrms_total < 25 &
                                 edu_999$num_tchrs_total < 10))

edu_999 <- outlierreplace(edu_999, 'num_ss_total',
                                (edu_999$num_ss_total > 2500 & 
                                 edu_999$num_classrms_total < 25 &
                                 edu_999$num_tchrs_total < 10))

edu_999 <- outlierreplace(edu_999, 'num_ss_total',
                                (edu_999$km_to_catchment_area > 30))

edu_999 <- outlierreplace(edu_999, 'km_to_catchment_area',
                    (edu_999$km_to_catchment_area > 55))                    
edu_999 <- outlierreplace(edu_999, 'km_to_secondary_school',
                    (edu_999$km_to_secondary_school > 25))
edu_999 <- outlierreplace(edu_999, 'num_students_frthr_than_3km',
                    (edu_999$num_students_frthr_than_3km > 1250))            
edu_999 <- outlierreplace(edu_999, 'num_toilet_girl',
                    (edu_999$num_toilet_girl > 750))            
edu_999 <- outlierreplace(edu_999, 'num_toilet_both',
                    (edu_999$num_toilet_both > 1000))            
edu_999 <- outlierreplace(edu_999, 'num_toilet_total',
                    (edu_999$num_toilet_total > 1000))                        
edu_999 <- outlierreplace(edu_999, 'num_tchrs_male',
                    (edu_999$num_tchrs_male > 100))
edu_999 <- outlierreplace(edu_999, 'num_tchrs_female',
                    (edu_999$num_tchrs_female > 100))
edu_999 <- outlierreplace(edu_999, 'num_tchrs_w_nce',
                    (edu_999$num_tchrs_w_nce > 100))
edu_999 <- outlierreplace(edu_999, 'num_tchrs_w_nce_plus',
                    (edu_999$num_tchrs_w_nce_plus > 100))            
edu_999 <- outlierreplace(edu_999, 'num_sr_staff_total',
                    (edu_999$num_sr_staff_total > 75))
edu_999 <- outlierreplace(edu_999, 'num_jr_staff_total',
                    (edu_999$num_jr_staff_total > 50))
edu_999 <- outlierreplace(edu_999, 'num_tchrs_attended_training',
                    (edu_999$num_tchrs_attended_training > 100))
edu_999 <- outlierreplace(edu_999, 'num_classrms_total',
                    (edu_999$num_tchrs_attended_training > 99))
edu_999 <- outlierreplace(edu_999, 'num_classrms_total',
                    (edu_999$num_classrms_total == 0))
edu_999 <- outlierreplace(edu_999, 'num_classrms_need_min_repairs',
                    (edu_999$num_classrms_need_min_repairs > 50))
edu_999 <- outlierreplace(edu_999, 'num_classrms_need_maj_repairs',
                    (edu_999$num_classrms_need_maj_repairs > 50))
#absolute example
edu_999 <- outlierreplace(edu_999, 'num_classrms_good_cond',
                    (edu_999$num_classrms_good_cond > 260))                    
#ratio/ combining other data points example
#edu_999 <- outlierreplace(edu_999, 'num_desks',
#     between(edu_999$ratio_students_to_desks, 750, Inf))
edu_999 <- outlierreplace(edu_999, 'num_desks',
                    (edu_999$num_desks > 2500))
edu_999 <- outlierreplace(edu_999, 'num_benches',
                    (edu_999$num_benches > 2500))
edu_999 <- outlierreplace(edu_999, 'num_classrm_w_chalkboard',
                    (edu_999$num_classrm_w_chalkboard > edu_999$num_classrms_total))      
#another absolute
edu_999 <- outlierreplace(edu_999, 'num_math_textbook_pry',
                    (edu_999$num_math_textbook_pry > 3000))    

edu_999 <- outlierreplace(edu_999, 'num_english_textbook_pry',
                    (edu_999$num_english_textbook_pry > 3000))      

edu_999 <- outlierreplace(edu_999, 'num_soc_science_textbook_pry',
                    (edu_999$num_soc_science_textbook_pry > 3000))      

edu_999 <- outlierreplace(edu_999, 'num_science_textbook_pry',
                    (edu_999$num_science_textbook_pry > 3000))      

edu_999 <- outlierreplace(edu_999, 'num_math_textbook_js',
                    (edu_999$num_math_textbook_js > 3000))      

edu_999 <- outlierreplace(edu_999, 'num_english_textbook_js',
                    (edu_999$num_english_textbook_js > 3000))      

edu_999 <- outlierreplace(edu_999, 'num_soc_science_textbook_js',
                    (edu_999$num_soc_science_textbook_js > 3000))      

edu_999 <- outlierreplace(edu_999, 'num_science_textbook_js',
                    (edu_999$num_science_textbook_js > 3000))      

edu_999 <- outlierreplace(edu_999, 'num_exercise_books_per_student_pry',
                    which((edu_999$num_exercise_books_per_student_pry > 19800) | 
                              (edu_999$num_students_total * 8 < 
                                   edu_999$num_exercise_books_per_student_pry)))

edu_999 <- outlierreplace(edu_999, 'num_exercise_books_per_student_jss',
                    which((edu_999$num_exercise_books_per_student_jss > 34000) | 
                              (edu_999$num_students_total * 8 < 
                                   edu_999$num_exercise_books_per_student_jss)))  

edu_999 <- outlierreplace(edu_999, 'ratio_students_to_toilet',
                    between(edu_999$ratio_students_to_toilet, 1000, Inf))      

edu_999 <- outlierreplace(edu_999, 'ratio_students_to_desks',
                    between(edu_999$ratio_students_to_desks, 1000, Inf)) 

edu_999 <- outlierreplace(edu_999, 'ratio_students_to_benches',
                    between(edu_999$ratio_students_to_benches, 1000, Inf)) 

edu_999$ratio_students_to_toilet <- ifelse(edu_999$ratio_students_to_toilet == 0, NA, 
                                           edu_999$ratio_students_to_toilet)

edu_999$ratio_students_to_desks <-  ifelse(edu_999$ratio_students_to_desks == 0, NA, 
                                           edu_999$ratio_students_to_desks)

edu_999$ratio_students_to_benches <- ifelse(edu_999$ratio_students_to_benches == 0, NA, 
                                            edu_999$ratio_students_to_benches)

edu_999$num_students_total <- ifelse(edu_999$num_students_total == 0, NA, 
                                     edu_999$num_students_total)

edu_999$num_tchrs_total <- apply(cbind(edu_999$num_tchrs_male, 
                                        edu_999$num_tchrs_female), 
                                        1, sum, na.rm=T)


#### TODO: investigate why these are in the data at all; constraints should have knocked these out #####

# replace all classroom indicators if they are below total classrooms
edu_999 <- outlierreplace(edu_999, 'num_classrms_need_min_repairs', 
                          edu_999$num_classrms_need_min_repairs > edu_999$num_classrms_total) 
edu_999 <- outlierreplace(edu_999, 'num_classrms_need_maj_repairs',
                          edu_999$num_classrms_need_maj_repairs > edu_999$num_classrms_total) 

edu_999 <- outlierreplace(edu_999, 'num_students_total',
                          edu_999$num_students_total > 2355)
##writing out.rds
saveRDS(edu_999, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Education_774_outliercleaned.rds")
rm(edu_999)

