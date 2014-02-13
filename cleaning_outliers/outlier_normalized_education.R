#####################################################################################################################
##Normalized Education Outlier Cleaning

source('base_scripts/InstallFormhub.R')
source('source_scripts/NMIS_Functions.R')
source('source_scripts/outlier_functions.R')

edu_normalized <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Normalized/Education_774_normalized.rds")

######################################
#### Outlier Cleaning 

edu_normalized$num_toilets_total <- ifelse(edu_normalized$toilet.none == F & edu_normalized$num_toilets_total == 0,
                                   NA, edu_normalized$num_toilets_total)

edu_normalized$num_students_total <- rowSums(cbind(edu_normalized$num_students_female, 
                                                    edu_normalized$num_students_male), 
                                                na.rm=T)

#dealing with total students problem
edu_normalized$test_f <- rowSums(cbind(edu_normalized$num_pry_female, 
                                        edu_normalized$num_js_female,
                                        edu_normalized$num_ss_female), 
                                    na.rm=T)

edu_normalized$test_m <- rowSums(cbind(edu_normalized$num_pry_male, 
                              edu_normalized$num_js_male,
                              edu_normalized$num_ss_male), 
                              na.rm=T)

edu_normalized$total <- edu_normalized$test_f + edu_normalized$test_m

edu_normalized[which(edu_normalized$total > edu_normalized$num_students_total),"num_students_total"] <- 
                                        edu_normalized[which(edu_normalized$total > edu_normalized$num_students_total),"total"]

#logic checks
edu_normalized <- outlierreplace(edu_normalized, 'num_tchrs_male', 
                    (edu_normalized$num_tchrs_male > edu_normalized$num_tchrs_total))

edu_normalized <- outlierreplace(edu_normalized, 'num_tchrs_female', 
                    (edu_normalized$num_tchrs_female > edu_normalized$num_tchrs_total))

edu_normalized <- outlierreplace(edu_normalized, 'num_tchrs_w_nce',
                    (edu_normalized$num_tchrs_w_nce > edu_normalized$num_tchrs_total))

edu_normalized <- outlierreplace(edu_normalized, 'num_tchrs_attended_training',
                    (edu_normalized$num_tchrs_attended_training > edu_normalized$num_tchrs_total))

edu_normalized <- outlierreplace(edu_normalized, 'num_tchrs_total',
                    (edu_normalized$num_tchrs_total > 20 & 
                         edu_normalized$num_students_total == 0))

edu_normalized <- outlierreplace(edu_normalized, 'num_tchrs_total',
                    (edu_normalized$num_tchrs_total > edu_normalized$num_tchrs_male + 
                         edu_normalized$num_tchrs_female))

edu_normalized <- outlierreplace(edu_normalized, 'num_classrms_need_maj_repairs',
                    (edu_normalized$num_classrms_need_maj_repairs > edu_normalized$num_classrms_total))

edu_normalized <- outlierreplace(edu_normalized, 'num_classrms_need_min_repairs',
                    (edu_normalized$num_classrms_need_min_repairs > edu_normalized$num_classrms_total))

edu_normalized <- outlierreplace(edu_normalized, 'num_classrms_good_cond',
                    (edu_normalized$num_classrms_good_cond > edu_normalized$num_classrms_total))

edu_normalized <- outlierreplace(edu_normalized, 'num_students_frthr_than_3km',
                    (edu_normalized$num_classrms_good_cond > edu_normalized$num_students_total))

edu_normalized <- outlierreplace(edu_normalized, 'num_classrms_good_cond',
                    (edu_normalized$num_classrms_good_cond + edu_normalized$num_classrms_need_min_repairs + 
                         edu_normalized$num_classrms_need_maj_repairs > edu_normalized$num_classrms_total))

edu_normalized <- outlierreplace(edu_normalized, 'num_students_total', edu_normalized$num_students_total == 0)    

##ratios
edu_normalized$ratio_students_to_toilet <- replace(edu_normalized$num_students_total, 
                                            is.na(edu_normalized$num_students_total), 0) /
                                    replace(edu_normalized$num_toilets_total, 
                                            is.na(edu_normalized$num_toilets_total), 0) 

edu_normalized$ratio_students_to_desks <-   replace(edu_normalized$num_students_total, 
                                       is.na(edu_normalized$num_students_total), 0) / 
                                    replace(edu_normalized$num_desks, 
                                        is.na(edu_normalized$num_desks), 0) 

edu_normalized$ratio_students_to_benches <-   replace(edu_normalized$num_students_total, 
                                         is.na(edu_normalized$num_students_total), 0) /
                                       replace(edu_normalized$num_benches, 
                                         is.na(edu_normalized$num_benches), 0) 

##textbooks
################## OCt 9th 

edu_normalized$ratio_pupil_math_pry_textbook <- replace(edu_normalized$num_math_textbook_pry, 
                                           is.na(edu_normalized$num_math_textbook_pry), 0) / 
                                        replace(edu_normalized$num_pry_total,
                                            is.na(edu_normalized$num_pry_total), 0)

edu_normalized$ratio_pupil_english_pry_textbook <- replace(edu_normalized$num_english_textbook_pry, 
                                                is.na(edu_normalized$num_english_textbook_pry), 0) / 
                                            replace(edu_normalized$num_pry_total,
                                                is.na(edu_normalized$num_pry_total), 0) 

edu_normalized$ratio_pupil_socscience_pry_textbook <- replace(edu_normalized$num_soc_science_textbook_pry, 
                                                    is.na(edu_normalized$num_soc_science_textbook_pry), 0) / 
                                                replace(edu_normalized$num_pry_total,
                                                    is.na(edu_normalized$num_pry_total), 0)

edu_normalized$ratio_pupil_science_pry_textbook <- replace(edu_normalized$num_science_textbook_pry, 
                                                is.na(edu_normalized$num_science_textbook_pry), 0) / 
                                            replace(edu_normalized$num_pry_total,
                                                is.na(edu_normalized$num_pry_total), 0)

edu_normalized$ratio_pupil_math_js_textbook <- replace(edu_normalized$num_math_textbook_js, 
                                            is.na(edu_normalized$num_math_textbook_js), 0) / 
                                        replace(edu_normalized$num_js_total,
                                            is.na(edu_normalized$num_js_total), 0)

edu_normalized$ratio_pupil_english_js_textbook <- replace(edu_normalized$num_english_textbook_js, 
                                                is.na(edu_normalized$num_english_textbook_js), 0) / 
                                            replace(edu_normalized$num_js_total,
                                                is.na(edu_normalized$num_js_total), 0)

edu_normalized$ratio_pupil_socscience_js_textbook <- replace(edu_normalized$num_soc_science_textbook_js, 
                                                  is.na(edu_normalized$num_soc_science_textbook_js), 0) / 
                                              replace(edu_normalized$num_js_total,
                                                  is.na(edu_normalized$num_js_total), 0)

edu_normalized$ratio_pupil_science_js_textbook <- replace(edu_normalized$num_science_textbook_js, 
                                                is.na(edu_normalized$num_science_textbook_js), 0) / 
                                            replace(edu_normalized$num_js_total,
                                                is.na(edu_normalized$num_js_total), 0)  

edu_normalized <- outlierreplace(edu_normalized, 'num_math_textbook_pry', 
                    between(edu_normalized$ratio_pupil_math_pry_textbook, 10, Inf))

edu_normalized <- outlierreplace(edu_normalized, 'num_english_textbook_pry', 
                    between(edu_normalized$ratio_pupil_english_pry_textbook, 10, Inf))

edu_normalized <- outlierreplace(edu_normalized, 'num_soc_science_textbook_pry', 
                    between(edu_normalized$ratio_pupil_socscience_pry_textbook, 10, Inf))

edu_normalized <- outlierreplace(edu_normalized, 'num_science_textbook_pry', 
                    between(edu_normalized$ratio_pupil_science_pry_textbook, 10, Inf))

edu_normalized <- outlierreplace(edu_normalized, 'num_math_textbook_js', 
                    between(edu_normalized$ratio_pupil_math_js_textbook, 10, Inf))

edu_normalized <- outlierreplace(edu_normalized, 'num_english_textbook_js', 
                    between(edu_normalized$ratio_pupil_english_js_textbook, 10, Inf))

edu_normalized <- outlierreplace(edu_normalized, 'num_soc_science_textbook_js', 
                    between(edu_normalized$ratio_pupil_socscience_js_textbook, 10, Inf))

edu_normalized <- outlierreplace(edu_normalized, 'num_science_textbook_js', 
                    between(edu_normalized$ratio_pupil_science_js_textbook, 10, Inf))

#new data points

# edu_normalized$pupil_class_ratio <- edu_normalized$num_students_total/edu_normalized$num_classrms_total
# edu_normalized <- outlierreplace(edu_normalized, 'num_students_total', 
#                  (edu_normalized$pupil_class_ratio < 5 | edu_normalized$pupil_class_ratio > 150))
# edu_normalized <- outlierreplace(edu_normalized, 'num_classrms_total', 
#                   (edu_normalized$pupil_class_ratio < 5 | edu_normalized$pupil_class_ratio > 150))

edu_normalized <- outlierreplace(edu_normalized, 'num_students_female',
                                (edu_normalized$num_students_female > 3000))

edu_normalized <- outlierreplace(edu_normalized, 'num_students_male',
                                (edu_normalized$num_students_male > 2500 &
                                 edu_normalized$num_classrms_total < 25))

edu_normalized <- outlierreplace(edu_normalized, 'num_students_total',
                                (edu_normalized$num_students_total > 2000 & 
                                 edu_normalized$num_classrms_total < 25 &
                                 edu_normalized$num_tchrs_total < 10))

edu_normalized <- outlierreplace(edu_normalized, 'num_pry_male',
                                (edu_normalized$num_pry_male > 2000 & 
                                edu_normalized$num_classrms_total < 25 &
                                edu_normalized$num_tchrs_total < 10))

edu_normalized <- outlierreplace(edu_normalized, 'num_pry_total',
                                (edu_normalized$num_pry_total > 2500 & 
                                 edu_normalized$num_classrms_total < 25 &
                                 edu_normalized$num_tchrs_total < 10))

edu_normalized <- outlierreplace(edu_normalized, 'num_js_female',
                                (edu_normalized$num_js_female > 1250 & 
                                 edu_normalized$num_classrms_total < 25 &
                                 edu_normalized$num_tchrs_total < 10))

edu_normalized <- outlierreplace(edu_normalized, 'num_js_male',
                                (edu_normalized$num_js_male > 1250 & 
                                 edu_normalized$num_classrms_total < 25 &
                                 edu_normalized$num_tchrs_total < 10))

edu_normalized <- outlierreplace(edu_normalized, 'num_js_total',
                                (edu_normalized$num_js_total > 2500 & 
                                 edu_normalized$num_classrms_total < 25 &
                                 edu_normalized$num_tchrs_total < 10))

edu_normalized <- outlierreplace(edu_normalized, 'num_ss_male',
                                (edu_normalized$num_ss_male > 1250 & 
                                 edu_normalized$num_classrms_total < 25 &
                                 edu_normalized$num_tchrs_total < 10))

edu_normalized <- outlierreplace(edu_normalized, 'num_ss_total',
                                (edu_normalized$km_to_catchment_area > 30))

edu_normalized <- outlierreplace(edu_normalized, 'km_to_catchment_area',
                    (edu_normalized$km_to_catchment_area > 55))                    

edu_normalized <- outlierreplace(edu_normalized, 'km_to_secondary_school',
                    (edu_normalized$km_to_secondary_school > 25))

edu_normalized <- outlierreplace(edu_normalized, 'num_students_frthr_than_3km',
                    (edu_normalized$num_students_frthr_than_3km > 1250))            

edu_normalized <- outlierreplace(edu_normalized, 'num_tchrs_male',
                    (edu_normalized$num_tchrs_male > 100))

edu_normalized <- outlierreplace(edu_normalized, 'num_tchrs_female',
                    (edu_normalized$num_tchrs_female > 100))

edu_normalized <- outlierreplace(edu_normalized, 'num_tchrs_w_nce',
                    (edu_normalized$num_tchrs_w_nce > 100))

edu_normalized <- outlierreplace(edu_normalized, 'num_tchrs_w_nce_plus',
                    (edu_normalized$num_tchrs_w_nce_plus > 100))            

edu_normalized <- outlierreplace(edu_normalized, 'num_sr_staff_total',
                    (edu_normalized$num_sr_staff_total > 75))

edu_normalized <- outlierreplace(edu_normalized, 'num_jr_staff_total',
                    (edu_normalized$num_jr_staff_total > 50))

edu_normalized <- outlierreplace(edu_normalized, 'num_tchrs_attended_training',
                    (edu_normalized$num_tchrs_attended_training > 100))

edu_normalized <- outlierreplace(edu_normalized, 'num_classrms_total',
                    (edu_normalized$num_tchrs_attended_training > 99))

edu_normalized <- outlierreplace(edu_normalized, 'num_classrms_total',
                    (edu_normalized$num_classrms_total == 0))

edu_normalized <- outlierreplace(edu_normalized, 'num_classrms_need_min_repairs',
                    (edu_normalized$num_classrms_need_min_repairs > 50))

edu_normalized <- outlierreplace(edu_normalized, 'num_classrms_need_maj_repairs',
                    (edu_normalized$num_classrms_need_maj_repairs > 50))
#absolute example
edu_normalized <- outlierreplace(edu_normalized, 'num_classrms_good_cond',
                    (edu_normalized$num_classrms_good_cond > 260))                    

#ratio/ combining other data points example
edu_normalized <- outlierreplace(edu_normalized, 'num_desks',
     between(edu_normalized$ratio_students_to_desks, 750, Inf))

edu_normalized <- outlierreplace(edu_normalized, 'num_desks',
                    (edu_normalized$num_desks > 2500))

edu_normalized <- outlierreplace(edu_normalized, 'num_benches',
                    (edu_normalized$num_benches > 2500))

#another absolute
edu_normalized <- outlierreplace(edu_normalized, 'num_math_textbook_pry',
                    (edu_normalized$num_math_textbook_pry > 3000))    

edu_normalized <- outlierreplace(edu_normalized, 'num_english_textbook_pry',
                    (edu_normalized$num_english_textbook_pry > 3000))      

edu_normalized <- outlierreplace(edu_normalized, 'num_soc_science_textbook_pry',
                    (edu_normalized$num_soc_science_textbook_pry > 3000))      

edu_normalized <- outlierreplace(edu_normalized, 'num_science_textbook_pry',
                    (edu_normalized$num_science_textbook_pry > 3000))      

edu_normalized <- outlierreplace(edu_normalized, 'num_math_textbook_js',
                    (edu_normalized$num_math_textbook_js > 3000))      

edu_normalized <- outlierreplace(edu_normalized, 'num_english_textbook_js',
                    (edu_normalized$num_english_textbook_js > 3000))      

edu_normalized <- outlierreplace(edu_normalized, 'num_soc_science_textbook_js',
                    (edu_normalized$num_soc_science_textbook_js > 3000))      

edu_normalized <- outlierreplace(edu_normalized, 'num_science_textbook_js',
                    (edu_normalized$num_science_textbook_js > 3000))      

edu_normalized <- outlierreplace(edu_normalized, 'num_exercise_books_per_student_pry',
                    which((edu_normalized$num_exercise_books_per_student_pry > 19800) | 
                              (edu_normalized$num_students_total * 8 < 
                                   edu_normalized$num_exercise_books_per_student_pry)))

edu_normalized <- outlierreplace(edu_normalized, 'num_exercise_books_per_student_jss',
                    which((edu_normalized$num_exercise_books_per_student_jss > 34000) | 
                              (edu_normalized$num_students_total * 8 < 
                                   edu_normalized$num_exercise_books_per_student_jss)))  

edu_normalized <- outlierreplace(edu_normalized, 'ratio_students_to_toilet',
                    between(edu_normalized$ratio_students_to_toilet, 1000, Inf))      

edu_normalized <- outlierreplace(edu_normalized, 'ratio_students_to_desks',
                    between(edu_normalized$ratio_students_to_desks, 1000, Inf)) 

edu_normalized <- outlierreplace(edu_normalized, 'ratio_students_to_benches',
                    between(edu_normalized$ratio_students_to_benches, 1000, Inf)) 

edu_normalized$ratio_students_to_toilet <- ifelse(edu_normalized$ratio_students_to_toilet == 0, NA, 
                                           edu_normalized$ratio_students_to_toilet)

edu_normalized$ratio_students_to_desks <-  ifelse(edu_normalized$ratio_students_to_desks == 0, NA, 
                                           edu_normalized$ratio_students_to_desks)

edu_normalized$ratio_students_to_benches <- ifelse(edu_normalized$ratio_students_to_benches == 0, NA, 
                                            edu_normalized$ratio_students_to_benches)

######## May need to delete next 3 lines after discussion
edu_normalized$num_students_total <- rowSums(cbind(edu_normalized$num_students_male, 
                                                    edu_normalized$num_students_female), 
                                            na.rm=T)

edu_normalized$num_students_total <- ifelse(edu_normalized$num_students_total == 0, NA, 
                                     edu_normalized$num_students_total)

edu_normalized$num_tchrs_total <- rowSums(cbind(edu_normalized$num_tchrs_male, 
                                                edu_normalized$num_tchrs_female), 
                                            na.rm=T)


#### TODO: investigate why these are in the data at all; constraints should have knocked these out #####

# replace all classroom indicators if they are below total classrooms
edu_normalized <- outlierreplace(edu_normalized, 'num_classrms_need_min_repairs', 
                          edu_normalized$num_classrms_need_min_repairs > edu_normalized$num_classrms_total) 

edu_normalized <- outlierreplace(edu_normalized, 'num_students_total',
                          edu_normalized$num_students_total > 2355)
##writing out.rds
saveRDS(edu_normalized, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Education_774_outliercleaned.rds")


