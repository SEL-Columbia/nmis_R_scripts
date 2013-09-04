source('base_scripts/InstallFormhub.R')
source('cleaning_outliers/outlier_functions.R')

###############################################################################################
######education################################################################################
###############################################################################################                    
e <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Education_661_999Cleaned.csv", header=TRUE)

#total
e$num_toilet.num_toilet_total <- replace(e$num_toilet.num_toilet_boy, is.na(e$num_toilet.num_toilet_boy), 0) + 
  replace(e$num_toilet.num_toilet_girl, 
          is.na(e$num_toilet.num_toilet_girl), 0) + 
  replace(e$num_toilet.num_toilet_both, is.na(e$num_toilet.num_toilet_both), 0)

e$num_toilet.num_toilet_total <- 
  ifelse(e$toilet.none == "FALSE" & e$num_toilet.num_toilet_total == 0, NA, e$num_toilet.num_toilet_total)

e$num_students_total_gender.num_students_total <-  replace(e$num_students_total_gender.num_students_female, 
                                                           is.na(e$num_students_total_gender.num_students_female), 0) +
  replace(e$num_students_total_gender.num_students_male, 
          is.na(e$num_students_total_gender.num_students_male), 0)

e$num_tchrs.num_tchrs_total <- replace(e$num_tchrs.num_tchrs_male, is.na(e$num_tchrs.num_tchrs_male), 0) + 
  replace(e$num_tchrs.num_tchrs_female, is.na(e$num_tchrs.num_tchrs_female), 0) 

#dealing with total students problem
e$test_f_p <- replace(e$num_pry_total_gender.num_pry_female, is.na(e$num_pry_total_gender.num_pry_female), 0) + 
  replace(e$num_js_total_gender.num_js_female, is.na(e$num_js_total_gender.num_js_female), 0) + 
  replace(e$num_ss_total_gender.num_ss_female, is.na(e$num_ss_total_gender.num_ss_female), 0)

e$test_m_p <- replace(e$num_pry_total_gender.num_pry_male, is.na(e$num_pry_total_gender.num_pry_male),0) +
  replace(e$num_js_total_gender.num_js_male, is.na(e$num_js_total_gender.num_js_male),0) + 
  replace(e$num_ss_total_gender.num_ss_male, is.na(e$num_ss_total_gender.num_ss_male), 0)

e$total <- e$test_f + e$test_m

e[which(e$total > e$num_students_total_gender.num_students_total),"num_students_total_gender.num_students_total"] <- 
  e[which(e$total > e$num_students_total_gender.num_students_total),"total"]

#test <- subset(e, select=c(test_f, num_students_total_gender.num_students_female, test_m, 
#                          num_students_total_gender.num_students_male, 
#                         test_total, num_students_total_gender.num_students_total), 
#                         test_total > num_students_total_gender.num_students_total)

#old script paramaters
#logic checks
e <- outlierreplace(e, 'num_tchrs.num_tchrs_male', 
                    (e$num_tchrs.num_tchrs_male > e$num_tchrs.num_tchrs_total))

e <- outlierreplace(e, 'num_tchrs.num_tchrs_female', 
                    (e$num_tchrs.num_tchrs_female > e$num_tchrs.num_tchrs_total))

e <- outlierreplace(e, 'num_tchrs_qualification.num_tchrs_w_nce',
                    (e$num_tchrs_qualification.num_tchrs_w_nce > e$num_tchrs.num_tchrs_total))

e <- outlierreplace(e, 'num_tchrs_attended_training',
                    (e$num_tchrs_attended_training > e$num_tchrs.num_tchrs_total))

e <- outlierreplace(e, 'num_tchrs.num_tchrs_total',
                    (e$num_tchrs.num_tchrs_total > 20 & 
                       e$num_students_total_gender.num_students_total == 0))

e <- outlierreplace(e, 'num_tchrs.num_tchrs_total',
                    (e$num_tchrs.num_tchrs_total > e$num_tchrs.num_tchrs_male + 
                       e$num_tchrs.num_tchrs_female))

e <- outlierreplace(e,'num_classrms_need_maj_repairs',
                    (e$num_classrms_need_maj_repairs > e$num_classrms_total))

e <- outlierreplace(e,'num_classrms_need_min_repairs',
                    (e$num_classrms_need_min_repairs > e$num_classrms_total))

e <- outlierreplace(e,'num_classrms_good_cond',
                    (e$num_classrms_good_cond > e$num_classrms_total))

e <- outlierreplace(e, 'num_students_frthr_than_3km',
                    (e$num_classrms_good_cond > e$num_students_total_gender.num_students_total))

e <- outlierreplace(e, 'num_classrms_good_cond',
                    (e$num_classrms_good_cond + e$num_classrms_need_min_repairs + 
                       e$num_classrms_need_maj_repairs > e$num_classrms_total))
e <- outlierreplace(e, 'num_classrms_need_min_repairs',
                    (e$num_classrms_good_cond + e$num_classrms_need_min_repairs + 
                       e$num_classrms_need_maj_repairs > e$num_classrms_total))
e <- outlierreplace(e, 'num_classrms_need_maj_repairs',
                    (e$num_classrms_good_cond + e$num_classrms_need_min_repairs + 
                       e$num_classrms_need_maj_repairs > e$num_classrms_total))
e <- outlierreplace(e, 'num_classrms_total',
                    (e$num_classrms_good_cond + e$num_classrms_need_min_repairs + 
                       e$num_classrms_need_maj_repairs > e$num_classrms_total))

e <- outlierreplace(e, 'num_students_total_gender.num_students_total',
                    (e$num_tchrs.num_tchrs_total > 20 & 
                       e$num_students_total_gender.num_students_total == 0))
e <- outlierreplace(e, 'num_students_total_gender.num_students_total',
                    e$num_students_total_gender.num_students_total == 0)    

e <- outlierreplace(e, 'num_students_total_gender.num_students_total',
                    (e$num_students_total_gender.num_students_total == 0))

e <- outlierreplace(e, 'num_classrms_good_cond',
                    (e$num_classrms_good_cond + e$num_classrms_need_min_repairs + 
                       e$num_classrms_need_maj_repairs > e$num_classrms_total))
e <- outlierreplace(e, 'num_classrms_need_min_repairs',
                    (e$num_classrms_good_cond + e$num_classrms_need_min_repairs + 
                       e$num_classrms_need_maj_repairs > e$num_classrms_total))
e <- outlierreplace(e, 'num_classrms_need_maj_repairs',
                    (e$num_classrms_good_cond + e$num_classrms_need_min_repairs + 
                       e$num_classrms_need_maj_repairs > e$num_classrms_total))
e <- outlierreplace(e, 'num_classrms_total',
                    (e$num_classrms_good_cond + e$num_classrms_need_min_repairs + 
                       e$num_classrms_need_maj_repairs > e$num_classrms_total))
##ratios

e$ratio_students_to_toilet <-   replace(e$num_students_total_gender.num_students_total, 
                                        is.na(e$num_students_total_gender.num_students_total), 0) /
  replace(e$num_toilet.num_toilet_total, is.na(e$num_toilet.num_toilet_total), 0) 

e$ratio_students_to_desks <-   replace(e$num_students_total_gender.num_students_total, 
                                       is.na(e$num_students_total_gender.num_students_total), 0) / 
  replace(e$num_desks, is.na(e$num_desks), 0) 

e$ratio_students_to_benches <-   replace(e$num_students_total_gender.num_students_total, 
                                         is.na(e$num_students_total_gender.num_students_total), 0) /
  replace(e$num_benches, is.na(e$num_benches), 0) 

##textbooks
e$ratio_pupil_math_pry_textbook <- replace(e$manuals_pry.num_math_textbook_pry, 
                                           is.na(e$manuals_pry.num_math_textbook_pry), 0) / 
  replace(e$num_pry_total_gender.num_pry_total,
          is.na(e$num_pry_total_gender.num_pry_total), 0)

e$ratio_pupil_english_pry_textbook <- replace(e$manuals_pry.num_english_textbook_pry, 
                                              is.na(e$manuals_pry.num_english_textbook_pry), 0) / 
  replace(e$num_pry_total_gender.num_pry_total,
          is.na(e$num_pry_total_gender.num_pry_total), 0) 
e$ratio_pupil_socscience_pry_textbook <- replace(e$manuals_pry.num_soc_science_textbook_pry, 
                                                 is.na(e$manuals_pry.num_soc_science_textbook_pry), 0) / 
  replace(e$num_pry_total_gender.num_pry_total,
          is.na(e$num_pry_total_gender.num_pry_total), 0)
e$ratio_pupil_science_pry_textbook <- replace(e$manuals_pry.num_science_textbook_pry, 
                                              is.na(e$manuals_pry.num_science_textbook_pry), 0) / 
  replace(e$num_pry_total_gender.num_pry_total,
          is.na(e$num_pry_total_gender.num_pry_total), 0)

e$ratio_pupil_math_js_textbook <- replace(e$manuals_js.num_math_textbook_js, 
                                          is.na(e$manuals_js.num_math_textbook_js), 0) / 
  replace(e$num_js_total_gender.num_js_total,
          is.na(e$num_js_total_gender.num_js_total), 0)
e$ratio_pupil_english_js_textbook <- replace(e$manuals_js.num_english_textbook_js, 
                                             is.na(e$manuals_js.num_english_textbook_js), 0) / 
  replace(e$num_js_total_gender.num_js_total,
          is.na(e$num_js_total_gender.num_js_total), 0)
e$ratio_pupil_socscience_js_textbook <- replace(e$manuals_js.num_soc_science_textbook_js, 
                                                is.na(e$manuals_js.num_soc_science_textbook_js), 0) / 
  replace(e$num_js_total_gender.num_js_total,
          is.na(e$num_js_total_gender.num_js_total), 0)
e$ratio_pupil_science_js_textbook <- replace(e$manuals_js.num_science_textbook_js, 
                                             is.na(e$manuals_js.num_science_textbook_js), 0) / 
  replace(e$num_js_total_gender.num_js_total,
          is.na(e$num_js_total_gender.num_js_total), 0)  

e <- outlierreplace(e, 'manuals_pry.num_math_textbook_pry', 
                    (e$ratio_pupil_math_pry_textbook > 10))
e <- outlierreplace(e, 'manuals_pry.num_english_textbook_pry', 
                    (e$ratio_pupil_english_pry_textbook > 10))
e <- outlierreplace(e, 'manuals_pry.num_soc_science_textbook_pry', 
                    (e$ratio_pupil_socscience_pry_textbook > 10))
e <- outlierreplace(e, 'manuals_pry.num_science_textbook_pry', 
                    (e$ratio_pupil_science_pry_textbook > 10))
e <- outlierreplace(e, 'manuals_js.num_math_textbook_js', 
                    (e$ratio_pupil_math_js_textbook > 10))
e <- outlierreplace(e, 'manuals_js.num_english_textbook_js', 
                    (e$ratio_pupil_english_js_textbook > 10))
e <- outlierreplace(e, 'manuals_js.num_soc_science_textbook_js', 
                    (e$ratio_pupil_socscience_js_textbook > 10))
e <- outlierreplace(e, 'manuals_js.num_science_textbook_js', 
                    (e$ratio_pupil_science_js_textbook > 10))

#new data points

#e$pupil_class_ratio <- e$num_students_total_gender.num_students_total/e$num_classrms_total
#e <- outlierreplace(e, 'num_students_total_gender.num_students_total', 
#                  (e$pupil_class_ratio < 5 | e$pupil_class_ratio > 150))
#e <- outlierreplace(e, 'num_classrms_total', 
#                   (e$pupil_class_ratio < 5 | e$pupil_class_ratio > 150))
e <- outlierreplace(e, 'num_students_total_gender.num_students_female',
                    (e$num_students_total_gender.num_students_female > 11000))
e <- outlierreplace(e, 'num_students_total_gender.num_students_female',
                    (e$num_students_total_gender.num_students_female > 3000))
e <- outlierreplace(e, 'num_students_total_gender.num_students_male',
                    (e$num_students_total_gender.num_students_male > 2500  & e$num_classrms_total < 25))
e <- outlierreplace(e, 'num_students_total_gender.num_students_total',
                    (e$num_students_total_gender.num_students_total > 2000 & e$num_classrms_total < 25 &
                       e$num_tchrs.num_tchrs_total < 10))
e <- outlierreplace(e, 'num_pry_total_gender.num_pry_female',
                    (e$num_pry_total_gender.num_pry_female > 2000 & e$num_classrms_total < 25 &
                       e$num_tchrs.num_tchrs_total < 10))
e <- outlierreplace(e, 'num_pry_total_gender.num_pry_male',
                    (e$num_pry_total_gender.num_pry_male > 2000 & e$num_classrms_total < 25 &
                       e$num_tchrs.num_tchrs_total < 10))
e <- outlierreplace(e, 'num_pry_total_gender.num_pry_total',
                    (e$num_pry_total_gender.num_pry_total > 2500 & e$num_classrms_total < 25 &
                       e$num_tchrs.num_tchrs_total < 10))
e <- outlierreplace(e, 'num_js_total_gender.num_js_female',
                    (e$num_js_total_gender.num_js_female > 1250 & e$num_classrms_total < 25 &
                       e$num_tchrs.num_tchrs_total < 10))
e <- outlierreplace(e, 'num_js_total_gender.num_js_male',
                    (e$num_js_total_gender.num_js_male > 1250 & e$num_classrms_total < 25 &
                       e$num_tchrs.num_tchrs_total < 10))

e <- outlierreplace(e, 'num_js_total_gender.num_js_total',
                    (e$num_js_total_gender.num_js_total > 2500 & e$num_classrms_total < 25 &
                       e$num_tchrs.num_tchrs_total < 10))

e <- outlierreplace(e, 'num_ss_total_gender.num_ss_female',
                    (e$num_ss_total_gender.num_ss_female > 1250 & e$num_classrms_total < 25 &
                       e$num_tchrs.num_tchrs_total < 10))
e <- outlierreplace(e, 'num_ss_total_gender.num_ss_male',
                    (e$num_ss_total_gender.num_ss_male > 1250 & e$num_classrms_total < 25 &
                       e$num_tchrs.num_tchrs_total < 10))
e <- outlierreplace(e, 'num_ss_total_gender.num_ss_total',
                    (e$num_ss_total_gender.num_ss_total > 2500 & e$num_classrms_total < 25 &
                       e$num_tchrs.num_tchrs_total < 10))
e <- outlierreplace(e, 'num_ss_total_gender.num_ss_total',
                    (e$km_to_catchment_area > 30))
e <- outlierreplace(e, 'km_to_catchment_area',
                    (e$km_to_catchment_area > 55))                    
e <- outlierreplace(e, 'km_to_secondary_school',
                    (e$km_to_secondary_school > 25))
e <- outlierreplace(e, 'num_students_frthr_than_3km',
                    (e$num_students_frthr_than_3km > 1250))            
e <- outlierreplace(e, 'num_toilet.num_toilet_girl',
                    (e$num_toilet.num_toilet_girl > 750))            
e <- outlierreplace(e, 'num_toilet.num_toilet_both',
                    (e$num_toilet.num_toilet_both > 1000))            
e <- outlierreplace(e, 'num_toilet.num_toilet_total',
                    (e$num_toilet.num_toilet_total > 1000))                        
e <- outlierreplace(e, 'num_tchrs.num_tchrs_male',
                    (e$num_tchrs.num_tchrs_male > 100))
e <- outlierreplace(e, 'num_tchrs.num_tchrs_female',
                    (e$num_tchrs.num_tchrs_female > 100))
e <- outlierreplace(e, 'num_tchrs_qualification.num_tchrs_w_nce',
                    (e$num_tchrs_qualification.num_tchrs_w_nce > 100))
e <- outlierreplace(e, 'num_tchrs_qualification.num_tchrs_w_nce_plus',
                    (e$num_tchrs_qualification.num_tchrs_w_nce_plus > 100))            
e <- outlierreplace(e, 'num_sr_staff_total',
                    (e$num_sr_staff_total > 75))
e <- outlierreplace(e, 'num_jr_staff_total',
                    (e$num_jr_staff_total > 50))
e <- outlierreplace(e, 'num_tchrs_attended_training',
                    (e$num_tchrs_attended_training > 100))
e <- outlierreplace(e, 'num_classrms_total',
                    (e$num_tchrs_attended_training > 99))
e <- outlierreplace(e, 'num_classrms_total',
                    (e$num_classrms_total == 0))
e <- outlierreplace(e, 'num_classrms_need_min_repairs',
                    (e$num_classrms_need_min_repairs > 50))
e <- outlierreplace(e, 'num_classrms_need_maj_repairs',
                    (e$num_classrms_need_maj_repairs > 50))
#absolute example
e <- outlierreplace(e, 'num_classrms_good_cond',
                    (e$num_classrms_good_cond > 260))                    
#ratio/ combining other data points example
#e <- outlierreplace(e, 'num_desks',
#     (e$ratio_students_to_desks > 750))
e <- outlierreplace(e, 'num_desks',
                    (e$num_desks > 2500))
e <- outlierreplace(e, 'num_benches',
                    (e$num_benches > 2500))
e <- outlierreplace(e, 'num_classrm_w_chalkboard',
                    (e$num_classrm_w_chalkboard > e$num_classrms_total))      
#another absolute
e <- outlierreplace(e, 'manuals_pry.num_math_textbook_pry',
                    (e$manuals_pry.num_math_textbook_pry > 3000))      
e <- outlierreplace(e, 'manuals_pry.num_english_textbook_pry',
                    (e$manuals_pry.num_english_textbook_pry > 3000))      
e <- outlierreplace(e, 'manuals_pry.num_soc_science_textbook_pry',
                    (e$manuals_pry.num_soc_science_textbook_pry > 3000))      
e <- outlierreplace(e, 'manuals_pry.num_science_textbook_pry',
                    (e$manuals_pry.num_science_textbook_pry > 3000))      
e <- outlierreplace(e, 'manuals_js.num_math_textbook_js',
                    (e$manuals_js.num_math_textbook_js > 3000))      
e <- outlierreplace(e, 'manuals_js.num_english_textbook_js',
                    (e$manuals_js.num_english_textbook_js > 3000))      
e <- outlierreplace(e, 'manuals_js.num_soc_science_textbook_js',
                    (e$manuals_js.num_soc_science_textbook_js > 3000))      
e <- outlierreplace(e, 'manuals_js.num_science_textbook_js',
                    (e$manuals_js.num_science_textbook_js > 3000))      
e <- outlierreplace(e, 'num_exercise_books_per_student_pry',
                    which((e$num_exercise_books_per_student_pry > 19800) | 
                            (e$num_students_total_gender.num_students_total * 8 < 
                               e$num_exercise_books_per_student_pry)))
e <- outlierreplace(e, 'num_exercise_books_per_student_jss',
                    which((e$num_exercise_books_per_student_jss > 34000) | 
                            (e$num_students_total_gender.num_students_total * 8 < 
                               e$num_exercise_books_per_student_jss)))  
e <- outlierreplace(e, 'ratio_students_to_toilet',
                    (e$ratio_students_to_toilet > 1000))      
e <- outlierreplace(e, 'ratio_students_to_desks',
                    (e$ratio_students_to_desks > 1000)) 
e <- outlierreplace(e, 'ratio_students_to_benches',
                    (e$ratio_students_to_benches > 1000)) 

e$ratio_students_to_toilet <-   replace(e$num_students_total_gender.num_students_total, 
                                        is.na(e$num_students_total_gender.num_students_total), 0) /
  replace(e$num_toilet.num_toilet_total, is.na(e$num_toilet.num_toilet_total), 0) 

e$ratio_students_to_toilet <- 
  ifelse(e$ratio_students_to_toilet == 0, NA, e$ratio_students_to_toilet)

e$ratio_students_to_desks <-   replace(e$num_students_total_gender.num_students_total, 
                                       is.na(e$num_students_total_gender.num_students_total), 0) / 
  replace(e$num_desks, is.na(e$num_desks), 0) 
e$ratio_students_to_desks <- 
  ifelse(e$ratio_students_to_desks == 0, NA, e$ratio_students_to_desks)

e$ratio_students_to_benches <-   replace(e$num_students_total_gender.num_students_total, 
                                         is.na(e$num_students_total_gender.num_students_total), 0) /
  replace(e$num_benches, is.na(e$num_benches), 0) 
e$ratio_students_to_benches <- 
  ifelse(e$ratio_students_to_benches == 0, NA, e$ratio_students_to_benches)

##textbooks
e$ratio_pupil_math_pry_textbook <- replace(e$manuals_pry.num_math_textbook_pry, 
                                           is.na(e$manuals_pry.num_math_textbook_pry), 0) / 
  replace(e$num_pry_total_gender.num_pry_total,
          is.na(e$num_pry_total_gender.num_pry_total), 0)

e$ratio_pupil_english_pry_textbook <- replace(e$manuals_pry.num_english_textbook_pry, 
                                              is.na(e$manuals_pry.num_english_textbook_pry), 0) / 
  replace(e$num_pry_total_gender.num_pry_total,
          is.na(e$num_pry_total_gender.num_pry_total), 0) 
e$ratio_pupil_socscience_pry_textbook <- replace(e$manuals_pry.num_soc_science_textbook_pry, 
                                                 is.na(e$manuals_pry.num_soc_science_textbook_pry), 0) / 
  replace(e$num_pry_total_gender.num_pry_total,
          is.na(e$num_pry_total_gender.num_pry_total), 0)
e$ratio_pupil_science_pry_textbook <- replace(e$manuals_pry.num_science_textbook_pry, 
                                              is.na(e$manuals_pry.num_science_textbook_pry), 0) / 
  replace(e$num_pry_total_gender.num_pry_total,
          is.na(e$num_pry_total_gender.num_pry_total), 0)

e$ratio_pupil_math_js_textbook <- replace(e$manuals_js.num_math_textbook_js, 
                                          is.na(e$manuals_js.num_math_textbook_js), 0) / 
  replace(e$num_js_total_gender.num_js_total,
          is.na(e$num_js_total_gender.num_js_total), 0)
e$ratio_pupil_english_js_textbook <- replace(e$manuals_js.num_english_textbook_js, 
                                             is.na(e$manuals_js.num_english_textbook_js), 0) / 
  replace(e$num_js_total_gender.num_js_total,
          is.na(e$num_js_total_gender.num_js_total), 0)
e$ratio_pupil_socscience_js_textbook <- replace(e$manuals_js.num_soc_science_textbook_js, 
                                                is.na(e$manuals_js.num_soc_science_textbook_js), 0) / 
  replace(e$num_js_total_gender.num_js_total,
          is.na(e$num_js_total_gender.num_js_total), 0)
e$ratio_pupil_science_js_textbook <- replace(e$manuals_js.num_science_textbook_js, 
                                             is.na(e$manuals_js.num_science_textbook_js), 0) / 
  replace(e$num_js_total_gender.num_js_total,
          is.na(e$num_js_total_gender.num_js_total), 0)  
#other ratios
e$num_toilet.num_toilet_total <- replace(e$num_toilet.num_toilet_boy, is.na(e$num_toilet.num_toilet_boy), 0) + 
  replace(e$num_toilet.num_toilet_girl, 
          is.na(e$num_toilet.num_toilet_girl), 0) + 
  replace(e$num_toilet.num_toilet_both, is.na(e$num_toilet.num_toilet_both), 0)

e$num_toilet.num_toilet_total <- 
  ifelse(e$toilet.none == "FALSE" & e$num_toilet.num_toilet_total == 0, NA, e$num_toilet.num_toilet_total)

e$num_students_total_gender.num_students_total <-  replace(e$num_students_total_gender.num_students_female, 
                                                           is.na(e$num_students_total_gender.num_students_female), 0) +
  replace(e$num_students_total_gender.num_students_male, 
          is.na(e$num_students_total_gender.num_students_male), 0)

e$num_students_total_gender.num_students_total <- 
  ifelse(e$num_students_total_gender.num_students_total == 0, NA, e$num_students_total_gender.num_students_total)

e$num_tchrs.num_tchrs_total <- replace(e$num_tchrs.num_tchrs_male, is.na(e$num_tchrs.num_tchrs_male), 0) + 
  replace(e$num_tchrs.num_tchrs_female, is.na(e$num_tchrs.num_tchrs_female), 0) 

e <- outlierreplace(e, 'ratio_students_to_toilet',
                    (e$ratio_students_to_toilet > 1000))      
e <- outlierreplace(e, 'ratio_students_to_desks',
                    (e$ratio_students_to_desks > 1000)) 
e <- outlierreplace(e, 'ratio_students_to_benches',
                    (e$ratio_students_to_benches > 1000)) 

#### TODO: investigate why these are in the data at all; constraints should have knocked these out #####

# replace all classroom indicators if they are below total classrooms
e <- outlierreplace(e, 'num_classrms_need_min_repairs',e$num_classrms_need_min_repairs > e$num_classrms_total) 
e <- outlierreplace(e, 'num_classrms_need_maj_repairs',e$num_classrms_need_maj_repairs > e$num_classrms_total) 

##writing out csv
write.csv(e, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Education_661_outliercleaned.csv", row.names=FALSE)
rm(e)                     


###############################################################################################
######health###################################################################################
###############################################################################################                    
h <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Health_661_999Cleaned.csv", header=TRUE)

#old script paramaters 
    h <- outlierreplace(h, 'medical_staff_active.num_doctors_active',
          (h$medical_staff_active.num_doctors_active > 12 & 
            (h$facility_type != "teachinghospital" & 
            h$facility_type != "federalmedicalcentre")))
    
    h <- outlierreplace(h, 'medical_staff_active.num_doctors_active',
          (h$medical_staff_active.num_doctors_active > 20 & 
            (h$facility_type == "teachinghospital" | 
            h$facility_type == "federalmedicalcentre")))
    
    h <- outlierreplace(h, 'medical_staff_active.num_nurses_active',
          (h$medical_staff_active.num_nurses_active > 16 & 
            (h$facility_type != "teachinghospital" & 
            h$facility_type != "federalmedicalcentre")))
    
    h <- outlierreplace(h, 'medical_staff_active.num_nurses_active',
          (h$medical_staff_active.num_nurses_active > 24 & 
            (h$facility_type == "teachinghospital" | 
            h$facility_type == "federalmedicalcentre")))
    
    h <- outlierreplace(h, 'medical_staff_active.num_midwives_active',
          (h$medical_staff_active.num_midwives_active > 24 & 
            (h$facility_type == "teachinghospital" | 
            h$facility_type == "federalmedicalcentre")))
    
    h <- outlierreplace(h, 'medical_staff_active.num_nursemidwives_active',
          (h$medical_staff_active.num_nursemidwives_active > 16 & 
            (h$facility_type != "teachinghospital" & 
            h$facility_type != "federalmedicalcentre")))
    
    h <- outlierreplace(h, 'medical_staff_active.num_nursemidwives_active',
          (h$medical_staff_active.num_nursemidwives_active > 24 & 
            (h$facility_type == "teachinghospital" | 
            h$facility_type == "federalmedicalcentre")))

    h <- outlierreplace(h, 'medical_staff_active.num_nursemidwives_active',
          (h$medical_staff_active.num_nursemidwives_active > 50))
    
    h <- outlierreplace(h, 'medical_staff_active.num_junior_chews_active',
          (h$medical_staff_active.num_junior_chews_active > 50))

#new data points

h <- outlierreplace(h, 'facility_type',
                    (((h$medical_staff_posted.num_doctors_posted < 30 & 
                      h$medical_staff_posted.num_doctors_posted != 0) & 
                      (h$medical_staff_posted.num_midwives_posted < 30 & 
                      h$medical_staff_posted.num_midwives_posted != 0) &
                      (h$medical_staff_posted.num_nurses_posted < 30 & 
                      h$medical_staff_posted.num_nurses_posted != 0)) &
                      (h$facility_type == "teachinghospital" | 
                      h$facility_type == "federalmedicalcentre")))

h <- outlierreplace(h, 'medical_staff_posted.num_doctors_posted',
      ((h$medical_staff_posted.num_doctors_posted > 500 | 
        h$medical_staff_posted.num_doctors_posted < 100) & 
        (h$facility_type == "teachinghospital"  |
        h$facility_type == "federalmedicalcentre")
        ))

h <- outlierreplace(h, 'medical_staff_posted.num_nurses_posted',
                    (h$medical_staff_posted.num_nurses_posted < 100 &
                      (h$facility_type == "teachinghospital" | 
                      h$facility_type == "federalmedicalcentre")))

h <- outlierreplace(h, 'medical_staff_posted.num_midwives_posted',
                    (h$medical_staff_posted.num_midwives_posted < 100 & 
                      (h$facility_type == "teachinghospital" | 
                      h$facility_type == "federalmedicalcentre")))

h <- outlierreplace(h, 'medical_staff_posted.num_nursemidwives_posted',
                    (h$medical_staff_posted.num_nursemidwives_posted < 50 &
                      (h$facility_type == "teachinghospital" | 
                      h$facility_type == "federalmedicalcentre")))
        
h <- outlierreplace(h, 'medical_staff_posted.num_midwives_posted',
      (h$medical_staff_posted.num_midwives_posted > 16 & 
        (h$facility_type != "teachinghospital" & 
        h$facility_type != "federalmedicalcentre")))
               
h <- outlierreplace(h, 'medical_staff_posted.num_nurses_posted',
      (h$medical_staff_posted.num_nurses_posted > 16 & 
        (h$facility_type != "teachinghospital" & 
        h$facility_type != "federalmedicalcentre")))            

h <- outlierreplace(h, 'medical_staff_posted.num_nursemidwives_posted',
                    (h$medical_staff_posted.num_nursemidwives_posted > 16 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))

h <- outlierreplace(h, 'medical_staff_posted.num_cho_posted',
                    (h$medical_staff_posted.num_cho_posted > 16 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))


h <- outlierreplace(h, 'medical_staff_posted.num_cho_posted',
                    (h$medical_staff_posted.num_cho_posted > 24 &
                      (h$facility_type == "teachinghospital" | 
                      h$facility_type == "federalmedicalcentre")))

h <- outlierreplace(h, 'medical_staff_posted.num_chews_posted',
                    (h$medical_staff_posted.num_chews_posted > 50 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))

h <- outlierreplace(h, 'medical_staff_posted.num_chews_posted',
                    (h$medical_staff_posted.num_chews_posted > 50 &
                      (h$facility_type == "teachinghospital" | 
                      h$facility_type == "federalmedicalcentre")))

h <- outlierreplace(h, 'medical_staff_posted.num_junior_chews_posted',
                    (h$medical_staff_posted.num_junior_chews_posted > 50 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))   

h <- outlierreplace(h, 'medical_staff_posted.num_junior_chews_posted',
                    (h$medical_staff_posted.num_junior_chews_posted > 24 &
                      (h$facility_type == "teachinghospital" | 
                      h$facility_type == "federalmedicalcentre")))

h <- outlierreplace(h, 'medical_staff_posted.pharmacists_posted',
                    (h$medical_staff_posted.pharmacists_posted > 5 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))

h <- outlierreplace(h, 'medical_staff_posted.pharmacists_posted',
                    (h$medical_staff_posted.pharmacists_posted < 5 &
                      (h$facility_type == "teachinghospital" | 
                      h$facility_type == "federalmedicalcentre")))

h <- outlierreplace(h, 'medical_staff_posted.environmental_health_officers_posted',
                    (h$medical_staff_posted.environmental_health_officers_posted > 6 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))      

h <- outlierreplace(h, 'medical_staff_posted.lab_technicians_posted',
                    (h$medical_staff_posted.lab_technicians_posted > 4 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))
      
h <- outlierreplace(h, 'medical_staff_posted.pharma_technicians_posted',
                    (h$medical_staff_posted.pharma_technicians_posted > 5 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))
       
h <- outlierreplace(h, 'medical_staff_posted.medical_records_officers_posted',
                    (h$medical_staff_posted.medical_records_officers_posted > 4 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))

# 
h <- outlierreplace(h, 'inpatient_care_num_beds',
                    (h$inpatient_care_num_beds > 50 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))

h <- outlierreplace(h, 'not_for_private_1.toilets_available.num_flush_or_pour_flush_piped',
                    (h$not_for_private_1.toilets_available.num_flush_or_pour_flush_piped > 20 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))

h <- outlierreplace(h, 'not_for_private_1.toilets_available.num_flush_other',
                    (h$not_for_private_1.toilets_available.num_flush_other > 10 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))

h <- outlierreplace(h, 'not_for_private_1.toilets_available.num_vip_latrine',
                    (h$not_for_private_1.toilets_available.num_vip_latrine > 6 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))

h <- outlierreplace(h, 'not_for_private_1.toilets_available.num_pit_w_slab',
                    (h$not_for_private_1.toilets_available.num_pit_w_slab > 8 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))

h <- outlierreplace(h, 'not_for_private_1.toilets_available.num_open_pit_latrine',
                    (h$not_for_private_1.toilets_available.num_open_pit_latrine > 4 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))

h <- outlierreplace(h, 'not_for_private_1.toilets_available.num_bucket_system',
                    (h$not_for_private_1.toilets_available.num_bucket_system > 10 & 
                      (h$facility_type != "teachinghospital" & 
                      h$facility_type != "federalmedicalcentre")))

h <- outlierreplace(h, 'not_for_private_1.toilets_available.num_bucket_system',
                    (h$not_for_private_1.toilets_available.num_bucket_system > 10 & 
                      (h$facility_type == "teachinghospital" | 
                      h$facility_type == "federalmedicalcentre")))

write.csv(h, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Health_661_outliercleaned.csv", row.names=FALSE)
rm(h)

###############################################################################################
######water####################################################################################
############################################################################################### 

file.copy("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Water_661_999Cleaned_Reclassified.csv", 
          "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Water_661_outliercleaned.csv",
          overwrite=TRUE)



