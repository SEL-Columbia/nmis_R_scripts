###############################
####EDUCATION####
###############################
e <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Education_661_999Cleaned.csv", header=TRUE)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
library("ggplot2")

"cells<-" = function(df,cols,rows,value) {
  for (r in rows) { for (c in cols) { df[[r, c]] <- value } }
  df
}

################################
####old scripts####   
################################
#logic
  #tchrs_male_nce > num_tchrs.num_tchrs_total
    e_tchrs_male <- subset(e, subset=(num_tchrs.num_tchrs_male > num_tchrs.num_tchrs_total), select=c(num_tchrs.num_tchrs_male,num_tchrs.num_tchrs_total)) 
  #tchrs_female_nce > num_tchrs.num_tchrs_total
    e_tchrs_female <- subset(e, subset=(num_tchrs.num_tchrs_female > num_tchrs.num_tchrs_total), select=c(num_tchrs.num_tchrs_female,num_tchrs.num_tchrs_total)) 
  #num_tchrs_qualification.num_tchrs_w_nce
    e_tchrs_nce <- subset(e, subset=(num_tchrs_qualification.num_tchrs_w_nce > num_tchrs.num_tchrs_total), select=c(num_tchrs_qualification.num_tchrs_w_nce,num_tchrs.num_tchrs_total))
  #num_tchrs_attended_training
    e_tchrs_attended_training <- subset(e, subset=(num_tchrs_attended_training > num_tchrs.num_tchrs_total), select=c(num_tchrs_attended_training,num_tchrs.num_tchrs_total))

#num_tchers_total greater than 20, and num_students_total is zero
  e_teachers20_pupils0 <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_tchrs.num_tchrs_total, num_students_total_gender.num_students_total), subset=(num_tchrs.num_tchrs_total > 20 & num_students_total_gender.num_students_total == 0))  
    #cleaning
  cells(e, c('num_tchrs.num_tchrs_total', 'num_students_total_gender.num_students_total'),
        which(e$num_tchrs.num_tchrs_total > 20 & e$num_students_total_gender.num_students_total == 0)) <- NA

#num_students_total is zero
  e_total_students <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_tchrs.num_tchrs_total, num_classrms_total, num_students_total_gender.num_students_total), subset=(num_students_total_gender.num_students_total == 0))  
  e_total_students_tchrs_clasrooms0 <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_tchrs.num_tchrs_total, num_classrms_total, num_students_total_gender.num_students_total), subset=(num_tchrs.num_tchrs_total == 0 & num_students_total_gender.num_students_total == 0 & num_classrms_total == 0))  
  # should we eliminate all values if these three are NA? we can't get any indicators...
    #cleaning
    cells(e, 'num_students_total_gender.num_students_total',
          which(e$num_students_total_gender.num_students_total == 0)) <- NA
    
#pupil to textbook ratio   
  #rule, if greater than 1 - replace with 1. 
  #columns
  e$ratio_pupil_math_pry_textbook <- e$manuals_pry.num_math_textbook_pry/e$num_pry_total_gender.num_pry_total
  e$ratio_pupil_english_pry_textbook <- e$manuals_pry.num_english_textbook_pry/e$num_pry_total_gender.num_pry_total  
  e$ratio_pupil_socscience_pry_textbook <- e$manuals_pry.num_soc_science_textbook_pry/e$num_pry_total_gender.num_pry_total  
  e$ratio_pupil_science_pry_textbook <- e$manuals_pry.num_science_textbook_pry/e$num_pry_total_gender.num_pry_total  
    e$ratio_pupil_math_js_textbook <- e$manuals_js.num_math_textbook_js/e$num_js_total_gender.num_js_total  
    e$ratio_pupil_english_js_textbook <- e$manuals_js.num_english_textbook_js/e$num_js_total_gender.num_js_total 
    e$ratio_pupil_socscience_js_textbook <- e$manuals_js.num_soc_science_textbook_js/e$num_js_total_gender.num_js_total  
    e$ratio_pupil_science_js_textbook <- e$manuals_js.num_science_textbook_js/e$num_js_total_gender.num_js_total  
  #subsets
    e_ratio_pupil_math_pry_textbook <- 
      subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, ratio_pupil_math_pry_textbook), ratio_pupil_math_pry_textbook > 10)
    e_ratio_pupil_english_pry_textbook <- 
      subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, ratio_pupil_english_pry_textbook), ratio_pupil_english_pry_textbook > 10)
    e_ratio_pupil_socscience_pry_textbook <-
      subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, ratio_pupil_socscience_pry_textbook), ratio_pupil_socscience_pry_textbook > 10)
    e_ratio_pupil_science_pry_textbook <- 
      subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, ratio_pupil_science_pry_textbook), ratio_pupil_science_pry_textbook > 10)
      e_ratio_pupil_math_js_textbook <- 
        subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, ratio_pupil_math_js_textbook), ratio_pupil_math_js_textbook > 10)
      e_ratio_pupil_english_js_textbook <- 
        subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, ratio_pupil_english_js_textbook), ratio_pupil_english_js_textbook > 10)
      e_ratio_pupil_socscience_js_textbook <-
        subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, ratio_pupil_socscience_js_textbook), ratio_pupil_socscience_js_textbook > 10)
      e_ratio_pupil_science_js_textbook <- 
        subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, ratio_pupil_science_js_textbook), ratio_pupil_science_js_textbook > 10)
    #cleaning
    cells(e, 'ratio_pupil_math_pry_textbook', 
          which(e$ratio_pupil_math_pry_textbook > 10)) <- 10
    cells(e, 'ratio_pupil_english_pry_textbook', 
          which(e$ratio_pupil_english_pry_textbook > 10)) <- 10
    cells(e, 'ratio_pupil_socscience_pry_textbook', 
          which(e$ratio_pupil_socscience_pry_textbook > 10)) <- 10  
    cells(e, 'ratio_pupil_science_pry_textbook', 
          which(e$ratio_pupil_science_pry_textbook > 10)) <- 10
    cells(e, 'ratio_pupil_math_js_textbook', 
          which(e$ratio_pupil_math_js_textbook > 10)) <- 10
    cells(e, 'ratio_pupil_english_js_textbook', 
          which(e$ratio_pupil_english_js_textbook > 10)) <- 10
    cells(e, 'ratio_pupil_socscience_js_textbook', 
          which(e$ratio_pupil_socscience_js_textbook > 10)) <- 10
    cells(e, 'ratio_pupil_science_js_textbook', 
          which(e$ratio_pupil_science_js_textbook > 10)) <- 10

#pupil to class
  #show
  qplot(data=e, x=num_students_total_gender.num_students_total, y=num_classrms_total ) 
    e$pupil_class_ratio <- e$num_students_total_gender.num_students_total/e$num_classrms_total
    #subset  
    e_pupil_class_ratio <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, pupil_class_ratio), subset=(pupil_class_ratio < 5 | pupil_class_ratio > 150)) 
  #cleaning
    cells(e, c('num_students_total_gender.num_students_total','num_classrms_total'), 
          which(e$pupil_class_ratio < 5 | e$pupil_class_ratio > 150)) <- NA

################################
####other ratios####   
################################
#pupil to teacher
    #show
  qplot(data=e, x=num_tchrs.num_tchrs_total, y=num_students_total_gender.num_students_total) + scale_x_continuous(limits=c(0,5000)) + coord_flip()
    #save
  e$ratio_student_teacher <- e$num_students_total_gender.num_students_total/e$num_tchrs.num_tchrs_total
  eratio_student_teacher <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, ratio_student_teacher), ratio_student_teacher <= 5)
    #cleaning
    cells(e, c('num_students_total_gender.num_students_total', 'num_tchrs.num_tchrs_total'), 
          which(e$num_students_total_gender.num_students_total/e$num_tchrs.num_tchrs_total > 10)) <- NA

################################ 
####misunderstood questions ##### 
################################
#num_exercise_books_per_student_pry
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#0.00    83.75   260.00   725.20   700.00 70780.00    52691 	

#num_exercise_books_per_student_jss
#Min.       1stQu.  Median  Mean   3rd Qu.  Max.    NA's 
#0      60     569    1818    1738   34740   57139

###############################
########graphs/tables##########
###############################

#####num_students_total_gender.num_students_female##### .... 3000
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_students_total_gender.num_students_female, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Students Female') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,3000))
    #cleaning
    #e_num_students_total_gender.num_students_female <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_students_total_gender.num_students_female), num_students_total_gender.num_students_female < )

#log 10 
#ggplot(e, aes(x=mylga_zone, y=log10(num_students_frthr_than_3km)), fill=mylga_zone) + coord_flip() + 
#  geom_boxplot() + ylab('Number of Students Farther Than 3km') + xlab('Zone') + scale_fill_manual(values=cbPalette)

#####num_students_total_gender.num_students_male#####  .... 2500 ... 25 classrooms
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_students_total_gender.num_students_male, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Students Male') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) 
    #cleaning
    #e_num_students_total_gender.num_students_male <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_students_total_gender.num_students_male, num_tchrs.num_tchrs_total, num_classrms_total, level_of_education), num_students_total_gender.num_students_male > 2500 & num_classrms_total < 25 )

#####num_students_total_gender.num_students_total#####  2000 students .... 25 class rooms.... 10 teachers
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_pry_total_gender.num_pry_female, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Students Total') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) 
    #cleaning
    #e_num_students_total_gender.num_students_total <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, level_of_education, num_tchrs.num_tchrs_total, num_students_total_gender.num_students_total, num_tchrs.num_tchrs_total, num_classrms_total, level_of_education), 
      #num_students_total_gender.num_students_total > 2000 &  num_classrms_total <= 25)

#####num_pry_total_gender.num_pry_female##### ... same
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_pry_total_gender.num_pry_female, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Primary Female') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) 
    #cleaning
    #e_num_pry_total_gender.num_pry_female <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_pry_total_gender.num_pry_total, num_pry_total_gender.num_pry_female), num_pry_total_gender.num_pry_female < )

#####num_pry_total_gender.num_pry_male#####  .... same
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_pry_total_gender.num_pry_male, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Primary Male') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) 
    #cleaning
    #e_num_pry_total_gender.num_pry_male <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_pry_total_gender.num_pry_total, num_pry_total_gender.num_pry_male), num_pry_total_gender.num_pry_male < )

#####num_pry_total_gender.num_pry_total#####    2500 studnets.... same teacher/class
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_pry_total_gender.num_pry_total, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Primary Total') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) 
    #cleaning
    #e_num_pry_total_gender.num_pry_total <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_pry_total_gender.num_pry_total), num_pry_total_gender.num_pry_total < )

#####num_js_total_gender.num_js_female#####   1250.... same class/teachers
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_js_total_gender.num_js_female, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of JS Female') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) 
    #cleaning
    #e_num_js_total_gender.num_js_female <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_js_total_gender.num_js_total, num_js_total_gender.num_js_female), num_js_total_gender.num_js_female < )

#####num_js_total_gender.num_js_male#####   1000... same class/ teachers         
    #by zone 
    ggplot(e, aes(x=mylga_zone, y=num_js_total_gender.num_js_male, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of JS Male') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) 
    #cleaning
    #e_num_js_total_gender.num_js_male <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_js_total_gender.num_js_total, num_js_total_gender.num_js_male), num_js_total_gender.num_js_male < )

#####num_js_total_gender.num_js_total##### ... 2500/ same class teacher              
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_js_total_gender.num_js_total, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of students total JS') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) 
    #cleaning
    #e_num_js_total_gender.num_js_total <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_js_total_gender.num_js_total), num_js_total_gender.num_js_total < )

#####num_ss_total_gender.num_ss_female#####  1250 ... same teach/class
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_ss_total_gender.num_ss_female, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of SS Female') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) 
    #cleaning
    #e_num_ss_total_gender.num_ss_female <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_ss_total_gender.num_ss_total, num_ss_total_gender.num_ss_female), num_ss_total_gender.num_ss_female < )

#####num_ss_total_gender.num_ss_male#####  1250 ... same teach/class              
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_ss_total_gender.num_ss_male, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of SS Male') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) 
    #cleaning
    #e_num_ss_total_gender.num_ss_male <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_ss_total_gender.num_ss_total, num_ss_total_gender.num_ss_male), num_ss_total_gender.num_ss_male < )

#####num_ss_total_gender.num_ss_total#####  2500... same tch/class           
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_ss_total_gender.num_ss_total, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of SS Total') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) 
    #cleaning
    #e_num_ss_total_gender.num_ss_total <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_ss_total_gender.num_ss_total), num_ss_total_gender.num_ss_total < )

#####km_to_catchment_area#####    25                        
    #by zone
    ggplot(e, aes(x=mylga_zone, y=km_to_catchment_area, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of KM to Catchment Area') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) + scale_y_continuous(limits=c(0,200))
    #cleaning
    #e_km_to_catchment_area <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, km_to_catchment_area), km_to_catchment_area < )

#####km_to_secondary_school#####  same
    #by zone
    ggplot(e, aes(x=mylga_zone, y=km_to_secondary_school, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of KM to Secondary School') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) + scale_y_continuous(limits=c(0,200))
    #cleaning
    #e_km_to_secondary_school <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, km_to_secondary_school), km_to_secondary_school < )

#####num_students_frthr_than_3km#####   1250                    
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_students_frthr_than_3km), fill=mylga_zone) + coord_flip() + 
      geom_boxplot() + ylab('Number of Students Farther Than 3km') + xlab('Zone') + scale_fill_manual(values=cbPalette)
    #log 10 
    ggplot(e, aes(x=mylga_zone, y=log10(num_students_frthr_than_3km)), fill=mylga_zone) + coord_flip() + 
      geom_boxplot() + ylab('Number of Students Farther Than 3km') + xlab('Zone') + scale_fill_manual(values=cbPalette)
    #cleaning
    #e_num_students_frthr_than_3km <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_students_frthr_than_3km), num_students_frthr_than_3km < )

#####days_no_potable_water#####   
    #by zone
    ggplot(e, aes(x=mylga_zone, y=days_no_potable_water, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Days with No Potable Water') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) 
    #cleaning
    #e_days_no_potable_water <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, days_no_potable_water), days_no_potable_water < )

#####num_toilet.num_toilet_boy#####                  
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_toilet.num_toilet_boy, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Toilets for Boys') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) 
    #cleaning
    e_rationum_toilets
    #e_num_toilet.num_toilet_boy <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_students_total_gender.num_students_male, num_toilet.num_toilet_boy), num_students_total_gender.num_students_male/num_toilet.num_toilet_boy < 30 )

#####num_toilet.num_toilet_girl#####   750               
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_toilet.num_toilet_girl, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Toilets for Girls') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) 
    #cleaning
    #e_num_toilet.num_toilet_girl <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_toilet.num_toilet_girl), num_toilet.num_toilet_girl < )

#####num_toilet.num_toilet_both#####    1000           
  #by zone
  ggplot(e, aes(x=mylga_zone, y=num_toilet.num_toilet_both, fill=mylga_zone)) +
    coord_flip() + geom_boxplot() + ylab('Number of Toilets Co-ed') + xlab('Zone') + 
    scale_fill_manual(values=cbPalette)
    #cleaning
    #e_num_toilet.num_toilet_both <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_toilet.num_toilet_both), num_toilet.num_toilet_both < )

#####num_toilet.num_toilet_total#####    1000      
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_toilet.num_toilet_total, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Toilets Total') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette)
    #cleaning
    #e_num_toilet.num_toilet_total <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_toilet.num_toilet_total), num_tchrs.num_tchrs_male < )

#####num_tchrs.num_tchrs_male#####    100                    
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_tchrs.num_tchrs_male, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Teachers Male') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_num_tchrs.num_tchrs_male <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_tchrs.num_tchrs_total, num_tchrs.num_tchrs_male), num_tchrs.num_tchrs_male > 100 )

#####num_tchrs.num_tchrs_female#####  100                
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_tchrs.num_tchrs_female, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Teachers Female') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette)
    #cleaning
    #e_num_tchrs.num_tchrs_female <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_tchrs.num_tchrs_total, num_tchrs.num_tchrs_female), num_tchrs.num_tchrs_female < )

#####num_tchrs.num_tchrs_total#####                      
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_tchrs.num_tchrs_total, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Teachers Total') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette)
    #cleaning
    #e_num_tchrs.num_tchrs_total <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_tchrs.num_tchrs_total), num_tchrs.num_tchrs_total < )

#####num_tchrs_qualification.num_tchrs_w_nce#####   100   
  #by zone
  ggplot(e, aes(x=mylga_zone, y=num_tchrs_qualification.num_tchrs_w_nce, fill=mylga_zone)) +
    coord_flip() + geom_boxplot() + ylab('Number of Teachers with \nNCE Qualification') + xlab('Zone') + 
    scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_num_tchrs_qualification.num_tchrs_w_nce <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_tchrs.num_tchrs_total, num_tchrs_qualification.num_tchrs_w_nce), num_tchrs_qualification.num_tchrs_w_nce < )

#####num_tchrs_qualification.num_tchrs_w_nce_plus##### 100
  #by zone
  ggplot(e, aes(x=mylga_zone, y=num_tchrs_qualification.num_tchrs_w_nce_plus, fill=mylga_zone)) +
    coord_flip() + geom_boxplot() + ylab('Number of Teachers with \nNCE Qualification Plus') + xlab('Zone') + 
    scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_num_tchrs_qualification.num_tchrs_w_nce_plus <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_tchrs.num_tchrs_total, num_tchrs_qualification.num_tchrs_w_nce_plus), num_tchrs_qualification.num_tchrs_w_nce_plus < )

#####num_sr_staff_total#####   75                        
  #by zone
    ggplot(e, aes(x=mylga_zone, y=num_sr_staff_total, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Senior Staff Total') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone") + scale_y_continuous(limits=c(0,500))
    #cleaning
    #e_num_sr_staff_total <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_tchrs.num_tchrs_total, num_sr_staff_total), num_sr_staff_total < )

#####num_jr_staff_total#####    50                       
  #by zone
  ggplot(e, aes(x=mylga_zone, y=num_jr_staff_total, fill=mylga_zone)) +
    coord_flip() + geom_boxplot() + ylab('Number of Junior Staff Total') + xlab('Zone') + 
    scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_num_jr_staff_total <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_tchrs.num_tchrs_total, num_jr_staff_total), num_jr_staff_total < )

#####num_tchrs_attended_training#####     100             
  #by zone
  ggplot(e, aes(x=mylga_zone, y=num_tchrs_attended_training, fill=mylga_zone)) +
    coord_flip() + geom_boxplot() + ylab('Number of Teachers \nthat Attended Training') + xlab('Zone') + 
    scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_num_tchrs_attended_training <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_tchrs.num_tchrs_total, num_tchrs_attended_training), num_tchrs_attended_training < )

#####num_tchrs_trained_new_curricula#####               
  #by zone
  ggplot(e, aes(x=mylga_zone, y=num_tchrs_trained_new_curricula, fill=mylga_zone)) +
    coord_flip() + geom_boxplot() + ylab('Number of Teachers Trained \nin New Curricula Training') + xlab('Zone') + 
    scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_num_tchrs_trained_new_curricula <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_tchrs.num_tchrs_total, num_tchrs_trained_new_curricula), num_tchrs_trained_new_curricula < )

#####times_tchr_pay_delay_pastyr##### 
  #by zone
  ggplot(e, aes(x=mylga_zone, y=times_tchr_pay_delay_pastyr, fill=mylga_zone)) +
    coord_flip() + geom_boxplot() + ylab('Times Teacher Payed Delay') + xlab('Zone') + 
    scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_times_tchr_pay_delay_pastyr <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_tchrs.num_tchrs_total, times_tchr_pay_delay_pastyr), times_tchr_pay_delay_pastyr < )

#####times_tchr_pay_miss_pastyr#####
  #by zone
  ggplot(e, aes(x=mylga_zone, y=times_tchr_pay_miss_pastyr, fill=mylga_zone)) +
    coord_flip() + geom_boxplot() + ylab('Times Teacher Pay Missed') + xlab('Zone') + 
    scale_fill_manual(values=cbPalette, name="Zone")
  #cleaning
  #e_times_tchr_pay_miss_pastyr <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_tchrs.num_tchrs_total, times_tchr_pay_miss_pastyr), times_tchr_pay_miss_pastyr < )

#####num_classrms_total#####   100
  #by zone
  ggplot(e, aes(x=mylga_zone, y=num_classrms_total, fill=mylga_zone)) +
    coord_flip() + geom_boxplot() + ylab('Number of Classrooms Total') + xlab('Zone') + 
    scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_num_classrms_total <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_classrms_total), num_classrms_total > 65 )
   
#####num_classrms_good_cond#####       100                  
  #by zone
  ggplot(e, aes(x=mylga_zone, y=num_classrms_total, fill=mylga_zone)) +
    coord_flip() + geom_boxplot() + ylab('Number of Classrooms Good Condition') + xlab('Zone') + 
    scale_fill_manual(values=cbPalette, name="Zone")
  #cleaning
  #e_num_classrms_good_cond <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_classrms_total, num_classrms_good_cond), num_classrms_good_cond < )

#####num_classrms_need_min_repairs#####     50            
  #by zone
  ggplot(e, aes(x=mylga_zone, y=num_classrms_need_min_repairs, fill=mylga_zone)) +
    coord_flip() + geom_boxplot() + ylab('Number of Classrooms that Need Minor Repairs') + xlab('Zone') + 
    scale_fill_manual(values=cbPalette, name="Zone")
  #log10
  ggplot(e, aes(x=mylga_zone, y=log10(num_classrms_need_min_repairs)), fill=mylga_zone) + 
    coord_flip() + geom_boxplot() + ylab('Number of Classrooms that Need Minor Repairs') + 
    xlab('Zone') + scale_fill_manual(values=cbPalette)
  #cleaning
  #e_num_classrms_need_min_repairs <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_classrms_total, num_classrms_need_min_repairs), num_classrms_need_min_repairs < )

#####num_classrms_need_maj_repairs#####   50             
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_classrms_need_maj_repairs, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Classrooms that Need Major Repairs') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone") + scale_y_continuous(limits=c(0,100))
    #log10
    ggplot(e, aes(x=mylga_zone, y=log10(num_classrms_need_maj_repairs)), fill=mylga_zone) + 
      coord_flip() + geom_boxplot() + ylab('Number of Classrooms that Need Major Repairs') + 
      xlab('Zone') + scale_fill_manual(values=cbPalette)
  #cleaning
  #e_num_classrms_need_maj_repairs <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_classrms_total, num_classrms_need_min_repairs), e$num_classrms_need_maj_repairs < )

#####num_desks#####   student_to_desk_ratio < 1    .....  student to desk ratio > 250                            
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_desks, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Desks') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone")
    ggplot(e, aes(x=mylga_zone, y=student_to_desk_ratio, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Student to Desk Ratio') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone")
  #cleaning
      e$student_to_desk_ratio <- e$num_students_total_gender.num_students_total/e$num_desks
  #e_num_desks <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_classrms_total, 
          num_desks, student_to_desk_ratio), subset=(( 
        student_to_desk_ratio > 20) & 
            student_to_desk_ratio != "Inf" ))

#####num_benches#####     2500  
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_benches, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Benches') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_num_benches <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_classrms_total, num_benches), e$num_benches < )

#####num_classrm_w_chalkboard#####    logic check + 50                 
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_classrm_w_chalkboard, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Classrooms with Chalkboards') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_num_classrm_w_chalkboard <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_classrms_total, num_classrm_w_chalkboard), e$num_classrm_w_chalkboard < )

#####manuals_pry.num_math_textbook_pry#####     3000        
    #by zone
    ggplot(e, aes(x=mylga_zone, y=manuals_pry.num_math_textbook_pry, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Math \nTextbooks Primary Schools') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone") + scale_y_continuous(limits=c(0,5000))
    #cleaning
    #e_manuals_pry.num_math_textbook_pry <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, manuals_pry.num_math_textbook_pry), e$num_classrm_w_chalkboard < )

#####manuals_pry.num_english_textbook_pry#####  3000       
    #by zone
    ggplot(e, aes(x=mylga_zone, y=manuals_pry.num_english_textbook_pry, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of English \nTextbooks Primary Schools') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_manuals_pry.num_english_textbook_pry <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, manuals_pry.num_english_textbook_pry), e$manuals_pry.num_english_textbook_pry < )

#####manuals_pry.num_soc_science_textbook_pry#####  3000     
    #by zone
    ggplot(e, aes(x=mylga_zone, y=manuals_pry.num_soc_science_textbook_pry, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Social Science \nTextbooks Primary Schools') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_manuals_pry.num_soc_science_textbook_pry <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, manuals_pry.num_soc_science_textbook_pry), e$manuals_pry.num_soc_science_textbook_pry < )

#####manuals_pry.num_science_textbook_pry#####     3000    
    #by zone
    ggplot(e, aes(x=mylga_zone, y=manuals_pry.num_science_textbook_pry, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Science \nTextbooks Primary Schools') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_manuals_pry.num_science_textbook_pry <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, manuals_pry.num_science_textbook_pry), e$manuals_pry.num_science_textbook_pry < )

#####manuals_js.num_math_textbook_js#####   3000
    #by zone
    ggplot(e, aes(x=mylga_zone, y=manuals_js.num_math_textbook_js, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Math \nTextbooks Junior Secondary Schools') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_manuals_js.num_math_textbook_js <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, manuals_js.num_math_textbook_js), e$manuals_js.num_math_textbook_js < )

#####manuals_js.num_english_textbook_js#####   3000        
    #by zone
    ggplot(e, aes(x=mylga_zone, y=manuals_js.num_english_textbook_js, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of English \nTextbooks Junior Secondary Schools') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_manuals_js.num_english_textbook_js <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, manuals_js.num_english_textbook_js), e$manuals_js.num_english_textbook_js < )

#####manuals_js.num_soc_science_textbook_js#####  3000      
    #by zone
    ggplot(e, aes(x=mylga_zone, y=manuals_js.num_soc_science_textbook_js, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Social Science \nTextbooks Junior Secondary Schools') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_manuals_js.num_soc_science_textbook_js <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, manuals_js.num_soc_science_textbook_js), e$manuals_js.num_soc_science_textbook_js < )

#####manuals_js.num_science_textbook_js#####    3000      
    #by zone
    ggplot(e, aes(x=mylga_zone, y=manuals_js.num_science_textbook_js, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Science \nTextbooks Junior Secondary Schools') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_manuals_js.num_science_textbook_js <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, manuals_js.num_science_textbook_js), e$manuals_js.num_science_textbook_js < )

#####num_exercise_books_per_student_pry#####  paramaters below...          
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_exercise_books_per_student_pry, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Exercise \nBooks per Student Primary Schools') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_num_exercise_books_per_student_pry <- subset(e, select=c(uuid, mylga, 
      #mylga_state, mylga_zone, num_students_total_gender.num_students_total, 
        #num_exercise_books_per_student_pry), 
          #e$num_students_total_gender.num_students_total * 8 < e$num_exercise_books_per_student_pry )

#####num_exercise_books_per_student_jss#####    same as above       
    #by zone
    ggplot(e, aes(x=mylga_zone, y=num_exercise_books_per_student_jss, fill=mylga_zone)) +
      coord_flip() + geom_boxplot() + ylab('Number of Exercise \nBooks per Student Junior Secondary Schools') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette, name="Zone")
    #cleaning
    #e_num_exercise_books_per_student_jss <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, num_students_total_gender.num_students_total, num_exercise_books_per_student_jss), e$num_exercise_books_per_student_jss < )

################################
#### categorical data ####
################################

ggplot(e, 
       aes(x=school_constr_fed_gov.loc_gov, 
           y=(..count..)/sum(..count..)*100)) +  # that is percent right there
             geom_bar() + 
             ylab('Percent (%)') + coord_flip()


