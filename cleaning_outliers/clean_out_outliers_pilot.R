##PILOT Outlier Cleaning Script
source("../InstallFormhub.R")
source('./outlier_functions.R')

###############################################################################################
######education################################################################################
###############################################################################################                    
e <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Education_pilot_999Cleaned.csv", header=TRUE, stringsAsFactors=F)


e$num_classrms_total <- e$num_total_classrooms
e$num_students_total_gender.num_students_female <- e$num_students_female
e$num_students_total_gender.num_students_male <- e$num_students_male


e$num_students_total_gender.num_students_total <-  replace(e$num_students_total_gender.num_students_female, is.na(e$num_students_total_gender.num_students_female), 0) +
    replace(e$num_students_total_gender.num_students_male, is.na(e$num_students_total_gender.num_students_male), 0)

e$num_tchrs.num_tchrs_total <- e$num_tchrs_total 

e$num_tchrs.num_tchrs_male <- replace(e$num_tchrs_male_full_time, is.na(e$num_tchrs_male_full_time), 0) #+


e$num_tchrs.num_tchrs_female <- replace(e$num_tchrs_female_full_time, is.na(e$num_tchrs_female_full_time), 0) #+


e <- outlierreplace(e, 'num_tchrs.num_tchrs_male', 
                    (e$num_tchrs.num_tchrs_male > e$num_tchrs.num_tchrs_total))

e <- outlierreplace(e, 'num_tchrs.num_tchrs_female', 
                    (e$num_tchrs.num_tchrs_female > e$num_tchrs.num_tchrs_total))

e <- outlierreplace(e, 'num_tchrs.num_tchrs_total',
                    (e$num_tchrs.num_tchrs_total > 20 & 
                         e$num_students_total_gender.num_students_total == 0))

e <- outlierreplace(e, 'num_tchrs.num_tchrs_total',
                    (e$num_tchrs.num_tchrs_total > e$num_tchrs.num_tchrs_male + 
                         e$num_tchrs.num_tchrs_female))

e <- outlierreplace(e,'num_classrms_need_maj_repairs',
                    (e$num_classrms_need_maj_repairs > e$num_classrms_total)) #num of class total ????????

e <- outlierreplace(e,'num_classrms_need_min_repairs',
                    (e$num_classrms_need_min_repairs > e$num_classrms_total))

e <- outlierreplace(e,'num_classrms_good_cond',
                    (e$num_classrms_good_cond > e$num_classrms_total))

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
#############
##ratios#####
#############

e$num_desks <- e$X_p_num_total_desk
e$num_benches <- e$X_p_num_benches_chairs

e$ratio_students_to_desks <-   replace(e$num_students_total_gender.num_students_total, 
                                       is.na(e$num_students_total_gender.num_students_total), 0) / 
    replace(e$num_desks, is.na(e$num_desks), 0)    

e$ratio_students_to_benches <-   replace(e$num_students_total_gender.num_students_total, 
                                         is.na(e$num_students_total_gender.num_students_total), 0) /
    replace(e$num_benches, is.na(e$num_benches), 0) 


###############
##textbooks####
###############

######################
#new data points######
######################
e <- outlierreplace(e, 'num_students_total_gender.num_students_female',
                    (e$num_students_total_gender.num_students_female > 11000))
e <- outlierreplace(e, 'num_students_total_gender.num_students_female',
                    (e$num_students_total_gender.num_students_female > 3000))
e <- outlierreplace(e, 'num_students_total_gender.num_students_male',
                    (e$num_students_total_gender.num_students_male > 2500  & e$num_classrms_total < 25))
e <- outlierreplace(e, 'num_students_total_gender.num_students_total',
                    (e$num_students_total_gender.num_students_total > 2000 & e$num_classrms_total < 25 &
                         e$num_tchrs.num_tchrs_total < 10))
e <- outlierreplace(e, 'km_to_secondary_school',
                    (e$km_to_secondary_school > 25))
e <- outlierreplace(e, 'num_tchrs.num_tchrs_male',
                    (e$num_tchrs.num_tchrs_male > 100))
e <- outlierreplace(e, 'num_tchrs.num_tchrs_female',
                    (e$num_tchrs.num_tchrs_female > 100))
e <- outlierreplace(e, 'num_classrms_need_min_repairs',
                    (e$num_classrms_need_min_repairs > 50))
e <- outlierreplace(e, 'num_classrms_need_maj_repairs',
                    (e$num_classrms_need_maj_repairs > 50))

#####################
#absolute example####
#####################

e <- outlierreplace(e, 'num_classrms_good_cond',
                    (e$num_classrms_good_cond > 260))                    
e <- outlierreplace(e, 'num_desks',
                    (e$num_desks > 2500))
e <- outlierreplace(e, 'num_benches',
                    (e$num_benches > 2500))
e <- outlierreplace(e, 'ratio_students_to_desks',
                    (e$ratio_students_to_desks > 1000)) 
e <- outlierreplace(e, 'ratio_students_to_benches',
                    (e$ratio_students_to_benches > 1000)) 

e$ratio_students_to_benches <-   replace(e$num_students_total_gender.num_students_total, 
                                         is.na(e$num_students_total_gender.num_students_total), 0) / replace(e$num_benches, is.na(e$num_benches), 0) 

e$num_students_total_gender.num_students_total <- replace(e$num_students_total_gender.num_students_female, 
                                                          is.na(e$num_students_total_gender.num_students_female), 0) +
    replace(e$num_students_total_gender.num_students_male, 
            is.na(e$num_students_total_gender.num_students_male), 0)

e$num_students_total_gender.num_students_total <- ifelse(e$num_students_total_gender.num_students_total == 0, 
                                                         NA, e$num_students_total_gender.num_students_total)

e$num_tchrs.num_tchrs_total <- replace(e$num_tchrs.num_tchrs_male, is.na(e$num_tchrs.num_tchrs_male), 0) + 
    replace(e$num_tchrs.num_tchrs_female, is.na(e$num_tchrs.num_tchrs_female), 0) 

e <- outlierreplace(e, 'ratio_students_to_desks',
                    (e$ratio_students_to_desks > 1000)) 
e <- outlierreplace(e, 'ratio_students_to_benches',
                    (e$ratio_students_to_benches > 1000)) 

write.csv(e, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Education_pilot_outliercleaned.csv", row.names=FALSE)





###############################################################################################
######health###################################################################################
###############################################################################################                    
##reading in data
h_p <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Health_pilot_999Cleaned.csv", stringsAsFactors=F)
hp <- h_p

#cleaning
hp <- outlierreplace(hp, 'num_doctors_fulltime',
                     hp$num_doctors_fulltime > 12) 

hp <- outlierreplace(hp, 'num_nurses_fulltime',
                     hp$num_nurses_fulltime > 16) 

hp <- outlierreplace(hp, 'num_nursemidwives_fulltime',
                     hp$num_nursemidwives_fulltime > 50) 

hp <- outlierreplace(hp, 'num_jr_chews_fulltime',
                     hp$num_jr_chews_fulltime > 50)                     
#new data points
hp <- outlierreplace(hp, 'num_midwives_fulltime',
                     hp$num_midwives_fulltime > 16)                     

hp <- outlierreplace(hp, 'num_nurses_fulltime',
                     hp$num_nurses_fulltime > 16)                     

hp <- outlierreplace(hp, 'num_nursemidwives_fulltime',
                     hp$num_nursemidwives_fulltime > 16)

hp <- outlierreplace(hp, 'num_chos_fulltime',
                     hp$num_chos_fulltime > 16)

hp <- outlierreplace(hp, 'num_chews_fulltime',
                     hp$num_chews_fulltime > 50)

hp <- outlierreplace(hp, 'num_pharm_techs_fulltime',
                     hp$num_pharm_techs_fulltime > 5)

hp <- outlierreplace(hp, 'num_lab_techs_fulltime',
                     hp$num_lab_techs_fulltime > 4)

hp <- outlierreplace(hp, 'inpatient_care_num_beds',
                     hp$inpatient_care_num_beds > 50)

write.csv(hp, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Health_pilot_outliercleaned.csv", row.names=F)
rm(hp)

###############################################################################################
######water####################################################################################
############################################################################################### 

file.copy("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Water_pilot_999Cleaned.csv",
          "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Water_pilot_outliercleaned.csv", overwrite=T)





