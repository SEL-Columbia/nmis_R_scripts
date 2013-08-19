source("base_scripts/InstallFormhub.R")
source('cleaning_outliers/outlier_functions.R')

###############################################################################################
######education################################################################################
###############################################################################################

e <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Education_113_999Cleaned.csv", header=TRUE, stringsAsFactors=F)


e$num_classrms_total <- rowSums(cbind(e$num_classrms_good_cond,
                                      e$num_classrms_need_min_repairs,
                                      e$num_classrms_need_maj_repairs,
                                      e$num_classrms_good_cond), na.rm=T)

e$num_students_total_gender.num_students_female <- e$num_students_female
e$num_students_total_gender.num_students_male <- e$num_students_male

e$num_students_total_gender.num_students_total <-  replace(e$num_students_total_gender.num_students_female, is.na(e$num_students_total_gender.num_students_female), 0) +
    replace(e$num_students_total_gender.num_students_male, is.na(e$num_students_total_gender.num_students_male), 0)

e$num_tchrs.num_tchrs_total <- e$num_tchrs_total 

e$num_tchrs.num_tchrs_male <- replace(e$num_tchrs_male_full_time, is.na(e$num_tchrs_male_full_time), 0) +
    replace(e$num_tchrs_male_part_time, is.na(e$num_tchrs_male_part_time), 0)

e$num_tchrs.num_tchrs_female <- replace(e$num_tchrs_female_full_time, is.na(e$num_tchrs_female_full_time), 0) +
    replace(e$num_tchrs_female_part_time, is.na(e$num_tchrs_female_part_time), 0)

e$num_tchrs_qualification.num_tchrs_w_nce <- replace(e$tchrs_male_nce, is.na(e$tchrs_male_nce), 0) +
    replace(e$tchrs_female_nce, is.na(e$tchrs_female_nce), 0) +
    replace(e$tchrs_male_other_w_nce, is.na(e$tchrs_male_other_w_nce), 0) +
    replace(e$tchrs_female_other_w_nce, is.na(e$tchrs_female_other_w_nce), 0)

e <- outlierreplace(e, 'num_tchrs.num_tchrs_male', 
                    (e$num_tchrs.num_tchrs_male > e$num_tchrs.num_tchrs_total))

e <- outlierreplace(e, 'num_tchrs.num_tchrs_female', 
                    (e$num_tchrs.num_tchrs_female > e$num_tchrs.num_tchrs_total))

e <- outlierreplace(e, 'num_tchrs_qualification.num_tchrs_w_nce',
                    which(e$num_tchrs_qualification.num_tchrs_w_nce > e$num_tchrs.num_tchrs_total))

e <- outlierreplace(e, 'num_tchrs_attended_training',
                    which(e$num_tchrs_attended_training > e$num_tchrs.num_tchrs_total))

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
#############
##ratios#####
############# 
e$num_benches <- rowSums(cbind(e$num_attached_benches, e$num_unattached_benches), na.rm=T)
e$ratio_students_to_benches <-   replace(e$num_students_total_gender.num_students_total, 
                                         is.na(e$num_students_total_gender.num_students_total), 0) /
    replace(e$num_benches, is.na(e$num_benches), 0) 

e <- outlierreplace(e, 'num_students_total_gender.num_students_female',
                    (e$num_students_total_gender.num_students_female > 11000))
e <- outlierreplace(e, 'num_students_total_gender.num_students_female',
                    (e$num_students_total_gender.num_students_female > 3000))
e <- outlierreplace(e, 'num_students_total_gender.num_students_male',
                    (e$num_students_total_gender.num_students_male > 2500  & e$num_classrms_total < 25))
e <- outlierreplace(e, 'num_students_total_gender.num_students_total',
                    (e$num_students_total_gender.num_students_total > 2000 & e$num_classrms_total < 25 &
                         e$num_tchrs.num_tchrs_total < 10))
e <- outlierreplace(e, 'km_to_catchment_area',
                    (e$km_to_catchment_area > 55))                    
e <- outlierreplace(e, 'km_to_secondary_school',
                    (e$km_to_secondary_school > 25))
e <- outlierreplace(e, 'num_students_frthr_than_3km',
                    (e$num_students_frthr_than_3km > 1250))            
e <- outlierreplace(e, 'num_tchrs.num_tchrs_male',
                    (e$num_tchrs.num_tchrs_male > 100))
e <- outlierreplace(e, 'num_tchrs.num_tchrs_female',
                    (e$num_tchrs.num_tchrs_female > 100))
e <- outlierreplace(e, 'num_tchrs_qualification.num_tchrs_w_nce',
                    (e$num_tchrs_qualification.num_tchrs_w_nce > 100))
e <- outlierreplace(e, 'num_sr_staff_total',
                    (e$num_sr_staff_total > 75))   #####    NEGATIVE NUMBER
e <- outlierreplace(e, 'num_jr_staff_total',
                    (e$num_jr_staff_total > 50))
e <- outlierreplace(e, 'num_tchrs_attended_training',
                    (e$num_tchrs_attended_training > 100))
e <- outlierreplace(e, 'num_classrms_need_min_repairs',
                    (e$num_classrms_need_min_repairs > 50))
e <- outlierreplace(e, 'num_classrms_need_maj_repairs',
                    (e$num_classrms_need_maj_repairs > 50))

#####################
#absolute example####
#####################

e <- outlierreplace(e, 'num_classrms_good_cond',
                    (e$num_classrms_good_cond > 260))                    
e <- outlierreplace(e, 'num_benches',
                    (e$num_benches > 2500))
e <- outlierreplace(e, 'ratio_students_to_benches',
                    (e$ratio_students_to_benches > 1000)) 

e$ratio_students_to_benches <-   replace(e$num_students_total_gender.num_students_total, 
                                         is.na(e$num_students_total_gender.num_students_total), 0) / replace(e$num_benches, is.na(e$num_benches), 0) 
e$num_students_total_gender.num_students_total <-  replace(e$num_students_total_gender.num_students_female, 
                                                           is.na(e$num_students_total_gender.num_students_female), 0) +
    replace(e$num_students_total_gender.num_students_male, 
            is.na(e$num_students_total_gender.num_students_male), 0)

e$num_students_total_gender.num_students_total <- 
    ifelse(e$num_students_total_gender.num_students_total == 0, NA, e$num_students_total_gender.num_students_total)

e$num_tchrs.num_tchrs_total <- replace(e$num_tchrs.num_tchrs_male, is.na(e$num_tchrs.num_tchrs_male), 0) + 
    replace(e$num_tchrs.num_tchrs_female, is.na(e$num_tchrs.num_tchrs_female), 0) 

e <- outlierreplace(e, 'ratio_students_to_benches',
                    (e$ratio_students_to_benches > 1000)) 

write.csv(e, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Education_113_outliercleaned.csv", row.names=FALSE)

rm(e)                   


###############################################################################################
######health###################################################################################
###############################################################################################                    

##reading in data
hh <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Health_113_999Cleaned.csv", stringsAsFactors=F)
h <- hh

#cleaning
h <- outlierreplace(h, 'num_doctors_fulltime', 
                    h$num_doctors_fulltime > 12 & 
                      (h$facility_type != "teachinghospital" &
                         h$facility_type != "federalmedicalcare"))

h <- outlierreplace(h, 'num_doctors_fulltime', 
                    h$num_doctors_fulltime > 20 & 
                      (h$facility_type == "teachinghospital" &
                         h$facility_type == "federalmedicalcare"))

h <- outlierreplace(h, 'num_doctors_fulltime', 
                    h$num_doctors_fulltime > 12 & 
                      (h$facility_type != "teachinghospital" &
                         h$facility_type != "federalmedicalcare"))

h <- outlierreplace(h, 'num_nurses_fulltime',
                    h$num_nurses_fulltime > 16 & 
                        (h$facility_type != "teachinghospital" &
                          h$facility_type != "federalmedicalcare"))

h <- outlierreplace(h, 'num_nurses_fulltime',
                    h$num_nurses_fulltime > 24 & 
                      (h$facility_type == "teachinghospital" &
                         h$facility_type == "federalmedicalcare"))

h <- outlierreplace(h, 'num_midwives_fulltime',
                    h$num_midwives_fulltime > 24 & 
                      (h$facility_type == "teachinghospital" &
                         h$facility_type == "federalmedicalcare"))

h <- outlierreplace(h, 'num_nursemidwives_fulltime',
                    h$num_nursemidwives_fulltime > 24 & 
                      (h$facility_type != "teachinghospital" &
                         h$facility_type != "federalmedicalcare"))

h <- outlierreplace(h, 'num_nursemidwives_fulltime',
                    h$num_nursemidwives_fulltime > 24 & 
                      (h$facility_type == "teachinghospital" &
                         h$facility_type == "federalmedicalcare"))

h <- outlierreplace(h, 'num_nursemidwives_fulltime',
                    h$num_nursemidwives_fulltime > 50) 

h <- outlierreplace(h, 'num_jr_chews_fulltime',
                    h$num_jr_chews_fulltime > 50) 
#new paramaters
h <- outlierreplace(h, 'facility_type',
                    (((h$num_doctors_fulltime < 30 &
                       h$num_doctors_fulltime != 0) &
                    (h$num_midwives_fulltime < 30 &
                       h$num_midwives_fulltime != 0) &
                    (h$num_nurses_fulltime < 30 &
                       h$num_nurses_fulltime != 0)) &
                    (h$facility_type == "teachinghospital" | 
                       h$facility_type == "federalmedicalcentre")))

h <- outlierreplace(h, 'num_doctors_fulltime',
                    ((h$num_doctors_fulltime > 500 | 
                      h$num_doctors_fulltime < 100 &                
                      (h$facility_type == "teachinghospital" | 
                          h$facility_type == "federalmedicalcentre"))))

h <- outlierreplace(h, 'num_nurses_fulltime',
                    (h$num_nurses_fulltime < 100 &
                      (h$facility_type == "teachinghospital" | 
                         h$facility_type == "federalmedicalcentre")))

h <- outlierreplace(h, 'num_midwives_fulltime',
                    (h$num_midwives_fulltime < 100 &
                       (h$facility_type == "teachinghospital" | 
                          h$facility_type == "federalmedicalcentre")))
                                    
h <- outlierreplace(h, 'num_nursemidwives_fulltime',
                    (h$num_nursemidwives_fulltime < 50 &
                       (h$facility_type == "teachinghospital" | 
                          h$facility_type == "federalmedicalcentre")))
                    
h <- outlierreplace(h, 'num_midwives_fulltime',
                    (h$num_midwives_fulltime > 16 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))

h <- outlierreplace(h, 'num_nurses_fulltime',
                    (h$num_nurses_fulltime > 16 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))
                    
h <- outlierreplace(h, 'num_nursemidwives_fulltime',
                    (h$num_nursemidwives_fulltime > 16 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))

h <- outlierreplace(h, 'num_chos_fulltime',
                    (h$num_chos_fulltime > 16 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))
                    
h <- outlierreplace(h, 'num_chos_fulltime',
                    (h$num_chos_fulltime > 24 & 
                       (h$facility_type == "teachinghospital" & 
                          h$facility_type == "federalmedicalcentre")))                   

h <- outlierreplace(h, 'num_chews_fulltime',
                    (h$num_chews_fulltime > 50))                    

h <- outlierreplace(h, 'num_jr_chews_fulltime',
                    (h$num_jr_chews_fulltime > 50 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))                              
                    
h <- outlierreplace(h, 'num_jr_chews_fulltime',
                    (h$num_jr_chews_fulltime > 24 & 
                       (h$facility_type == "teachinghospital" & 
                          h$facility_type == "federalmedicalcentre")))                              
                    
h <- outlierreplace(h, 'num_pharm_techs_fulltime',
                    (h$num_pharm_techs_fulltime > 5 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))                              
                    
h <- outlierreplace(h, 'num_pharm_techs_fulltime',
                    (h$num_pharm_techs_fulltime < 5 & 
                       (h$facility_type == "teachinghospital" & 
                          h$facility_type == "federalmedicalcentre")))                              
                    
h <- outlierreplace(h, 'num_lab_techs_fulltime',
                    (h$num_lab_techs_fulltime > 4 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))                              

h <- outlierreplace(h, 'num_lab_techs_fulltime',
                    (h$num_lab_techs_fulltime > 5 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))                              

h <- outlierreplace(h, 'num_lab_techs_fulltime',
                    (h$num_lab_techs_fulltime > 5 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))                              
 
h <- outlierreplace(h, 'num_pharm_techs_fulltime',
                    (h$num_pharm_techs_fulltime > 5 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))                              
                    
h <- outlierreplace(h, 'num_med_rcrds_officers_fulltime',
                    (h$num_med_rcrds_officers_fulltime > 4 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))                              
                    
h <- outlierreplace(h, 'inpatient_care_num_beds',
                    (h$inpatient_care_num_beds > 50 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))                              
        
h <- outlierreplace(h, 'inpatient_care_num_beds',
                    (h$inpatient_care_num_beds > 50 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))                              

h <- outlierreplace(h, 'vip_latrine_number',
                    (h$vip_latrine_number > 6 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))                              

h <- outlierreplace(h, 'slab_pit_latrine_number',
                    (h$slab_pit_latrine_number > 8 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))                              
                    
h <- outlierreplace(h, 'open_pit_latrine_number',
                    (h$open_pit_latrine_number > 4 & 
                       (h$facility_type != "teachinghospital" & 
                          h$facility_type != "federalmedicalcentre")))                              

h <- outlierreplace(h, 'bucket_system_number',
                    h$bucket_system_number > 10 )
                    
write.csv(h, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Health_113_outliercleaned.csv", row.names=FALSE)
rm(h)

                    
###############################################################################################
######water####################################################################################
############################################################################################### 

file.copy("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Water_113_999Cleaned.csv",
     "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Water_113_outliercleaned.csv", overwrite=T)


                    
                    
                    
