slug_search <- function(slug, df=e)
{
    names(df)[grep(pattern=slug, x=names(df), ignore.case=T)]
}

setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
source("scripts/InstallFormhub.R")

outlierOutputList = list()

outlierreplace = function(df, c, rowpredicate, replaceVal=NA) 
{
    naCount1 <- length(which(is.na(df[,c])))
    df[,c] <- replace(df[,c], rowpredicate, replaceVal)
    naCount2 <- length(which(is.na(df[,c])))
    #print(str_c(naCount2-naCount1, " outliers replaced for field: ", c))
    outlierOutputList <<- c(outlierOutputList, 
                            str_c(naCount2-naCount1, " outliers replaced for field: ", c))
    df
}


##########
#Education
##########
e <- read.csv("in_process_data/999cleaned/Education_113_999Cleaned.csv", header=TRUE)


e$num_classrms_total <- rowSums(cbind(e$num_classrms_good_cond,
                                      e$num_classrms_need_min_repairs,
                                      e$num_classrms_need_maj_repairs,
                                      e$num_classrms_good_cond), na.rm=T)

e$num_students_total_gender.num_students_female <- e$num_students_female
e$num_students_total_gender.num_students_male <- e$num_students_male

e$num_students_total_gender.num_students_total <-  replace(e$num_students_total_gender.num_students_female, is.na(e$num_students_total_gender.num_students_female), 0) +
                                                    replace(e$num_students_total_gender.num_students_male, is.na(e$num_students_total_gender.num_students_male), 0)

e$num_tchrs.num_tchrs_total <- e$num_tchrs_total 
 

e[which(e$total > e$num_students_total_gender.num_students_total),"num_students_total_gender.num_students_total"] <- 
    e[which(e$total > e$num_students_total_gender.num_students_total),"total"]

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
e <- outlierreplace(e, 'num_ss_total_gender.num_ss_total',
                    (e$km_to_catchment_area > 30))
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

write.csv(e, "in_process_data/outlier_cleaned/Education_113_outliercleaned.csv", row.names=FALSE)
# rm(e)                   