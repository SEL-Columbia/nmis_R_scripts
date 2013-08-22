#Education pilot: facility level
source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")

########
########
###PILOT
#reading in data
e_p <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Education_pilot_outliercleaned.csv",
                stringsAsFactors=F)

#strip common variables
e_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Education_661_NMIS_Facility.csv")
subnm <- names(e_661)[which(names(e_661) %in% names(e_p))]

nm_p <- names(e_p)[! names(e_p) %in% subnm]
nm_p <- c(nm_p, "uuid")

ed <- subset(e_p, select=subnm)
e_p_left <- subset(e_p, select=nm_p)

rm(subnm, nm_p)

###################
###SNAPSHOT#####
ed$facility_name <- e_p$school_name
ed$facility_type <- e_p$level_of_education
ed$education_type <- e_p$education_type
ed$owner_manager <- NA
ed$num_tchrs_total <- e_p$num_tchrs_total
ed$num_students_total <- e_p$num_students_total
ed$num_classrms_total <- e_p$num_total_classrooms
ed$chalkboard_each_classroom_yn <- e_p$num_total_classrooms <= e_p$chalkboard_each_classroom_yn
ed$improved_water_supply <- e_p$water_pipe_water | e_p$water_tube_well
ed$improved_sanitation <- e_p$toilet_flush_or_pour_flush | e_p$toilet_ventilated_improved | e_p$toilet_pit_latrine_with_slab
ed$phcn_electricity <- e_p$power_grid_connection == T
#####################
####################

ed$mylga <-e_p$lga
ed$mylga_state <- e_p$state
ed$mylga_zone <- e_p$zone
ed$unique_lga <- paste(ed$mylga_state, ed$mylga, sep='_')

ed$formhub_photo_id <- e_p$photo

ed$Type <- e_p$education_type

ed$potable_water <- ((e_p$days_no_potable_water < 7) & (e_p$water_none == FALSE))

ed$num_textbooks <- rowSums(e_p[,c("num_textbooks_english", "num_textbooks_math", 
                                   "num_textbooks_social_sci", "num_textbooks_pry_sci")], na.rm=T)
ed$textbook_to_pupil_ratio <- ed$num_textbooks / rowSums(e_p[,c("num_students_female", "num_students_male")], na.rm=T) #num_students_total doesnt EXIST, calculated based on sum of male & female

ed$school_1kmplus_secondary_school <- (e_p$km_to_secondary_school > 1)

ed$male_to_female_student_ratio <- e_p$num_students_male / e_p$num_students_female



ed$education_improved_sanitation <- apply(as.matrix(cbind(e_p$toilet_flush_or_pour_flush == TRUE,
                                                          e_p$toilet_ventilated_improved == TRUE, 
                                                          e_p$toilet_pit_latrine_with_slab == TRUE)), 1, any_na.rm) 

ed$covered_roof_good_condi <- e_p$covered_roof_yn %in% c("roof_fence_good_condition", 'yes')

ed$pupil_classrm_ratio <- rowSums(e_p[,c("num_students_female", "num_students_male")], na.rm=T) / rowSums(e_p[, c("num_classrms_good_cond", 
                                                                                                                  "num_classrms_need_min_repairs", 
                                                                                                  "num_classrms_need_maj_repairs")], na.rm=T)
ed$multigrade_classrms <- (e_p$num_classrooms_multiple_use >= 1)

ed$pupil_bench_ratio <- (rowSums(e_p[,c("num_students_female", "num_students_male")], na.rm=T) / e_p$X_p_num_benches_chairs)
ed$pupil_desk_ratio <- (rowSums(e_p[,c("num_students_female", "num_students_male")], na.rm=T) / e_p$X_p_num_total_desk)

ed$tchr_pay_delay <- (as.numeric(e_p$times_tchr_pay_delay_pastyr) >= 1)

ed$tchr_pay_miss <- (as.numeric(e_p$times_tchr_pay_miss_pastyr) >= 1)

e_pilot_comp <- ed 

ed$phcn_electricity <- e_p$power_grid_connection == T


#Adding distant to every facility
#combining calculated result back to original data
ed <- lga_boudary_dist(ed, gps_col="gps")
e_pilot_comp <- ed
e_p <- merge(ed, e_p_left, by="uuid")


#Delete all those have dist >= 35 km
e_pilot_comp <- subset(e_pilot_comp, dist_fake <= 35 | is.na(dist_fake))
e_p <- subset(e_p, dist_fake <= 35 | is.na(dist_fake))


#writing
write.csv(x_y_killa(e_pilot_comp), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Education_Pilot_NMIS_Facility.csv", row.names=F)
write.csv(x_y_killa(e_p), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Education_Pilot_ALL_FACILITY_INDICATORS.csv", row.names=F)
