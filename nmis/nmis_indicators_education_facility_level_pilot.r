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
ed <- subset(e_p, select=subnm)
rm(subnm)

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
#ed$num_tchrs_total, ** num_tchr_total doesnt exist in  edu_pilot data
ed$Type <- e_p$education_type
#ed$school_1kmplus_catchment_area , ** km_to_catchment_area doesn't exist
ed$potable_water <- ((e_p$days_no_potable_water < 7) & (e_p$water_none == FALSE))
#ed$num_tchrs_with_nce, ** tchrs_male_nce and tchrs_female_nce DONT EXIST ~^ rowSums(e_p[, c("tchrs_male_nce", "tchrs_female_nce")], na.rm=T) 
ed$num_textbooks <- rowSums(e_p[,c("num_textbooks_english", "num_textbooks_math", 
                                   "num_textbooks_social_sci", "num_textbooks_pry_sci")], na.rm=T)
ed$textbook_to_pupil_ratio <- ed$num_textbooks / rowSums(e_p[,c("num_students_female", "num_students_male")], na.rm=T) #num_students_total doesnt EXIST, calculated based on sum of male & female
# ed$natl_curriculum_yn, **natl_curriculum_yn NOT EXIST
ed$school_1kmplus_secondary_school <- (e_p$km_to_secondary_school > 1)
# ed$students_living_3kmplus_school, ** num_students_frthr_than_3km NO EXIST ~^ ( (e_p$num_students_frthr_than_3km / e_p$num_students_total) > 0.2 ) #Inf
ed$male_to_female_student_ratio <- e_p$num_students_male / e_p$num_students_female


# ed$education_improved_sanitation, **flush_toilet_drain_to NOT EXIST ~^ (e_p$flush_toilet_drain_to == 'improved')), 
ed$education_improved_sanitation <- apply(as.matrix(cbind(e_p$toilet_flush_or_pour_flush == TRUE,
                                                          e_p$toilet_ventilated_improved == TRUE, 
                                                          e_p$toilet_pit_latrine_with_slab == TRUE)), 1, any_na.rm) 

# ed$pupil_toilet_ratio_facility, ** flush_toilet_drain_to NOT EXIST
# ed$pupil_toilet_ratio_facility <- (ifelse(e_p$flush_toilet_drain_to == "improved", 
#                                           rowSums(cbind(e_p$flush_toilet_number, 
#                                                         as.numeric(e_p$vip_latrine_number),
#                                                         as.numeric(e_p$slab_pit_latrine_number)), na.rm=T),
#                                           rowSums(cbind(as.numeric(e_p$vip_latrine_number),
#                                                         as.numeric(e_p$slab_pit_latrine_number),
#                                                         ed$education_improved_sanitation), na.rm=T)) / e_p$num_students_total )


# ed$power_access, ** all funct_yn NOT EXIST
# no "power_sources_generator_p", "power_grid_connection_p", "power_solar_system"
# ed$power_access <- apply(as.matrix(cbind(e_p$power_generator & e_p$generator_funct_yn == 'yes', 
#                                          e_p$power_solar_system & e_p$solar_funct_yn == 'yes', 
#                                          e_p$power_grid_connection & e_p$grid_funct_yn == 'yes')), 
#                                          1, any_na.rm)

ed$covered_roof_good_condi <- e_p$covered_roof_yn %in% c("roof_fence_good_condition", 'yes')
# ed$access_clinic_dispensary, ** health_services_yn NOT EXIST ~^ ed$access_clinic_dispensary <- e_p$health_services_yn %in% c("health_services_clinic")
# SAME as ABOVE ~^ ed$access_first_aid <- e_p$health_services_yn %in% c("health_services_clinic", "health_services_aid_kit")

# ed$wall_fence_good_condi, **boundary_wall_fence_yn has ONLY 'yes'/'no' ~^ <- e_p$boundary_wall_fence_yn %in% c("roof_fence_good_condition")


ed$pupil_classrm_ratio <- rowSums(e_p[,c("num_students_female", "num_students_male")], na.rm=T) / rowSums(e_p[, c("num_classrms_good_cond", 
                                                                                                                  "num_classrms_need_min_repairs", 
                                                                                                                  "num_classrms_need_maj_repairs")], na.rm=T)
# ed$classes_outside_yn, ** classes_outside_yn NOT EXIST 
ed$multigrade_classrms <- (e_p$num_classrooms_multiple_use >= 1)

ed$pupil_bench_ratio <- (rowSums(e_p[,c("num_students_female", "num_students_male")], na.rm=T) / e_p$X_p_num_benches_chairs)
ed$pupil_desk_ratio <- (rowSums(e_p[,c("num_students_female", "num_students_male")], na.rm=T) / e_p$X_p_num_total_desk)

#ed$pupil_tchr_ratio, ** num_tchrs_total NOT EXIST, ~^ (e_p$num_students_total / e_p$num_tchrs_total)

# ed$teacher_nonteachingstaff_ratio, ** num_tchrs_total, num_sr_staff_total,num_jr_staff_total NOT EXIST
# ~^ (e_p$num_tchrs_total / rowSums(e_p[, c("num_sr_staff_total", "num_jr_staff_total")], na.rm=T))

# ed$num_tchrs_attended_training, ** num_tchrs_attended_training NOT EXIST
ed$tchr_pay_delay <- (as.numeric(e_p$times_tchr_pay_delay_pastyr) >= 1)

ed$tchr_pay_miss <- (as.numeric(e_p$times_tchr_pay_miss_pastyr) >= 1)

#ed$functioning_library_yn, ** "functioning_library_yn" NOT in REFERENCE
#ed$provide_exercise_books_yn, ** "provide_exercise_books_yn" NOT REFERENCE

e_pilot_comp <- ed 

# ed$owner_manager <- # not sure how to extract data from this => X_p_managed_by
# ed$improved_water_supply <- 

ed$phcn_electricity <- e_p$power_grid_connection == T

#writing
write.csv(boundary_clean(e_pilot_comp,"mylga_state", "gps"), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Education_Pilot_NMIS_Facility.csv", row.names=F)
write.csv(boundary_clean(cbind(e_p, e_pilot_comp),"mylga_state", "gps"), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Education_Pilot_ALL_FACILITY_INDICATORS.csv", row.names=F)

#all facility indicators
# write.csv(boundary_clean(rbind.fill(e_pilot_comp, education_113_comp), "mylga_state", "gps"), "raw_data/113/Education_pilot_113_combined.csv", row.names=F)


