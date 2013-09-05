#Education 113: facility level
source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")

#reading in data
e_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Education_113_outliercleaned.csv",
                  stringsAsFactors=F)

##### TO DO list:###
e_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Education_661_NMIS_Facility.csv",
                  nrow=1)

###113
subnm <- names(e_661)[which(names(e_661) %in% names(e_113))]
nm_113 <- names(e_113)[! names(e_113) %in% subnm]
nm_113 <- c(nm_113, "uuid")
ed <- subset(e_113, select=subnm)
e_113_left <- subset(e_113, select=nm_113)
rm(subnm, nm_113)

ed$mylga <- e_113$lga
ed$mylga_state <- e_113$state
ed$mylga_zone <- e_113$zone
ed$unique_lga <- paste(ed$mylga_state, ed$mylga, sep='_')
ed$formhub_photo_id <- e_113$photo
#################
####SNAPSHOT#####
ed$facility_name <- e_113$school_name
ed$facility_type <- e_113$level_of_education
ed$education_type <- e_113$education_type
ed$owner_manager <- ifelse(e_113$school_managed_fed_gov, "public", 
                           ifelse(e_113$school_managed_loc_gov, "public",
                                  ifelse(e_113$school_managed_st_gov, "public", 
                                         ifelse(e_113$school_managed_priv_profit, "private", 
                                                ifelse(e_113$school_managed_priv_noprofit, "private", 
                                                       NA_character_)))))
ed$num_tchrs_total <- e_113$num_tchrs_total
ed$num_students_total <- e_113$num_students_total
ed$num_classrms_total <- e_113$num_classrms_total
ed$chalkboard_each_classroom_yn <- e_113$num_classrms_total <= e_113$chalkboard_each_classroom_yn
ed$improved_water_supply <- (e_113$water_pipe_water == T | e_113$water_tube_well == T)
ed$improved_sanitation <- (e_113$toilet_flush_or_pour_flush == T| e_113$toilet_ventilated_improved == T | e_113$toilet_pit_latrine_with_slab)
ed$phcn_electricity <- e_113$power_grid_connection == T
################### 
###################

ed$school_1kmplus_catchment_area <- e_113$km_to_catchment_area > 1
ed$num_tchrs_w_nce <- as.numeric(e_113$num_tchrs_w_nce)
ed$num_textbooks <- rowSums(e_113[,c("num_textbooks_english", "num_textbooks_math", 
                                     "num_textbooks_social_sci", "num_textbooks_pry_sci")], na.rm=T)

ed$textbook_to_pupil_ratio <- ed$num_textbooks / e_113$num_students_total # NA & Inf introduced

ed$school_1kmplus_secondary_school <- (e_113$km_to_secondary_school > 1)
ed$students_living_3kmplus_school <- ( (e_113$num_students_frthr_than_3km / e_113$num_students_total) > 0.2 ) #Inf
ed$male_to_female_student_ratio <- e_113$num_students_male / e_113$num_students_female

#didn't find "_p_num_improved_sanitation", used the education_improved_sanitation instead
ed$pupil_toilet_ratio_facility <- (ifelse(e_113$flush_toilet_drain_to == "improved", 
                                            rowSums(cbind(e_113$flush_toilet_number, 
                                                        as.numeric(e_113$vip_latrine_number),
                                                        as.numeric(e_113$slab_pit_latrine_number)), na.rm=T),
                                            rowSums(cbind(as.numeric(e_113$vip_latrine_number),
                                                        as.numeric(e_113$slab_pit_latrine_number),
                                                        ed$education_improved_sanitation), na.rm=T)) / e_113$num_students_total )

# no "power_sources_generator_p", "power_grid_connection_p", "power_solar_system"
ed$power_access <- apply(as.matrix(cbind(e_113$power_generator & e_113$generator_funct_yn == 'yes', 
                                        e_113$power_solar_system & e_113$solar_funct_yn == 'yes', 
                                        e_113$power_grid_connection & e_113$grid_funct_yn == 'yes')), 
                                        1, any_na.rm)

ed$covered_roof_good_condi <- e_113$covered_roof_yn %in% c("roof_fence_good_condition", 'yes')
ed$access_clinic_dispensary <- e_113$health_services_yn %in% c("health_services_clinic")
ed$access_first_aid <- e_113$health_services_yn %in% c("health_services_clinic", "health_services_aid_kit")
ed$wall_fence_good_condi <- e_113$boundary_wall_fence_yn %in% c("roof_fence_good_condition")
    

ed$pupil_classrm_ratio <- e_113$num_students_total / rowSums(e_113[, c("num_classrms_good_cond", 
                                                                       "num_classrms_need_min_repairs", 
                                                                        "num_classrms_need_maj_repairs")], na.rm=T)

ed$multigrade_classrms <- (e_113$num_classrooms_multiple_use >= 1)

#'_p_num_benches_chairs' not found
ed$pupil_bench_ratio <- (e_113$num_students_total / rowSums(e_113[,c("num_attached_benches", 
                                                                    "num_unattached_benches")], 
                                                            na.rm=T))
#''_p_num_total_desk' not found
ed$pupil_desk_ratio <- (e_113$num_students_total / rowSums(e_113[,c("num_attached_benches", 
                                                                     "num_unattached_desks")], 
                                                           na.rm=T))

ed$pupil_tchr_ratio <- (e_113$num_students_total / e_113$num_tchrs_total)

ed$teacher_nonteachingstaff_ratio <- (e_113$num_tchrs_total / rowSums(e_113[, c("num_sr_staff_total", 
                                                                                "num_jr_staff_total")], 
                                                                      na.rm=T))
    

ed$tchr_pay_delay <- (as.numeric(e_113$times_tchr_pay_delay_pastyr) >= 1)

ed$tchr_pay_miss <- (as.numeric(e_113$times_tchr_pay_miss_pastyr) >= 1)
                        

#Adding distant to every facility
#combining calculated result back to original data
ed <- lga_boudary_dist(ed, gps_col="gps")
education_113_comp <- ed
e_113 <- merge(ed, e_113_left, by="uuid")


#Delete all those have dist >= 35 km
education_113_comp <- subset(education_113_comp, dist_fake <= 35 | is.na(dist_fake))
e_113 <- subset(e_113, dist_fake <= 35 | is.na(dist_fake))



#writing out
write.csv(x_y_killa(education_113_comp), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Education_113_NMIS_Facility.csv", row.names=F)
write.csv(x_y_killa(e_113), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Education_113_ALL_FACILITY_INDICATORS.csv", row.names=F)

