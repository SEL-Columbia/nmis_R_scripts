## ALIASES / PREP ##
source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")

edu_outlier <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Education_774_outliercleaned.rds")

edu_sub <- subset(edu_outlier , select=c("uuid", "mylga", "mylga_state", "mylga_zone", "gps", "school_name", "level_of_education", "unique_lga", "lga_id", "photo", "src"))
edu_sub$formhub_photo_id <- edu_sub$photo


nm_774 <- names(edu_outlier)[! names(edu_outlier) %in% names(edu_sub)]
nm_774 <- c(nm_774, "uuid")
e_774_left <- subset(edu_outlier, select=nm_774)
rm(nm_774)

################
## SNAPSHOT ####
edu_sub$facility_name <- edu_sub$school_name
edu_sub$facility_type <- edu_sub$level_of_education
edu_sub$education_type <- edu_outlier$education_type

edu_sub$owner_manager <- recodeVar(edu_outlier$school_managed, 
                          c("fed_gov", "loc_gov", "st_gov", "priv_profit", "priv_noprofit", "faith_org"),
                          c("public", "public", "public", "private", "private", "private"),
                          default=NA)

edu_sub$num_tchrs_total <- edu_outlier$num_tchrs_total
edu_sub$num_students_total <- edu_outlier$num_students_total
edu_sub$num_classrms_total <- edu_outlier$num_classrms_total
edu_sub$chalkboard_each_classroom_yn <- edu_outlier$chalkboard_each_classroom_yn
edu_sub$improved_water_supply <- (edu_outlier$water.pipe_water == T) | (edu_outlier$water.tube_well == T)

edu_sub$improved_sanitation <- edu_outlier$toilet.flush_or_pour_flush_improved == T | 
                                    edu_outlier$toilet.ventilated_improved == T | 
                                    edu_outlier$toilet.pit_latrine_with_slab == T


edu_sub$phcn_electricity <- ifelse(edu_outlier$src == "661", 
                                   edu_outlier$grid_proximity == 'connected_to_grid',
                                   edu_outlier$power_sources.grid == T)
################
##################

edu_sub$school_1kmplus_catchment_area <- edu_outlier$km_to_catchment_area > 1
edu_sub$num_classrms_need_maj_repairs <- edu_outlier$num_classrms_need_maj_repairs


edu_sub$num_textbooks <- 
    ifelse(edu_outlier$src == "661",
        ifelse(edu_outlier$level_of_education %in% c('primary_only', 'preprimary_and_primary'),  
               edu_outlier$num_math_textbook_pry + edu_outlier$num_english_textbook_pry + 
               edu_outlier$num_soc_science_textbook_pry + edu_outlier$num_science_textbook_pry,
        ifelse(edu_outlier$level_of_education %in% c('junior_and_senior_sec', 'juniors_sec_only'),
               edu_outlier$num_math_textbook_js + edu_outlier$num_english_textbook_js + 
               edu_outlier$num_soc_science_textbook_js + edu_outlier$num_science_textbook_js,
        ifelse(edu_outlier$level_of_education %in% c('primary_and_junior_sec', 'primary_junior_and_senior_sec'),
               edu_outlier$num_math_textbook_pry + edu_outlier$num_english_textbook_pry + 
               edu_outlier$num_soc_science_textbook_pry + edu_outlier$num_science_textbook_pry +
               edu_outlier$num_math_textbook_js + edu_outlier$num_english_textbook_js + 
               edu_outlier$num_soc_science_textbook_js + edu_outlier$num_science_textbook_js,
               0))),  
            apply(cbind(edu_outlier$num_textbooks_english, 
                        edu_outlier$num_textbooks_math, 
                        edu_outlier$num_textbooks_social_sci,
                        edu_outlier$num_textbooks_pry_sci), 1, sum, na.rm=T))
                                      

## Fix the num_textbooks outlier issuse, take 0.95 quantile(1300) as the cutt off 
edu_sub$num_textbooks <- ifelse(edu_sub$num_textbooks > 2000, NA, edu_sub$num_textbooks)
    
edu_sub$textbook_to_pupil_ratio <- edu_sub$num_textbooks / edu_outlier$num_students_total

edu_sub$natl_curriculum_yn <- edu_outlier$natl_curriculum_yn 

## ACCESS ##
# see school_1kmplus_catchment_area from above
edu_sub$school_1kmplus_secondary_school <- edu_outlier$km_to_secondary_school > 1
edu_sub$students_living_3kmplus_school <- edu_outlier$num_students_frthr_than_3km

## PARTICIPATION ##
edu_sub$male_to_female_student_ratio <- edu_outlier$num_students_male / edu_outlier$num_students_female

## Infrastructure: Water & San ##
edu_sub$functional_water <- edu_outlier$borehole_tubewell_repair_time == TRUE
edu_sub$potable_water <- edu_outlier$potable_water
# notes: water.tube_well = borehole or tubewell; not including "protected" wells here; 
# repair_time is really a question about functionality



edu_sub$gender_separated_toilets_yn <- (edu_outlier$num_toilet_boy > 1) & (edu_outlier$num_toilet_girl > 1)

edu_sub$pupil_toilet_ratio_facility <- edu_outlier$num_students_total / edu_outlier$num_toilet_total)


          
edu_sub$pupil_toilet_ratio_facility <- ifelse(is.infinite(edu_sub$pupil_toilet_ratio_facility),
                                              NA, edu_sub$pupil_toilet_ratio_facility)

# can't trust the xform calculations because of "999" numbers
## Infrastructure: Building Structure ##
edu_sub$power_access <- (edu_outlier$power_sources.generator & edu_outlier$generator_funct_yn) |
                        (edu_outlier$power_sources.solar_system & edu_outlier$solar_funct_yn) |
                        (edu_outlier$power_sources.grid & edu_outlier$grid_funct_yn)

edu_sub$num_classrms_need_min_repairs <- edu_outlier$num_classrms_need_min_repairs
edu_sub$covered_roof_good_condi <- edu_outlier$covered_roof_good_condi
edu_sub$num_classrms_total <- edu_outlier$num_classrms_total

## Infrastructure: Health and Safety ##
edu_sub$access_clinic_dispensary <- edu_outlier$health_services_yn %in% c('yes_clinic_dispensary', 
                                                                          'health_services_clinic')

edu_sub$access_first_aid <- edu_outlier$health_services_yn %in% c('first_aid_kit', 
                                                                  "health_services_clinic", 
                                                                  "health_services_aid_kit")

edu_sub$wall_fence_good_condi <- edu_outlier$boundary_wall_fence_yn %in% c("yes_good_condition", 
                                                                           "roof_fence_good_condition",
                                                                           "yes")
## Infrastructure: Learning Environment ##
edu_sub$pupil_classrm_ratio <- edu_outlier$num_students_total / edu_outlier$num_classrms_total
# actually, lets just make sure to re-calculate totals in the outlier scripts
edu_sub$classes_outside_yn <- edu_outlier$classes_outside_yn 
edu_sub$two_shifts_yn <- edu_outlier$two_shifts_yn

edu_sub$multigrade_classrms <- edu_outlier$multigrade_teaching_yn %in% c('yes_not_enough_space',
                                                                         'yes_no_teacher_no_space',
                                                                         'yes_not_enough_teacher')
edu_sub$multigrade_classrms[edu_sub$src == "113"] <- (edu_outlier$num_classrooms_multiple_use[edu_outlier$src == "113"] >=1) 


## Furniture ##
edu_sub$pupil_bench_ratio <- edu_outlier$num_students_total / edu_outlier$num_benches
edu_sub$pupil_desk_ratio <- edu_outlier$num_students_total / edu_outlier$num_desks

## Adequacy of Staffing ##
edu_sub$pupil_tchr_ratio <- edu_outlier$num_students_total / edu_outlier$num_tchrs_total
edu_sub$teacher_nonteachingstaff_ratio <- edu_outlier$num_tchrs_total / (edu_outlier$num_sr_staff_total)
edu_sub$num_tchrs_with_nce <- edu_outlier$num_tchrs_w_nce
edu_sub$num_tchrs_attended_training <- edu_outlier$num_tchrs_attended_training

## Institutional Development ##
edu_sub$tchr_pay_delay <- edu_outlier$times_tchr_pay_delay_pastyr > 0
edu_sub$tchr_pay_miss <- edu_outlier$times_tchr_pay_miss_pastyr > 0

## Curriculum Issues ##
edu_sub$textbook_to_pupil_ratio <- edu_sub$num_textbooks / edu_outlier$num_students_total
edu_sub$provide_exercise_books_yn <- edu_outlier$provide_exercise_books_yn 
edu_sub$provide_pens_yn <- edu_outlier$provide_pens_yn 
edu_sub$teacher_guide_yn <- edu_outlier$teacher_guide_yn 
edu_sub$functioning_library_yn <- edu_outlier$functioning_library_yn 


#Adding distant to every facility
#combining calculated result back to original data
edu_sub <- lga_boudary_dist(edu_sub, gps_col="gps")
# education_661_comp <- edu_sub
e_774 <- merge_non_redundant(edu_sub, e_774_left, by="uuid")


#Delete all those have dist >= 35 km
edu_sub <- subset(edu_sub, dist_fake <= 35 | is.na(dist_fake))
e_774 <- subset(e_774, dist_fake <= 35 | is.na(dist_fake))



saveRDS(x_y_killa(edu_sub), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Normalized/Education_774_NMIS_Facility.rds")
saveRDS(x_y_killa(e_774), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Normalized/Education_774_ALL_FACILITY_INDICATORS.rds")
