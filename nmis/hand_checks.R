#hand_calculation checks

########################################################################################
##HEALTH#################################################################################
########################################################################################

############################
#lga:661 ukwa_east##########
#########################################################################################
h_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Health_661_Merged.csv", 
                       stringsAsFactors=F)
#subsetting for dataframe creation
ukiez <- subset(h_661, mylga == "ukwa_east") 

ukwa_east <- data.frame(num_health_facilities, num_level_2_health_facilities,
                        num_level_3_health_facilities, num_level_4_health_facilities,
                        num_level_other_health_facilities, num_health_facilities,
                        proportion_health_facilities_inpatient_care, 
                        proportion_health_facilities_open_24_7,
                        num_doctors, num_nursemidwives_midwives,
                        num_nurses, num_chews, num_lab_techs,
                        num_skilled_health_providers_per_1000,
                        num_chews_per_1000, proportion_staff_paid,
                        proportion_health_facilities_routine_immunization,
                        proportion_growth_monitoring, #proportion_deworming,
                        proportion_no_user_fees_child_health,
                        proportion_delivery_24_7, proportion_at_least_1_sba,
                        proportion_antenatal, num_health_facilities_c_sections,
                        proportion_access_functional_emergency_transport,
                        proportion_family_planning, proportion_delivery_no_user_fees,
                        proportion_health_facilities_hiv_testing,
                        proportion_health_facilities_art_treatment,
                        proportion_malaria_testing, proportion_act_treatment_for_malaria,
                        proportion_malaria_prevention_pregnancy,
                        proportion_offer_bednets, proportion_no_user_fees_malaria,
                        proportion_health_facilities_tb_treatment,
                        proportion_health_facilities_tb_testing,
                        proportion_any_power_access, #proportion_improved_water_source,
                        #proportion_functional_sanitation, 
                        proportion_mobile_coverage,
                        proportion_health_facilities_med_waste_separated,
                        proportion_stockout_essential_meds,  
                                      stringsAsFactors=F)
  
#facilities
num_level_1_health_facilities = 5
num_level_2_health_facilities = 0 
num_level_3_health_facilities = 14
num_level_4_health_facilities = 0
num_level_other_health_facilities = 0
num_health_facilities = 19
proportion_health_facilities_inpatient_care = 0.7368421
proportion_health_facilities_open_24_7 = 0.5263158

#staffing
num_doctors = 0
num_nursemidwives_midwives = 15
num_nurses = 5
num_chews = 34
num_lab_techs = 5
num_skilled_health_providers_per_1000 = 0.5775928
num_chews_per_1000 = 0.3397605
proportion_staff_paid = 0.9473684

#child health
proportion_health_facilities_routine_immunization = .3157895
proportion_growth_monitoring = .3157895
# proportion_deworming == n/a
proportion_no_user_fees_child_health = 0.3684211

#maternal health
proportion_delivery_24_7 = 0.3157895
proportion_at_least_1_sba = 0.3157895
proportion_antenatal = 0.8421053
num_health_facilities_c_sections = 0 
proportion_access_functional_emergency_transport = 0.8421053
proportion_family_planning = 0.5263158 
proportion_delivery_no_user_fees = 0.1052632 

#hiv/aids, malaria and other diseases
proportion_health_facilities_hiv_testing = 0
proportion_health_facilities_art_treatment = 0.9473684
proportion_malaria_testing = 0 
proportion_act_treatment_for_malaria = 0.3157895
proportion_malaria_prevention_pregnancy = 0.7368421
proportion_offer_bednets = 0.2105263
proportion_no_user_fees_malaria = 0.3157895
proportion_health_facilities_tb_treatment = 0.2631579
proportion_health_facilities_tb_testing = 0.05263158

#infrastructure
proportion_any_power_access = 0.4210526
#TODO: proportion_improved_water_source = ??
#TODO: proportion_functional_sanitation = ?? 
proportion_mobile_coverage = 0 
proportion_health_facilities_med_waste_separated = 0.6315789

#equip
proportion_stockout_essential_meds = 0.6842105

#TODO: write out csvs to somewhere!
# write.csv("")
rm(ukiez, ukwa_east)


###############################
#lga:113 kaiama###############
##################################################################################################
h_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Health_PhII_RoundI&II&III_Clean_2011.10.21.csv",
                  na.strings = c('n/a',"NA"),stringsAsFactors=F)
h_113 <- subset(h_113, subset=!is.na(geocodeoffacility)) # REMOVING ALL FACILITIES WITHOUT GEO CODE
h_113$uuid <- sapply(paste(h_113$geocodeoffacility, h_113$photo), FUN=digest)
h_113 <- subset(h_113, !duplicated(h_113$uuid))
# OUTPUT SHOULD BE 0
anyDuplicated(h_113$uuid)
#subsetting for dataframe creation
kaiamz <- subset(h_113, lga == "KAIAMA") 

kaiama <- data.frame(num_health_facilities, num_level_2_health_facilities,
                        num_level_3_health_facilities, num_level_4_health_facilities,
                        num_level_other_health_facilities, num_health_facilities,
                        proportion_health_facilities_inpatient_care, 
                        proportion_health_facilities_open_24_7,
                        num_doctors, num_nursemidwives_midwives,
                        num_nurses, num_chews, num_lab_techs,
                        num_skilled_health_providers_per_1000,
                        num_chews_per_1000, proportion_staff_paid,
                        proportion_health_facilities_routine_immunization,
                        proportion_growth_monitoring, proportion_deworming,
                        proportion_no_user_fees_child_health,
                        proportion_delivery_24_7, proportion_at_least_1_sba,
                        proportion_antenatal, #num_health_facilities_c_sections,
                        proportion_access_functional_emergency_transport,
                        proportion_family_planning, proportion_delivery_no_user_fees,
                        proportion_health_facilities_hiv_testing,
                        proportion_health_facilities_art_treatment,
                        proportion_malaria_testing, proportion_act_treatment_for_malaria,
                        proportion_malaria_prevention_pregnancy,
                        proportion_offer_bednets, proportion_no_user_fees_malaria,
                        proportion_health_facilities_tb_treatment,
                        proportion_health_facilities_tb_testing,
                        proportion_any_power_access, #proportion_improved_water_source,
                        #proportion_functional_sanitation, 
                        proportion_mobile_coverage,
                        proportion_health_facilities_med_waste_separated,
                        proportion_stockout_essential_meds,  
                        stringsAsFactors=F)

#facilities
num_level_1_health_facilities = 4
num_level_2_health_facilities = 9  
num_level_3_health_facilities = 17
num_level_4_health_facilities = 0 
num_level_other_health_facilities = 2 
num_health_facilities = 32
proportion_health_facilities_inpatient_care = 0.5625 
proportion_health_facilities_open_24_7 = 30

#staffing
num_doctors = 4
num_nursemidwives_midwives = 13
num_nurses = 18
num_chews = 55
num_lab_techs = 8
num_skilled_health_providers_per_1000 = 0.2818852
num_chews_per_1000 = 0.4429625
proportion_staff_paid = 1.00

#child health
proportion_health_facilities_routine_immunization = 0.625
proportion_growth_monitoring = 0.375
proportion_deworming = 0.46875
proportion_no_user_fees_child_health = 0.78125

#maternal health
proportion_delivery_24_7 = 0.03125
proportion_at_least_1_sba = 0.1875
proportion_antenatal = 0.90625
# num_health_facilities_c_sections =  
proportion_access_functional_emergency_transport = 0.5625 
proportion_family_planning = 0.40625
proportion_delivery_no_user_fees = 0.78125  

#hiv/aids, malaria and other diseases
proportion_health_facilities_hiv_testing = 0.125
proportion_health_facilities_art_treatment = 0.46875
proportion_malaria_testing = 0.125 
proportion_act_treatment_for_malaria = 0.84375
proportion_malaria_prevention_pregnancy = 0.5625
proportion_offer_bednets = 0.8125
proportion_no_user_fees_malaria = 0.4375
proportion_health_facilities_tb_treatment = 0.1875
proportion_health_facilities_tb_testing = 0.0625

#infrastructure
proportion_any_power_access = 0.28125
#TODO: proportion_improved_water_source = ??
#TODO: proportion_functional_sanitation = ?? 
proportion_mobile_coverage = 0.5
proportion_health_facilities_med_waste_separated = 0.71875

#equip
proportion_stockout_essential_meds = 0.25

#TODO: write out csvs to somewhere!
# write.csv("")
rm(kaiamz, kaiama)
  
###############################
#lga:pilot miga################
##################################################################################################
h_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Data_Health_Clean_2011.11.18.csv",                    
                    na.strings = c('NA', 'n/a'), stringsAsFactors=F)
h_pilot <- subset(h_pilot, subset=!is.na(geocodeoffacility)) # REMOVING ALL FACILITIES WITHOUT GEO CODE
h_pilot$uuid <- sapply(paste(h_pilot$geocodeoffacility, h_pilot$photo), FUN=digest)
h_pilot <- subset(h_pilot, !duplicated(h_pilot$uuid))
# OUTPUT SHOULD BE 0
anyDuplicated(h_pilot$uuid)
#subsetting for dataframe creation
migaz <- subset(h_pilot, lga == "MIGA")

miga <- data.frame(num_health_facilities, num_level_2_health_facilities,
                     num_level_3_health_facilities, num_level_4_health_facilities,
                     num_level_other_health_facilities, num_health_facilities,
                     proportion_health_facilities_inpatient_care, 
                     proportion_health_facilities_open_24_7,
                     num_doctors, num_nursemidwives_midwives,
                     num_nurses, num_chews, num_lab_techs,
                     num_skilled_health_providers_per_1000,
                     num_chews_per_1000, proportion_staff_paid,
                     proportion_health_facilities_routine_immunization,
                     proportion_growth_monitoring, proportion_deworming,
                     proportion_no_user_fees_child_health,
                     proportion_delivery_24_7, proportion_at_least_1_sba,
                     proportion_antenatal, #num_health_facilities_c_sections,
                     proportion_access_functional_emergency_transport,
                     proportion_family_planning, proportion_delivery_no_user_fees,
                     proportion_health_facilities_hiv_testing,
                     proportion_health_facilities_art_treatment,
                     proportion_malaria_testing, proportion_act_treatment_for_malaria,
                     proportion_malaria_prevention_pregnancy,
                     proportion_offer_bednets, proportion_no_user_fees_malaria,
                     proportion_health_facilities_tb_treatment,
                     proportion_health_facilities_tb_testing,
                     proportion_any_power_access, #proportion_improved_water_source,
                     #proportion_functional_sanitation, 
                     proportion_mobile_coverage,
                     proportion_health_facilities_med_waste_separated,
                     #proportion_stockout_essential_meds,  
                     stringsAsFactors=F)

#facilities
num_level_1_health_facilities = 11
num_level_2_health_facilities = 7
num_level_3_health_facilities = 4
num_level_4_health_facilities = 0  
num_level_other_health_facilities = 0
num_health_facilities = 22
proportion_health_facilities_inpatient_care = 0.4090909 
proportion_health_facilities_open_24_7 = 0.3636364 

#staffing
num_doctors = 13
num_nursemidwives_midwives = NA
num_nurses = NA
num_chews = 37
num_lab_techs = 44
num_skilled_health_providers_per_1000 = 0.1012272
num_chews_per_1000 = 0.3426151
proportion_staff_paid = 1.0

#child health
proportion_health_facilities_routine_immunization = 0.2727273
proportion_growth_monitoring = 0.5909091
proportion_deworming = 0.6818182
proportion_no_user_fees_child_health = 0.2727273

#maternal health
proportion_delivery_24_7 = 0 
proportion_at_least_1_sba = 0.1818182
proportion_antenatal = 0.5909091
# num_health_facilities_c_sections =  
proportion_access_functional_emergency_transport = 0
proportion_family_planning = 0.5
proportion_delivery_no_user_fees = 0.7272727

#hiv/aids, malaria and other diseases
proportion_health_facilities_hiv_testing = 0 
proportion_health_facilities_art_treatment = NA
proportion_malaria_testing = 0.04545455
proportion_act_treatment_for_malaria = 1.0
proportion_malaria_prevention_pregnancy = NA 
proportion_offer_bednets = 0.1818182
proportion_no_user_fees_malaria = 0.5
proportion_health_facilities_tb_treatment = 0.2272727
proportion_health_facilities_tb_testing = 0.04545455

#infrastructure
proportion_any_power_access = 0 
#TODO: proportion_improved_water_source = ??
#TODO: proportion_functional_sanitation = ?? 
proportion_mobile_coverage = NA 
proportion_health_facilities_med_waste_separated = 0.6818182

#equip
# proportion_stockout_essential_meds = n/a 

  
#TODO: write out csvs to somewhere!
# write.csv("")
rm(migaz, miga)



#########################################################################################
##EDUCATION#################################################################################
#########################################################################################

############################
#lga:661 ukwa_east##########
#########################################################################################
e_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Education_661_Merged.csv", 
                  stringsAsFactors=F)
#subsetting for dataframe creation
ukiez <- subset(e_661, mylga == "ukwa_east") 

ukwa_east <- data.frame(num_primary_schools, num_junior_secondary_schools,
                        num_senior_secondary_schools, num_schools,
                        proportion_schools_1kmplus_catchment_primary, proportion_schools_1kmplus_catchment_juniorsec,
                        proportion_schools_1kmplus_ss,proportion_students_3kmplus_primary,
                        proportion_students_3kmplus_juniorsec, net_enrollment_rate_boys_primary,
                        net_enrollment_rate_girls_primary, net_enrollment_rate_boys_js,
                        net_enrollment_rate_girls_js, gender_parity_index_primary, gender_parity_index_js,
                        proportion_schools_potable_water_primary, proportion_schools_potable_water_juniorsec,
                        proportion_schools_improved_sanitation_primary, proportion_schools_improved_sanitation_juniorsec,
                        proportion_schools_gender_sep_toilet_primary, proportion_schools_gender_sep_toilet_juniorsec,
                        pupil_toilet_ratio_primary, pupil_toilet_ratio_secondary, proportion_schools_power_access_primary,
                        proportion_schools_power_access_juniorsec, proportion_classrooms_need_major_repair_primary,
                        proportion_classrooms_need_major_repair_juniorsec, proportion_classrooms_need_minor_repair_primary,
                        proportion_classrooms_need_minor_repair_juniorsec, proportion_schools_covered_roof_good_cond_primary, 
                        proportion_schools_covered_roof_good_cond_primary, proportion_schools_with_clinic_dispensary_primary, 
                        proportion_schools_with_clinic_dispensary_juniorsec, proportion_schools_with_first_aid_kit_primary, 
                        proportion_schools_with_first_aid_kit_juniorsec,
                        proportion_schools_fence_good_cond_primary, proportion_schools_fence_good_cond_juniorsec,
                        student_classroom_ratio_lga_primary, student_classroom_ratio_lga_juniorsec,
                        proportion_schools_hold_classes_outside_primary, proportion_schools_hold_classes_outside_juniorsec,
                        proportion_schools_two_shifts_primary, proportion_schools_two_shifts_juniorsec,
                        proportion_schools_multigrade_classrooms_primary, proportion_schools_multigrade_classrooms_juniorsec,
                        proportion_schools_chalkboard_all_rooms_primary,
                        proportion_schools_chalkboard_all_rooms_juniorsec, pupil_bench_ratio_lga_primary, 
                        pupil_bench_ratio_lga_juniorsec, pupil_desk_ratio_lga_primary, pupil_desk_ratio_lga_juniorsec,
                        primary_school_pupil_teachers_ratio_lga, junior_secondary_school_pupil_teachers_ratio_lga,
                        teacher_nonteachingstaff_ratio_lga_primary, teacher_nonteachingstaff_ratio_lga_juniorsec, 
                        proportion_teachers_nce_primary, proportion_teachers_nce_juniorsec, proportion_teachers_training_last_year_juniorsec,
                        proportion_schools_delay_pay_primary, proportion_schools_delay_pay_juniorsec, proportion_schools_missed_pay_primary,
                        proportion_schools_missed_pay_juniorsec, num_textbooks_per_pupil_primary, num_textbooks_per_pupil_juniorsec,
                        proportion_provide_exercise_books_primary, proportion_provide_exercise_books_juniorsec,
                        proportion_provide_pens_pencils_primary, proportion_provide_pens_pencils_juniorsec, proportion_natl_curriculum_primary,
                        proportion_natl_curriculum_juniorsec, proportion_teachers_with_teacher_guide_primary,  
                        proportion_teachers_with_teacher_guide_juniorsec, proportion_schools_functioning_library_primary,
                        proportion_schools_functioning_library_juniorsec, transition_rate_primary_to_js1_male, 
                        transition_rate_primary_to_js1_female, repetition_rate_primary_male, repetition_rate_primary_female,
                        literacy_rate,
                              stringsAsFactors=F)
                        
  
#facilities
num_primary_schools =  
num_junior_secondary_schools = 
num_senior_secondary_schools = 
num_schools = 

#access
proportion_schools_1kmplus_catchment_primary = 
proportion_schools_1kmplus_catchment_juniorsec = 
proportion_schools_1kmplus_ss = 
proportion_students_3kmplus_primary = 
proportion_students_3kmplus_juniorsec = 

#participation
net_enrollment_rate_boys_primary
net_enrollment_rate_girls_primary
net_enrollment_rate_boys_js
net_enrollment_rate_girls_js
gender_parity_index_primary
gender_parity_index_js

#infra
proportion_schools_potable_water_primary
proportion_schools_potable_water_juniorsec
proportion_schools_improved_sanitation_primary
proportion_schools_improved_sanitation_juniorsec
proportion_schools_gender_sep_toilet_primary
proportion_schools_gender_sep_toilet_juniorsec
pupil_toilet_ratio_primary
pupil_toilet_ratio_secondary

#building structure
proportion_schools_power_access_primary
proportion_schools_power_access_juniorsec
proportion_classrooms_need_major_repair_primary
proportion_classrooms_need_major_repair_juniorsec
proportion_classrooms_need_minor_repair_primary
proportion_classrooms_need_minor_repair_juniorsec
proportion_schools_covered_roof_good_cond_primary
proportion_schools_covered_roof_good_cond_primary

#health&safety
proportion_schools_with_clinic_dispensary_primary
proportion_schools_with_clinic_dispensary_juniorsec
proportion_schools_with_first_aid_kit_primary
proportion_schools_with_first_aid_kit_juniorsec
proportion_schools_fence_good_cond_primary
proportion_schools_fence_good_cond_juniorsec

#learning environment
student_classroom_ratio_lga_primary
student_classroom_ratio_lga_juniorsec
proportion_schools_hold_classes_outside_primary
proportion_schools_hold_classes_outside_juniorsec
proportion_schools_two_shifts_primary
proportion_schools_two_shifts_juniorsec
proportion_schools_multigrade_classrooms_primary
proportion_schools_multigrade_classrooms_juniorsec

#furniture
proportion_schools_chalkboard_all_rooms_primary
proportion_schools_chalkboard_all_rooms_juniorsec
pupil_bench_ratio_lga_primary
pupil_bench_ratio_lga_juniorsec
pupil_desk_ratio_lga_primary
pupil_desk_ratio_lga_juniorsec

#adecuacy of staffing
primary_school_pupil_teachers_ratio_lga
junior_secondary_school_pupil_teachers_ratio_lga
teacher_nonteachingstaff_ratio_lga_primary
teacher_nonteachingstaff_ratio_lga_juniorsec
proportion_teachers_nce_primary
proportion_teachers_nce_juniorsec
proportion_teachers_training_last_year_juniorsec

#institutional development
proportion_schools_delay_pay_primary
proportion_schools_delay_pay_juniorsec
proportion_schools_missed_pay_primary
proportion_schools_missed_pay_juniorsec

#curriculum issues
num_textbooks_per_pupil_primary
num_textbooks_per_pupil_juniorsec
proportion_provide_exercise_books_primary
proportion_provide_exercise_books_juniorsec
proportion_provide_pens_pencils_primary
proportion_provide_pens_pencils_juniorsec
proportion_natl_curriculum_primary
proportion_natl_curriculum_juniorsec
proportion_teachers_with_teacher_guide_primary
proportion_teachers_with_teacher_guide_juniorsec
proportion_schools_functioning_library_primary
proportion_schools_functioning_library_juniorsec

#efficiency (flow rates)
transition_rate_primary_to_js1_male
transition_rate_primary_to_js1_female
repetition_rate_primary_male
repetition_rate_primary_female

#learning outcomes
literacy_rate




