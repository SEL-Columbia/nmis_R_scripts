#hand_calculation checks
##have to be a little careful here because values might be slightly off after outlier removal
########################################################################################
##HEALTH################################################################################
########################################################################################

health <- data.frame(lga = c('ukwa_east', 'kaiama', 'miga'),
                            num_level_1_health_facilities = c('5', '4', '11'),
                            num_level_2_health_facilities = c('0', '9', '7'), 
                            num_level_3_health_facilities = c('14', '17', '4'),
                            num_level_4_health_facilities = c('0', '0', '0'),
                            num_level_other_health_facilities = c('0', '2', '0'),
                            num_health_facilities = c('19', '32', '22'),
                            proportion_health_facilities_inpatient_care = c('0.7368421', '0.5625', '0.4090909'),
                            proportion_health_facilities_open_24_7 = c('0.5263158', '30', '0.3636364'),
                            num_doctors = c('0', '4', '13'),
                            num_nursemidwives_midwives = c('15', '13', 'NA'),
                            num_nurses = c('5', '18', NA),
                            num_chews = c('34', '55', '37'),
                            num_lab_techs = c('5', '8', '44'),
                            num_skilled_health_providers_per_1000 = c('0.5775928', '0.2818852', '0.1012272'),
                            num_chews_per_1000 = c('0.3397605', '0.4429625', '0.3426151'),
                            proportion_staff_paid = c('0.9473684', '1', '1.0'),
                            proportion_health_facilities_routine_immunization = c('.3157895', '0.625', '0.2727273'),
                            proportion_growth_monitoring = c('.3157895', '0.375', '0.5909091'),
                            proportion_deworming == c('NA', '0.46875', '0.6818182'),
                            proportion_no_user_fees_child_health = c('0.3684211', '0.78125', '0.2727273'),
                            proportion_delivery_24_7 = c('0.3157895', '0.03125', '0'),
                            proportion_at_least_1_sba = c('0.3157895', '0.1875', '0.1818182'),
                            proportion_antenatal = c('0.8421053', '0.90625', '0.5909091'),
                            num_health_facilities_c_sections = c('0', NA, NA), 
                            proportion_access_functional_emergency_transport = c('0.8421053', '0.5625', '0'),
                            proportion_family_planning = c('0.5263158', '0.40625', '0.5'), 
                            proportion_delivery_no_user_fees = c('0.1052632', '0.78125', '0.7272727'),                                          
                            proportion_health_facilities_hiv_testing = c('0', '0.125', '0'),
                            proportion_health_facilities_art_treatment = c('0.9473684', '0.46875', NA),
                            proportion_malaria_testing = c('0', '0.125', '0.04545455'), 
                            proportion_act_treatment_for_malaria = c('0.3157895', '0.84375', '1.0'),
                            proportion_malaria_prevention_pregnancy = c('0.7368421', '0.5625', NA),
                            proportion_offer_bednets = c('0.2105263', '0.8125', '0.1818182'),
                            proportion_no_user_fees_malaria = c('0.3157895', '0.4375', '0.5'),
                            proportion_health_facilities_tb_treatment = c('0.2631579', '0.1875', '0.2272727'),
                            proportion_health_facilities_tb_testing = c('0.05263158', '0.0625', '0.04545455'),
                            proportion_any_power_access = c('0.4210526', '0.28125', '0'),
                            #proportion_improved_water_source = c('??', '??', '??'),
                            #proportion_functional_sanitation = c('??', '??', '??'), 
                            proportion_mobile_coverage = c('0', '0.5', NA), 
                            proportion_health_facilities_med_waste_separated = c('0.6315789', '0.71875', '0.6818182'),
                            proportion_stockout_essential_meds = c('0.6842105', '0.25', NA),
                                      stringsAsFactors=F)

##DATA##
h_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Health_661_Merged.csv", 
                  stringsAsFactors=F)
h_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Health_PhII_RoundI&II&III_Clean_2011.10.21.csv",
                  na.strings = c('n/a',"NA"),stringsAsFactors=F)
h_113 <- subset(h_113, subset=!is.na(geocodeoffacility)) # REMOVING ALL FACILITIES WITHOUT GEO CODE

h_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Data_Health_Clean_2011.11.18.csv",                    
                    na.strings = c('NA', 'n/a'), stringsAsFactors=F)
h_pilot <- subset(h_pilot, subset=!is.na(geocodeoffacility)) # REMOVING ALL FACILITIES WITHOUT GEO CODE
h_pilot$uuid <- sapply(paste(h_pilot$geocodeoffacility, h_pilot$photo), FUN=digest)
h_pilot <- subset(h_pilot, !duplicated(h_pilot$uuid))

#subsetting for dataframe creation
ukiez <- subset(h_661, mylga == "ukwa_east") 
kaiamz <- subset(h_113, lga == "KAIAMA") 
migaz <- subset(h_pilot, lga == "MIGA")

#writing out
write.csv(health, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/outputs/hand_checks/health_check.csv", 
          row.names=F)
rm(ukiez, kaiamz, migaz, h_661, h_113, h_pilot)

#########################################################################################
##EDUCATION##############################################################################
#########################################################################################

education <- data.frame(lga = c('ukwa_east', 'kaiama', 'miga'),
                    num_primary_schools =  c('17', '84', '52'),
                    num_junior_secondary_schools = c('4', '14', '7'),
                    num_senior_secondary_schools = c('1', '2', '2'),
                    num_schools = c('22', '100', '61'),
                    proportion_schools_1kmplus_catchment_primary = c('0.5294118', '0.2619048', NA),
                    proportion_schools_1kmplus_catchment_juniorsec = c('0.75', '0.4285714', NA),
                    proportion_schools_1kmplus_ss = c('0.4117647', '0.7857143', '0.6538462'),
                    proportion_students_3kmplus_primary = c('0.4705882', '0.3452381', NA),
                    proportion_students_3kmplus_juniorsec = c('0.5', '0.7142857', NA),
                    net_enrollment_rate_boys_primary = c(NA, NA, NA),
                    net_enrollment_rate_girls_primary = c(NA, NA, NA),
                    net_enrollment_rate_boys_js = c(NA, NA, NA),
                    net_enrollment_rate_girls_js = c(NA, NA, NA),
                    gender_parity_index_primary = c(NA, NA, NA),
                    gender_parity_index_js = c(NA, NA, NA),                     
                        #below indicators may be mislabeled from "proportion_schools_improved_water_supply_primary"
                    # TODO: what is going on here? proportion_schools_potable_water_primary = c('??', '??', ''),
                    # TODO: what is going on here? proportion_schools_potable_water_juniorsec = c('??', '??', ''),
                    proportion_schools_improved_sanitation_primary = c('0.4117647', '0.1666667', '0.1730769'),
                    proportion_schools_improved_sanitation_juniorsec = ('0.25', '0.2857143', '0.8571429'),
                    proportion_schools_gender_sep_toilet_primary = c('0.2352941', '0.1428571', '0.1538462'),
                    proportion_schools_gender_sep_toilet_juniorsec = c('0.25', '0.1428571', '0.5714286'),
                    pupil_toilet_ratio_primary = c('53.33333', NA, '8120'), #really high for miga....
                    pupil_toilet_ratio_secondary = c('720.25', NA, '300.6667'),
                    proportion_schools_power_access_primary = c('0.1176471', '0.04761905', NA),
                    proportion_schools_power_access_juniorsec = c('0', '0.07142857', NA), 
                    proportion_classrooms_need_major_repair_primary = c('0.5741935', '0.2952756', '0.3990385'),
                    proportion_classrooms_need_major_repair_juniorsec = c('0.7580645', '0.2178218', '0.2058824'),
                    proportion_classrooms_need_minor_repair_primary = c('0.3483871', '0.3188976', '0.2692308'),
                    proportion_classrooms_need_minor_repair_juniorsec = c('0.1774194', '0.2475248', '0.2647059'),
                    proportion_schools_covered_roof_good_cond_primary = c('0', '0.1904762', '0.9038462'),
                    proportion_schools_covered_roof_good_cond_juniorsec = c('0', '0.4285714', '1.0'),
                    proportion_schools_with_clinic_dispensary_primary = c('0', '0.01190476', NA),
                    proportion_schools_with_clinic_dispensary_juniorsec = c('0', '0', NA), 
                    proportion_schools_with_first_aid_kit_primary = c('0', '0.3452381', NA),
                    proportion_schools_with_first_aid_kit_juniorsec = c('0', '0.8571429', NA),  
                    proportion_schools_fence_good_cond_primary = c('0', '0.01190476', NA), 
                    proportion_schools_fence_good_cond_juniorsec = c('0', '0', NA), 
                    student_classroom_ratio_lga_primary = c('34.97419', '11.37205', '39.03846'),
                    student_classroom_ratio_lga_juniorsec = c('422.6774', '20.15842', '26.52941'),
                    proportion_schools_hold_classes_outside_primary = c('0.1764706', '0.4166667', NA),
                    proportion_schools_hold_classes_outside_juniorsec = c('0.5', '0.07142857', NA),
                    proportion_schools_two_shifts_primary = c('0', '0.04761905', '0.03846154'),
                    proportion_schools_two_shifts_juniorsec = c('0', '0', '0'), 
                    proportion_schools_multigrade_classrooms_primary = c('0.5294118', '0.452381', '0.1538462'),
                    proportion_schools_multigrade_classrooms_juniorsec = c('0', '0', '0'),
                    proportion_schools_chalkboard_all_rooms_primary = c('0.2352941', '0.8571429', '0.9230769'),
                    proportion_schools_chalkboard_all_rooms_juniorsec = c('0.25', '1.0', '0'),
                    pupil_bench_ratio_lga_primary = c('5.202353', '1.567707', NA),
                    pupil_bench_ratio_lga_juniorsec = c('1.242579', '0.7258467', NA),
                    pupil_desk_ratio_lga_primary = c('4.347099', '3.287991', '7.33514'),
                    pupil_desk_ratio_lga_juniorsec = c('1.228483', '0.9505135', '1.786139'),
                    primary_school_pupil_teachers_ratio_lga = c('18.57563', '9.111987', NA),
                    junior_secondary_school_pupil_teachers_ratio_lga = c('451.8276', '10.23116', NA),
                    teacher_nonteachingstaff_ratio_lga_primary =  c('3.216216', '3.123153', NA),
                    teacher_nonteachingstaff_ratio_lga_juniorsec = c('29', '2.926471', NA), 
                    proportion_teachers_nce_primary = c('0.7857143', '0.2602524', NA),
                    proportion_teachers_nce_juniorsec = c('0.2758621', '0.1959799', NA),
                    proportion_teachers_training_last_year_juniorsec = c('0.8275862', '0.1959799', NA),
                    proportion_schools_delay_pay_primary = c('0.7647059', '0.3571429', '0'),
                    proportion_schools_delay_pay_juniorsec = c('0.25', '0.2857143', '0'),
                    proportion_schools_missed_pay_primary = c('0.1764706', '0.01190476', '0'),
                    proportion_schools_missed_pay_juniorsec = c('0.25', '0.1428571', '0'),                
                    num_textbooks_per_pupil_primary = c('4.675034', '0.2921932', '0.1593596'),
                    num_textbooks_per_pupil_juniorsec = c('0.2336869', '0.05943026', '0.2572062'),
                    proportion_provide_exercise_books_primary = c('0.1176471', '0.08333333', NA),
                    proportion_provide_exercise_books_juniorsec = c('0', '0', NA),
                    proportion_provide_pens_pencils_primary = c('0', '0.0952381', '0.1153846'),
                    proportion_provide_pens_pencils_juniorsec = c('0', '0', '0'), 
                    proportion_natl_curriculum_primary = c('0.4117647', '0.7261905', NA),
                    proportion_natl_curriculum_juniorsec = c('0.75', '0.8571429', NA),
                    proportion_teachers_with_teacher_guide_primary = c('0.4117647', '0.9166667', '0.8461538'),
                    proportion_teachers_with_teacher_guide_juniorsec = c('0.5', '0.7142857', '1.0'),
                    proportion_schools_functioning_library_primary = c('0.4705882', '0.01190476', '0.03846154'),
                    proportion_schools_functioning_library_juniorsec = c('0.25', '0', '0.1428571'), 
                    transition_rate_primary_to_js1_male = c(NA, NA, NA),
                    transition_rate_primary_to_js1_female = c(NA, NA, NA),
                    repetition_rate_primary_male = c(NA, NA, NA),
                    repetition_rate_primary_female = c(NA, NA, NA),
                    literacy_rate = c(NA, NA, NA),
                                                        stringsAsFactors=F)

##DATA##
e_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Education_661_Merged.csv", 
                  stringsAsFactors=F)
e_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Educ_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv",
                  stringsAsFactors=F, na.strings = c("NA", "n/a"))
e_113 <- subset(e_113, subset=!is.na(gps)) # REMOVING ALL FACILITIES WITHOUT GEO CODE
e_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Education_cleaned_2011Nov17.csv",
                    stringsAsFactors=F, na.strings = c("NA", "n/a"))
e_pilot <- subset(e_pilot, subset=!is.na(gps)) # REMOVING ALL FACILITIES WITHOUT GEO CODE

#subsetting for dataframe creation
ukiez <- subset(e_661, mylga == "ukwa_east") 
kaiamz <- subset(e_113, lga == "KAIAMA") 
migaz <- subset(e_pilot, lga == "MIGA") 

#writing out
write.csv(ukwa_east, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/outputs/hand_checks/education_check.csv", 
          row.names=F)
rm(ukiez, kaiamz, migaz, e_661, e_113, e_pilot)


########################################################################################
##WATER#################################################################################
########################################################################################
water <- data.frame(lga = c('ukwa_east', 'kaiama', 'mika'),
                num_improved_water_points = c('24', '212', '1'), 
                num_overhead_tanks = c('15', NA, NA),
                num_taps = c('1', '23', '0'),
                num_handpumps = c('8', '189', '1'),
                num_unimproved_points = c('0', '20', '114'),
                num_total_water_points = c('24', '232', '115'),
#                 percentage_functional_improved = c(NA, '', '1.0'),
                #TODO: should the below indicator be taken from 'percentage_functional_taps' instead for 113/pilot? 
                percentage_funtaps = c('0.5333333', NA, NA),
                percentage_functional_handpumps =  c(NA, '0.7333333', '1.0'),
                num_diesel = c('0', '9', NA),
                percentage_diesel_functional = c(NA, '0.2222222', NA),
                num_electric = c('0', '11', NA),
                percentage_electric_functional = c(NA, '0.1818182', NA),
                num_solar = c('0', '3', NA),
                percentage_solar_functional = c(NA, '0.6666667', NA),
                num_handpumps = c('0', '180', '1'),
                          stringsAsFactors=F)

##DATA##
w_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Water_661_999Cleaned_Reclassified.csv", 
                  stringsAsFactors=F)  
w_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Water_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv",
                  stringsAsFactors=F)
w_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Water_cleaned_2011Aug29.csv",
                    stringsAsFactors=F, na.strings = c("NA", "n/a"))
#subsetting for dataframe creation
ukiez <- subset(w_661, mylga == "ukwa_east") 
kaiamz <- subset(w_113, lga == "KAIAMA") 
migaz <- subset(w_pilot, lga == "Miga") 

#writing out
write.csv(water, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/outputs/hand_checks/water_check.csv", 
          row.names=F)
rm(ukiez, kaiamz, mikaz)

#health data checks
source("source_scripts/NMIS_Functions.R")
source("tests/CheckDataConsistancy.R")

health_norm <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Normalized/Health_774_ALL_FACILITY_INDICATORS.rds")
health_orig <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/pipeline_data_copy/nmis/data_774/Health_774_NMIS_Facility.csv", stringsAsFactors=F)

test <- check_connsistency(health_norm, health_orig)

f_d_nm <- names(test)

compare_value(health_norm, health_orig, f_d_nm[1:2])



