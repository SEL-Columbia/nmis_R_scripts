##### Percentage signs

# c_nm <- names(combined)
# p_nm <- read.csv("./facility_data/indicator_list", header=F, stringsAsFactors=F)
# p_nm <- p_nm$V1
# p_nm[p_nm == "gross_enrollment_rate_primary"] = "gross_enrollment_ratio_primary_education"
# 
# percent <- p_nm[p_nm %in% c_nm]
# percent <- unique(c(percent, c_nm[grep("percent", c_nm, ignore.case=T)]))
# percent <- unique(c(percent, c_nm[grep("proportion", c_nm, ignore.case=T)]))
# percent <- c(percent, "antenatal_care_coverage")
# 
# p_nm[!p_nm %in% c_nm]


source("source_scripts/post_processing_functions.R")
source("4_nmis_scripts/RJson.R")


percent_names <- c("proportion_schools_power_access_primary", "proportion_health_facilities_hiv_testing", 
                   "proportion_at_least_1_sba", "proportion_schools_multigrade_classrooms_primary", 
                   "proportion_natl_curriculum_primary", "proportion_classrooms_need_major_repair_juniorsec", 
                   "transition_rate_primary_to_js1_female", "net_enrollment_rate_js", 
                   "proportion_schools_gender_sep_toilet_juniorsec", "proportion_schools_gender_sep_toilet_primary", 
                   "gross_enrollment_rate_js", "proportion_health_facilities_inpatient_care", 
                   "proportion_schools_1kmplus_catchment_primary", "proportion_of_children_u5_diarrhea_treated_with_ors_med", 
                   "proportion_schools_missed_pay_juniorsec", "proportion_health_facilities_open_24_7", 
                   "proportion_teachers_training_last_year_primary", "proportion_schools_with_first_aid_kit_juniorsec", 
                   "proportion_access_functional_emergency_transport", "percentage_functional_handpumps", 
                   "proportion_schools_covered_roof_good_cond_juniorsec", "percentage_functional_taps", 
                   "percent_antenatal_care_four", "proportion_teachers_with_teacher_guide_juniorsec", 
                   "proportion_schools_fence_good_cond_primary", "proportion_provide_pens_pencils_juniorsec", 
                   "proportion_schools_improved_sanitation_juniorsec", "percentage_electric_functional", 
                   "repetition_rate_primary_male", "percentage_diesel_functional", 
                   "proportion_classrooms_need_minor_repair_primary", "proportion_schools_multigrade_classrooms_juniorsec", 
                   "proportion_schools_covered_roof_good_cond_primary", "proportion_improved_sanitation", 
                   "proportion_health_facilities_tb_testing", "proportion_offer_bednets", 
                   "proportion_schools_fence_good_cond_juniorsec", "proportion_schools_with_clinic_dispensary_primary", 
                   "net_enrollment_rate_girls_js", "proportion_teachers_training_last_year_juniorsec", 
                   "proportion_delivery_24_7", "proportion_teachers_nce_juniorsec", 
                   "proportion_malaria_prevention_pregnancy", "proportion_schools_improved_sanitation_primary", 
                   "proportion_provide_exercise_books_primary", "proportion_schools_two_shifts_juniorsec", 
                   "proportion_provide_pens_pencils_primary", "proportion_provide_exercise_books_juniorsec", 
                   "proportion_children_u5_sleeping_under_itns", "prevalence_of_underweight_children_u5", 
                   "proportion_schools_functioning_library_primary", "proportion_schools_functioning_library_juniorsec", 
                   "proportion_health_facilities_tb_treatment", "proportion_teachers_nce_primary", 
                   "transition_rate_primary_to_js1_male", "repetition_rate_primary_female", 
                   "percentage_solar_functional", "proportion_health_facilities_art_treatment", 
                   "net_enrollment_rate_boys_js", "proportion_schools_hold_classes_outside_primary", 
                   "proportion_schools_with_clinic_dispensary_juniorsec", "proportion_deworming", 
                   "percentage_households_with_access_to_improved_sanitation", "proportion_schools_missed_pay_primary", 
                   "proportion_schools_chalkboard_all_rooms_juniorsec", "immunization_rate_measles", 
                   "proportion_students_3kmplus_primary", "proportion_schools_1kmplus_catchment_juniorsec", 
                   "proportion_schools_chalkboard_all_rooms_primary", "proportion_stockout_essential_meds", 
                   "literacy_rate", "net_enrollment_rate_pry", "proportion_delivery_no_user_fees", 
                   "proportion_growth_monitoring", "proportion_schools_with_first_aid_kit_primary", 
                   "proportion_no_user_fees_malaria", "gross_enrollment_rate_pry", 
                   "proportion_antenatal", "percentage_households_with_access_to_improved_water_sources", 
                   "proportion_students_3kmplus_juniorsec", "proportion_schools_power_access_juniorsec", 
                   "proportion_schools_delay_pay_juniorsec", "proportion_schools_improved_water_supply_juniorsec", 
                   "proportion_health_facilities_med_waste_separated", "proportion_any_power_access", 
                   "proportion_family_planning", "proportion_improved_water_supply", 
                   "proportion_classrooms_need_minor_repair_juniorsec", "proportion_malaria_testing", 
                   "prevalence_of_stunting_children_u5", "proportion_health_facilities_routine_immunization", 
                   "proportion_act_treatment_for_malaria", "proportion_schools_improved_water_supply_primary", 
                   "proportion_schools_hold_classes_outside_juniorsec", "percentage_pregnant_women_tested_for_hiv_during_pregnancy", 
                   "proportion_teachers_with_teacher_guide_primary", "proportion_natl_curriculum_juniorsec", 
                   "proportion_no_user_fees_child_health", "proportion_staff_paid", 
                   "percentage_of_individuals_tested_for_hiv_ever", "proportion_mobile_coverage", 
                   "proportion_teachers_nce", "proportion_schools_delay_pay_primary", 
                   "immunization_rate_dpt3", "percentage_functional_improved", "proportion_schools_two_shifts_primary", 
                   "proportion_classrooms_need_major_repair_primary", "proportion_schools_1kmplus_ss", 
                   "prevalence_of_wasting_children_u5", "percent_teaching_guides", 
#                    "percentage_population_improved", "percentage_population_improved_functional", 
#                    "population_improved_water_points", "population_improved_functional_water_points",
                   "proportion_households_with_at_least_1_itn", "proportion_men_15_24_with_comp_correct_hiv_aids_knowledge", 
                   "proportion_of_births_with_postnatal_care_within_first_week", 
                   "proportion_of_children_u5_diarrhea_last_2wks", "proportion_of_children_u5_reporting_fever_last_2wks", 
                   "proportion_of_children_u5_treated_with_antimalarial_drugs", 
                   "proportion_women_15_24_with_comp_correct_hiv_aids_knowledge", 
                   "antenatal_care_coverage", "percent_compr_oc_c_sections", "percent_management_public",
                    "percent_management_public_js", "percent_management_public_primary", 
                    "percent_natl_curriculum_primary", "percent_natl_curriculum_js",
                    "percent_natl_curriculum", "num_school_1kmplus_secondary_school", "percent_functional_water_primary",
                    "percent_improved_sanitation_primary","percent_improved_sanitation_js", "percent_phcn_electricity_primary",
                    "percent_phcn_electricity_js", "percent_functional_water_js",
                    "proportion_delivery_24_7_sansHP", "proportion_vaccines_fridge_freezer_sansHP", "proportion_measles", 
                    "proportion_phcn_electricity", "proportion_power_alternative_functional")


nmis_lga <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/All_774_LGA.rds")

for (name in percent_names){
    idx <- which(is.finite(nmis_lga[,name]))
    na_idx <- which(!is.finite(nmis_lga[,name]))
    nmis_lga[idx, name] <- paste(format(round(nmis_lga[idx, name]*100, digits=2), nsmall=2, trim=T), "%", sep="")
    nmis_lga[na_idx, name] <- NA
}

write.csv(nmis_lga,"~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/final_output/All_774_LGA.csv", row.names=F)



######## PUlling GAP SHEET data
gap_sheet <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/final_output/gap_sheet.RDS")


######## Pull in the external state-level data
#It seems we have most of the indicators lets just leave it as it is for now


####### replace baseline data with mopup data
edu_774 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/normalized/Education_774_NMIS_Facility.rds")
edu_774_all <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/normalized/Education_774_ALL_FACILITY_INDICATORS.rds")

health_774 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/normalized/Health_774_NMIS_Facility.rds")
health_774_all <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/normalized/Health_774_ALL_FACILITY_INDICATORS.rds")

water_774 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/normalized/Water_774_NMIS_Facility.rds")
water_774_all <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/normalized/Water_774_ALL_FACILITY_INDICATORS.rds")

edu_774 <- update_mopup(edu_774, edu_flag=T)
edu_774_all <- update_mopup(edu_774_all, edu_flag=T)

health_774 <- update_mopup(health_774, edu_flag=F)
health_774_all <- update_mopup(health_774_all, edu_flag=F)


######## Addiing short id to baseline data
edu_774 <- shortid_generate(edu_774, prefix="E")
stopifnot(!anyDuplicated(edu_774$facility_ID))

edu_774_all <- shortid_generate(edu_774_all, prefix="E")
stopifnot(!anyDuplicated(edu_774_all$facility_ID))

health_774 <- shortid_generate(health_774, prefix="H")
stopifnot(!anyDuplicated(health_774$facility_ID))

health_774_all <- shortid_generate(health_774_all, prefix="H")
stopifnot(!anyDuplicated(health_774_all$facility_ID))

water_774 <- shortid_generate(water_774, prefix="W")
stopifnot(!anyDuplicated(water_774$facility_ID))

water_774_all <- shortid_generate(water_774_all, prefix="W")
stopifnot(!anyDuplicated(water_774_all$facility_ID))

write.csv(water_774, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/final_output/Water_774_NMIS_Facility.csv", row.names=F)
write.csv(edu_774, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/final_output/Education_774_NMIS_Facility.csv", row.names=F)
write.csv(health_774, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/final_output/Health_774_NMIS_Facility.csv", row.names=F)

write.csv(water_774_all, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/final_output/Water_774_ALL_FACILITY_INDICATORS.csv", row.names=F)
write.csv(edu_774_all, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/final_output/Education_774_ALL_FACILITY_INDICATORS.csv", row.names=F)
write.csv(health_774_all, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/final_output/Health_774_ALL_FACILITY_INDICATORS.csv", row.names=F)

# output json file for each lga
output_dir <- "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/final_output/jsons"

RJson_ouput(BASE_DIR=output_dir, nmis_lga, gap_sheet, 
            edu_774, health_774, water_774)

