#hand_calculation checks

##HEALTH##
########################################################################################
#lga:661################################################################################
########################################################################################
# ukwa_e
h_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Health_661_Merged.csv", 
                       stringsAsFactors=F)
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
                        proportion_growth_monitoring, proportion_no_user_fees_child_health,
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


  