##########
##Health##
##########
setwd("~/Code/nmis_R_scripts/")
source("source_scripts/Normailize_Functions.R")

h_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Health_661_Merged.csv", 
                  stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999"))
h_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Health_PhII_RoundI&II&III_Clean_2011.10.21.csv",
                  stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999"))
h_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Data_Health_Clean_2011.11.18.csv",
                    stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999"))

h_661$src <- "661"
h_113$src <- "113"
h_pilot$src <- "pilot"

h_113$uuid <- sapply(paste(h_113$gps, h_113$photo), FUN=digest)
h_pilot$uuid <- sapply(paste(h_pilot$gps, h_pilot$photo), FUN=digest)


####
common_slugs_113 <- names(h_661)[(which(names(h_661) %in% names(h_113)))]
common_slugs_pilot <- names(h_661)[(which(names(h_661) %in% names(h_pilot)))]
health_common <- common_slug(c("h_113", "h_661", "h_pilot"))
h_class <- common_type(c("h_113", "h_661", "h_pilot"))

########################
#### Mapping Names #####
########################

#661 
h_661 <- rename(h_661, c("fees_adults.paid_services_routine_visit" = "paid_services_routine_visit",
                         "fees_adults.paid_services_lab_testing" = "paid_services_lab_testing",
                         "fees_adults.paid_services_medication" = "paid_services_medication",
                         "fees_adults.paid_services_registration" = "paid_services_registration",
                         "fees_adults.paid_services_routine_anc_visit" = "paid_services_routine_anc_visit",
                         "fees_adults.paid_services_contraceptives" = "paid_services_contraceptives",
                         "fees_adults.paid_services_anc_delivery" = "paid_services_anc_delivery",
                         "fees_adults.paid_services_immunization" = "paid_services_immunization",
                         "fees_adults.paid_services_malaria_treatment" = "paid_services_malaria_treatment",
                         "power_sources.grid" = "power_sources_grid",
                         "power_sources.solar" = "power_sources_solar",
                         "power_sources.generator" = "power_sources_generator"
                          ))

#113
h_113 <- rename(h_113, c("lga" = "mylga",
                         "state" = "mylga_state",
                         "zone" = "mylga_zone",
                         "num_doctors_fulltime" = "num_doctors_posted",
                         "num_midwives_fulltime" = "num_midwives_posted",
                         "num_nurses_fulltime" = "num_nurses_posted",
                         "num_nursemidwives_fulltime" = "num_nursemidwives_posted",
                         "num_chos_fulltime" = "num_cho_posted",
                         "num_chews_fulltime" = "num_chews_posted",
                         "num_jr_chews_fulltime" = "num_junior_chews_posted",
                         "num_lab_techs_fulltime" = "lab_technicians_posted",
                         "bucket_system_number" = "num_bucket_system",
                         "flush_toilet_number" = "num_flush_other", 
                         "water_sources_tap_in_compound" = "water_sources.tap_in_compound", 
                         "water_sources_tap_outside" = "water_sources.tap_outside",
                         "water_sources_borehole_tube_well" = "water_sources.borehole_tube_well",
                         "toilet_types_flush_or_pour_flush" = "num_flush_or_pour_flush_piped",
                         "equipment_scale" = "equipment.scale",
                         "child_health_weighing_scale" = "weighing_scale_funct_yn",
                         "vip_latrine_number" = "num_vip_latrine", 
                         "slab_pit_latrine_number" = "num_pit_w_slab", 
                         "open_pit_latrine_number" = "num_open_pit_latrine",
                         "equipment_bp_machine" = "equipment.bp_machine",
                         "lab_tests_hemoglobin_testing" = "lab_tests.hemoglobin_testing",
                         "lab_tests_urine_testing" = "lab_tests.urine_testing",
                         "emoc_needles_tubing" = "supplies.needles_and_tubing",
                         "emoc_enough_antishock_garment" = "equipment.emoc_antishock_garment",
                         "medication_oral_contraceptives" = "medication.oral_contraceptives",
                         "medication_injectable_contracept" = "medication.injectable_contracept",
                         "child_health_measles_immun" = "immunization.measles_immun",
                         "child_health_opv_immuization" = "immunization.opv_immuization",
                         "child_health_dpt_immunization" = "immunization.dpt_immunization", 
                         "child_health_tetanus_immun" = "immunization.tetanus_immun",
                         "child_health_hepb_immunization" = "immunization.hepb_immunization",
                         "child_health_bcg_immunization" = "immunization.bcg_immunization",
                         "child_health_yellow_fever_immun" = "immunization.yellow_fever_immun",
                         "child_health_csm_immunization" = "immunization.csm_immunization",
                         "lab_tests_malaria_rdt" = "lab_tests.malaria_rdt",
                         "lab_tests_malaria_microscopy" = "lab_tests.malaria_microscopy",
                         "lab_tests_pregnancy" = "lab_tests.pregnancy",
                         "lab_tests_tb_microscopy" = "lab_tests.tb_microscopy",
                         "lab_tests_hiv_testing" = "lab_tests.hiv_testing",
                         "medication_iv_fluid" = "medication.iv_fluid",
                         "medication_antihistamines" = "medication.antihistamines",
                         "medication_anti_diarrheals" = "medication.anti_diarrheals",
                         "medication_anti_pyretics" = "medication.anti_pyretics",
                         "medication_arvs" = "medication.arvs",
                         "medication_tb_medicines" = "medication.tb_medicines",
                         "medication_none" = "medication.none"
                         ))

#pilot
h_pilot <- rename(h_pilot, c("lga" = "mylga",
                             "state" = "mylga_state",
                             "zone" = "my_zone",
                             "num_doctors_fulltime" = "num_doctors_posted",
                             "num_midwives_fulltime" = "num_midwives_fulltime",
                             "num_nursemidwives_fulltime" = "num_nursemidwives_posted",
                             "num_chews_fulltime" = "num_chews_posted",
                             "num_jr_chews_fulltime" = "num_junior_chews_posted",
                             "num_chos_fulltime" = "num_cho_posted",
                             "num_nurses_fulltime" = "num_nurses_posted",
                             "num_lab_techs_fulltime" = "lab_technicians_posted",
                             "medication_arvs" = "medication.arvs",
                             "medication_tb_medicines" = "medication.tb_medicines",
                             "medication_none" = "medication.none"
                             ))

####################################
#Adding/subtracting a few vars 
####################################

#661
h_661$facility_owner_manager <- as.character(ifelse(h_661$facility_owner_manager.federalgovernment,
                                                    "federalgovernment",
                                             ifelse(h_661$facility_owner_manager.stategovernment,
                                                    "stategovrenment",
                                             ifelse(h_661$facility_owner_manager.lga,
                                                    "lga",
                                             ifelse(h_661$facility_owner_manager.private_forprofit,
                                                    "private_forprofit",
                                             ifelse(h_661$facility_owner_manager.charitable_ngo,
                                                    "private_notforprofit",
                                             ifelse(h_661$facility_owner_manager.religious_org,
                                                     "church_mission",
                                                          NA_character_)))))))

h_661$emoc_antibiotics <- as.logical(recodeVar(h_661$emoc_antibiotics_yn, 
                                               c('yes', 'no'),
                                               c(TRUE, FALSE)))   

h_661 <- subset(h_661, select=-c(facility_owner_manager.private_forprofit, facility_owner_manager.charitable_ngo,
                                 facility_owner_manager.religious_org, facility_owner_manager.stategovernment,
                                 facility_owner_manager.lga, facility_owner_manager.none, emoc_antibiotics_yn,
                                 facility_owner_manager_other, facility_owner_manager.federalgovernment))                       

h_661$power_sources_grid <- (h_661$grid_proximity == 'connected_to_grid' | 
                               h_661$local_grid_proximity == 'connected_to_local_grid')


#113
h_113$facility_owner_manager <- recodeVar(h_113$facility_owner_manager,
                                          c('federalgovrenment'),
                                          c('federalgovernment'))

##################################
#### combining 661, 113 & pilot
#################################
h_total <- rbind.fill(h_661, h_113, h_pilot)

###############################################
####mapping values and standardize the type####
###############################################

yes_no_columns <- c("private_yn", "road_yn", "all_weather_road_yn", "generator_funct_yn", 
                    "solar_funct_yn", "grid_funct_yn", "vaccine_storage_yn", "toilets_yn", 
                    "flush_improved_functional_yn", "flush_unimproved_functional_yn", 
                    "vip_latrine_functional_yn", "slab_pit_latrine_functional_yn", 
                    "open_pit_latrine_functional_yn", "bucket_system_functional_yn", 
                    "placenta_pit_yn", "sharps_separated_yn", "midwivery_service_scheme_yn", 
                    "delivery_services_yn", "delivery_skilled_birth_247_yn", "delivery_staff_csection_yn", 
                    "facility_open_247_yn", "staff_quarters_yn", "staff_paid_1mths_yn", 
                    "staff_paid_3mths_yn", "tb_treatment_yn", "antenatal_care_yn", 
                    "emergency_obstetrics_yn", "emoc_uterotonics_yn", "emoc_parenteral_anticonvulsant_yn", 
                    "emoc_vacuum_extractor_yn", "emoc_sba_placenta_removal_yn", "c_section_yn", 
                    "family_planning_yn", "inpatient_care_yn", "laboratory_yn", "laboratory_functional_yn", 
                    "staff_outreach_particip_yn", "thermometer_funct_yn", "weighing_scale_funct_yn", 
                    "bp_machine_funct_yn", "sterilizer_funct_yn", "xray_funct_yn", 
                    "ultra_sound_funct_yn", "aspirator_funct_yn", "extractor_funct_yn", 
                    "forceps_funct_yn", "antishock_funct_yn", "oxygen_funct_yn", 
                    "ambubag_funct_yn", "neonatal_mask_funct_yn", "iv_kits_funct_yn", 
                    "suction_funct_yn", "antibiotics_oral_stockout_yn", "antibiotics_musc_stockout_yn", 
                    "antibiotics_iv_stockout_yn", "iv_fliud_stockout_yn", "ort_stockout_yn", 
                    "uterotonics_stockout_yn", "antidiarrheal_stockout_yn", "antipyretics_stockout_yn", 
                    "act_stockout_yn", "sulphadoxine_stockout_yn", "arvs_stockout_yn", 
                    "nevirapine_stockout_yn", "azt_stockout_yn", "tb_meds_stockout_yn", 
                    "sedatives_stockout_yn", "antihistamines_stockout_yn", "anticonvulsants_stockout_yn", 
                    "oral_contacept_stockout_yn", "inject_contacept_stockout_yn", 
                    "implants_stockout_yn", "iud_stockout_yn", "zinc_stockout_yn", 
                    "iron_stockout_yn", "folic_acid_stockout_yn", "vitamin_a_stockout_yn", 
                    "bcg_immun_stockout_yn", "opv_immun_stockout_yn", "measles_immun_stockout_yn", 
                    "dpt_immun_stockout_yn", "yfever_immun_stockout_yn", "csm_immun_stockout_yn", 
                    "hepb_immun_stockout_yn", "tetanus_immun_stockout_yn", "adult_tx_fees_yn", 
                    "child_tx_fees_yn", "vaccines_stored_yn", "gender_separated_toilets_yn", 
                    "separate_toilets_for_staff_yn", "med_waste_separated_yn", "landline_funct_yn", 
                    "mobile_phone_funct_yn", "mobile_signal_funct_yn", "computer_funct_yn", 
                    "internet_funct_yn", "printer_funct_yn", "staff_quarters_sufficient_yn", 
                    "staff_paid_lastmth_yn", "staff_paid_6mths_yn", "primary_routine_care_yn", 
                    "malaria_treatment_yn", "sti_treatment_yn", "hiv_treatment_yn", 
                    "child_health_yn", "comprehensive_obstetrics_yn", "inpatient_care_enough_beds_yn", 
                    "surgical_theatre_funct_yn", "refrigerator_funct_yn", "public_transport_funct_yn", 
                    "public_transport_shared_yn", "antimalarials_stockout_yn", "antimalarials_expired_yn", 
                    "antidiarrheal_expired_yn", "antipyretics_expired_yn", "arvs_expired_yn", 
                    "tb_meds_expired_yn", "antibiotics_stockout_yn", "antibiotics_expired_yn", 
                    "iron_tablets_stockout_yn", "iron_tablets_expired_yn", "folic_acid_expired_yn", 
                    "iv_fliud_expired_yn", "cough_meds_stockout_yn", "cough_meds_expired_yn", 
                    "antihistamines_expired_yn", "oral_contracept_stockout_yn", "oral_contracept_expired_yn", 
                    "injectables_stockout_yn", "injectables_expired_yn", "oxytocin_stockout_yn", 
                    "oxytocin_expired_yn", "bednets_stockout_yn", "condoms_stockout_yn", 
                    "syringes_stockout_yn", "gloves_stockout_yn")

h_total <- yes_no_batch(h_total, yes_no_columns)





#list of variables used..
# newname_pilot <- c("child_health_bcg_immunization_calc", "child_health_dpt_immunization_calc", 
#                    "child_health_dpt_immunization_calc", "child_health_tetanus_immun_calc", 
#                    "child_health_hepb_immunization_calc", "child_health_yellow_fever_immun_calc",
#                    "vaccines_fridge_freezer", "child_health_csm_immunization_calc", 
#                    "improved_water_supply", "family_planning_pill_calc_calc", 
#                    "family_planning_injectables_calc_calc", "family_planning_iud_calc",
#                    "family_planning_implants_calc", "child_health_measles_immun_calc", 
#                    "child_health_opv_immuization_calc", "sterilization_yn_calc", 
#                    "emergency_transport_currently_functioning", "lab_tests_stool_calc", 
#                    "sulpha_and_antenatal", "lab_tests_pregnancy_calc", "essential_meds_stockout",
#                    "hiv_tx_srvcs_pmtct_services_calc", "delivery_services_yn", "has_itns", 
#                    "power_access_and_functional", "maternal_health_delivery_services_24_7",
#                    "lab_tests_hemoglobin_testing_calc", "iv_antibiotics_yn_calc", "condoms_yn", 
#                    "lab_tests_urine_testing", "lab_tests_hiv_testing_calc", "malaria_testing",
#                    "improved_sanitation", "lab_tests_tb_microscopy_calc", "health_no_user_fees",
#                    #lga level stuff...  
#                    "emoc_available_24_7", "paid_services_anc_delivery", 
#                    "compr_oc_available_24_7", "paid_services_malaria_treatment", 
#                    "paid_services_child_health")

#droppin from pilot
# paid_services_inpatient_stay, paid_services_lab_testing, paid_services_routine_visit
# days_no_electricity, antibiotics_stockout_yn, antimalarials_stockout_yn, equipment_emergency_transport
# emoc_antibiotics, comprehensive_obstetrics_yn, lab_tests_hemoglobin_testing, malaria_treatment_yn
# supplies_available_bednets,  malaria_treatment_srvcs_itn, malaria_treatment_sulphadoxine
# family_planning_pill, family_planning_iud, lab_tests_malaria_microscopy, lab_tests_malaria_rdt
# family_planning_implants, family_planning_sterilization_m, child_health_immunization_p,
# vaccines_stored_yn, water_sources_yn_p, num_toilets_improved_p, lab_tests_tb_microscopy
# lab_tests_hiv_testing_calc,lab_tests_pregnancy, sti_tx_srvcs_condoms, hiv_tx_srvcs_condoms
# supplies_available_condoms, sti_treatment_yn, lab_tests_urine_testing, power_sources_generator
# generator_funct_yn, power_sources_solar, solar_funct_yn, power_sources_grid, grid_funct_yn
# daily_pub_transport_p, paid_services_hiv_treatment, paid_services_tb_treatment, paid_services_contraceptives




  
  
  
  