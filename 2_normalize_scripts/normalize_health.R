######################################################################################################################
##Normalizing Health Data: 661, 113, Pilot 

source("source_scripts/Normailize_Functions.R")
source("source_scripts/NMIS_Functions.R")
source("source_scripts/NMIS_Utils.R")

#reading in data
h_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Health_661_Merged.csv", 
                  stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", "-8"))
h_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Health_PhII_RoundI&II&III_Clean_2011.10.21.csv",
                  stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", "-8"))
h_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Data_Health_Clean_2011.11.18.csv",
                    stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999", "-8"))

#adding surveying source column
h_661$src <- "661"
h_113$src <- "113"
h_pilot$src <- "pilot"

#adding uuid to 113 + pilot
h_113$uuid <- sapply(paste(h_113$gps, h_113$photo), FUN=digest)
h_pilot$uuid <- sapply(paste(h_pilot$gps, h_pilot$photo), FUN=digest)

########################
#Mapping Names

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
                         "power_sources.generator" = "power_sources_generator",
                         "sharps_separated_yn"= "med_waste_separated_yn",
                         "power_sources.none" = "power_sources_none"))

#113
h_113 <- rename(h_113, c("lga" = "mylga",
                         "state" = "mylga_state",
                         "zone" = "mylga_zone",
                         "start_time" = "start",
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
                         "emoc_needles_tubing" = "supplies.needles_and_tubing",
                         "medication_oral_contraceptives" = "medication.oral_contraceptives",
                         "medication_injectable_contracept" = "medication.injectable_contracept",
                         "lab_tests_malaria_rdt" = "lab_tests.malaria_rdt",
                         "lab_tests_malaria_microscopy" = "lab_tests.malaria_microscopy",
                         "medication_antihistamines" = "medication.antihistamines",
                         "medication_anti_diarrheals" = "medication.anti_diarrheals",
                         "medication_anti_pyretics" = "medication.anti_pyretics",
                         "medication_iv_fluid" = "medication.iv_fluid",
                         "medication_arvs" = "medication.arvs",
                         "medication_tb_medicines" = "medication.tb_medicines",
                         "medication_none" = "medication.none"))

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
                             "medication_none" = "medication.none",
                             "start_time" = "start"))

###############################
#Adding/subtracting a few vars 

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

h_661$emoc_antibiotics <- as.logical(revalue(h_661$emoc_antibiotics_yn, 
                                               c("yes" = TRUE, 
                                                 "no" = FALSE)))
                                             
h_661 <- subset(h_661, select=-c(facility_owner_manager.private_forprofit, facility_owner_manager.charitable_ngo,
                                 facility_owner_manager.religious_org, facility_owner_manager.stategovernment,
                                 facility_owner_manager.lga, facility_owner_manager.none, emoc_antibiotics_yn,
                                 facility_owner_manager_other, facility_owner_manager.federalgovernment))                       

h_661$power_sources_grid <- (h_661$grid_proximity == 'connected_to_grid' | 
                               h_661$local_grid_proximity == 'connected_to_local_grid')

h_661$toilets_yn <- as.logical(revalue(h_661$toilets_yn,
                                c('no_toilets_available' = FALSE,
                                  'toilets_available' = TRUE)))

h_661$vaccine_storage_yn <- as.logical(revalue(h_661$vaccine_storage_yn,
                                c("yes" = TRUE,
                                  "no_stored_other_palce" = FALSE,
                                  "no_vaccines_not_offered" = FALSE)))


h_661$child_health_growth_monitor <-  (h_661$weighing_scale_funct_yn == 'yes') & 
                                        h_661$equipment.scale &
                                          h_661$supplies.muac_tape



h_661$power_sources_solar_functional <- ((h_661$power_sources_solar &
                                          h_661$solar_funct_yn == 'yes') &  
                                        (h_661$power_sources_none == F))

h_661$power_sources_generator_functional <- ((h_661$power_sources_generator & 
                                               h_661$generator_funct_yn == 'yes') & 
                                             (h_661$power_sources_none == F))

h_661$power_sources_alternative_functional <- (h_661$power_sources_generator_functional |
                                                h_661$power_sources_solar_functional)

h_661$access_to_alternative_power_source <- (h_661$power_sources_generator | h_661$power_sources_solar)


  
#113
h_113$medication.antibiotic_oral <- ((h_113$sti_tx_srvcs_penicilling | h_113$sti_tx_srvcs_doxycycline | 
                                      h_113$sti_tx_srvcs_ciprofloxacin) & (h_113$sti_treatment_yn == 'yes')) |
                                    ((h_113$child_health_ampicillin | h_113$child_health_ciprofloxain) & 
                                       h_113$child_health_yn == 'yes') | (h_113$medication_anti_biotics)

h_113$facility_owner_manager <- revalue(h_113$facility_owner_manager,
                                          c('federalgovrenment'='federalgovernment'))

facility_type_value <- c('healthpostdispensary', 'healthpostdispensary',
                         'federalmedicalcentre', 'wardmodelphccentre')

names(facility_type_value) <- c('healthpost','dispensary',
                                'federalmedicalcare', 'wardmodelprimaryhealthcarecentre')

h_113$facility_type <- revalue(h_113$facility_type, facility_type_value)

h_113$transport_to_referral <-  ifelse(h_113$emergency_transport_ambulance,
                                       "ambulance",
                                  ifelse(h_113$transport_to_referral_taxi,
                                           "taxi",
                                    ifelse(h_113$emergency_transport_keke_napep,
                                              "keke", NA)))

h_113$medication.iv_fluid <- h_113$medication.iv_fluid | 
                              ((h_113$supplies.needles_and_tubing & h_113$emergency_obstetrics_yn == 'yes') | 
                              (h_113$emoc_parenteral1 | h_113$emoc_antibiotics))

h_113$lab_tests.hemoglobin_testing <- h_113$lab_tests_hemoglobin_testing & h_113$laboratory_yn == 'yes' 
h_113$lab_tests.urine_testing <- h_113$lab_tests_urine_testing & h_113$laboratory_yn == 'yes' 
h_113$lab_tests.hiv_testing <- h_113$lab_tests_hiv_testing & h_113$laboratory_yn == 'yes'
h_113$lab_tests.tb_microscopy <- h_113$lab_tests_tb_microscopy & h_113$laboratory_yn == 'yes'
h_113$lab_tests_pregnancy_calc <- h_113$lab_tests_pregnancy & h_113$laboratory_yn == 'yes'
h_113$medication.iud <- h_113$family_planning_iud & h_113$family_planning_yn == 'yes'
h_113$medication.implants <- h_113$family_planning_implants & h_113$family_planning_yn == 'yes'
h_113$immunization.measles_immun <- h_113$child_health_measles_immun & h_113$child_health_yn == 'yes'
h_113$immunization.opv_immuization <- h_113$child_health_opv_immuization & h_113$child_health_yn == 'yes'
h_113$immunization.dpt_immunization <- h_113$child_health_dpt_immunization & h_113$child_health_yn == 'yes'  
h_113$immunization.tetanus_immun <- h_113$child_health_tetanus_immun & h_113$child_health_yn == 'yes' 
h_113$immunization.hepb_immunization <- h_113$child_health_hepb_immunization & h_113$child_health_yn == 'yes'
h_113$immunization.bcg_immunization <-  h_113$child_health_bcg_immunization & h_113$child_health_yn == 'yes' 
h_113$immunization.yellow_fever_immun <- h_113$child_health_yellow_fever_immun  & h_113$child_health_yn == 'yes'
h_113$immunization.csm_immunization <- h_113$child_health_csm_immunization == T & h_113$child_health_yn == 'yes'

h_113$supplies.insecticide_treated_bednets <- h_113$malaria_treatment_yn == 'yes' & 
                                                (h_113$malaria_treatment_srvcs_itn | 
                                                  h_113$supplies_available_bednets)

h_113$equipment.emoc_antishock_garment <- ((h_113$emoc_antishock_garment & h_113$emoc_enough_antishock_garment) & 
                                             h_113$emergency_obstetrics_yn == 'yes') | 
                                          ((h_113$emoc_antishock_garment & h_113$compr_oc_antishock_garment) & 
                                             h_113$comprehensive_obstetrics_yn == 'yes')

h_113$supplies.condoms <- h_113$sti_tx_srvcs_condoms | h_113$hiv_tx_srvcs_condoms | h_113$supplies_available_condoms

h_113$improved_sanitation_and_functional <- (h_113$toilet_types_vip_latrine | 
                                               h_113$toilet_types_pit_w_slab | 
                                               (h_113$num_flush_or_pour_flush_piped & 
                                                  h_113$flush_toilet_drain_to == 'improved')) & 
                                            ((h_113$toilet_types_vip_latrine & 
                                                (h_113$vip_latrine_not_working < 7)) | 
                                               (h_113$toilet_types_pit_w_slab & 
                                                  (h_113$slab_pit_latrine_not_working) < 7) | 
                                               (h_113$num_flush_or_pour_flush_piped & 
                                                  h_113$flush_toilet_drain_to == 'improved' & 
                                                  (h_113$flush_toilet_not_working < 7)))
h_113$access_to_alternative_power_source <- h_113$power_sources_generator |
    h_113$power_sources_solar
h_113$power_access_and_functional <- (((h_113$power_sources_generator &
                                          h_113$generator_funct_yn == 'yes') |
                                         (h_113$power_sources_solar &
                                            h_113$solar_funct_yn == 'yes') |
                                         (h_113$power_sources_grid &
                                            h_113$grid_funct_yn == 'yes')) &
                                        (h_113$days_no_electricity <= 7))

h_113$power_sources_solar_functional <- ((h_113$power_sources_solar &
                                            h_113$solar_funct_yn == 'yes') & 
                                        (h_113$days_no_electricity <= 7))

h_113$power_sources_generator_functional <- ((h_113$power_sources_generator &
                                              h_113$generator_funct_yn == 'yes') &
                                        (h_113$days_no_electricity <= 7))

h_113$power_sources_alternative_functional <- (h_113$power_sources_generator_functional |
                                                 h_113$power_sources_solar_functional)
h_113$compr_oc_c_sections <- h_113$comprehensive_obstetrics_yn == 'yes' &
                                h_113$compr_oc_c_sections


#pilot
h_pilot$improved_sanitation <- h_pilot$num_toilets_improved_p > 0 

h_pilot$family_planning_iud <- as.logical(recodeVar(h_pilot$family_planning_iud,
                                         c('', 'yes'),
                                         c(FALSE, TRUE)))

facility_type_value <- c('healthpostdispensary', 'healthpostdispensary',
                         'wardmodelphccentre')
names(facility_type_value) <-  c('healthpost','dispensary',
                                 'wardmodelprimaryhealthcarecentre')

h_pilot$facility_type <- revalue(h_pilot$facility_type, facility_type_value)

h_pilot$transport_to_referral <-  ifelse(h_pilot$transport_to_referral_ambulance,
                                          "ambulance",
                                    ifelse(h_pilot$transport_to_referral_taxi,
                                            "taxi",
                                      ifelse(h_pilot$transport_to_referral_keke,
                                              "keke", NA)))
h_pilot$access_to_alternative_power_source <- h_pilot$power_sources_generator |
    h_pilot$power_sources_solar


h_pilot$lab_tests.hemoglobin_testing <- h_pilot$lab_tests_hemoglobin_testing & h_pilot$laboratory_yn == 'yes' 
h_pilot$lab_tests.urine_testing <- h_pilot$lab_tests_urine_testing & h_pilot$laboratory_yn == 'yes'
h_pilot$lab_tests.hiv_testing <- h_pilot$lab_tests_hiv_testing & h_pilot$laboratory_yn == 'yes'
h_pilot$lab_tests.tb_microscopy <- h_pilot$lab_tests_tb_microscopy & h_pilot$laboratory_yn == 'yes'
h_pilot$lab_tests_pregnancy_calc <- h_pilot$lab_tests_pregnancy & h_pilot$laboratory_yn == 'yes'   
h_pilot$supplies.condoms <- h_pilot$sti_tx_srvcs_condoms | h_pilot$hiv_tx_srvcs_condoms | h_pilot$supplies_available_condoms
h_pilot$medication.iud <- h_pilot$family_planning_iud & h_pilot$family_planning_yn == 'yes'  
h_pilot$medication.implants <- h_pilot$family_planning_implants & h_pilot$family_planning_yn == 'yes'
h_pilot$immunization.measles_immun <- h_pilot$child_health_immunization_p & h_pilot$child_health_yn == 'yes'  
h_pilot$immunization.opv_immuization <- h_pilot$child_health_immunization_p & h_pilot$child_health_yn == 'yes'
h_pilot$immunization.dpt_immunization <- h_pilot$child_health_immunization_p & h_pilot$child_health_yn == 'yes'
h_pilot$immunization.tetanus_immun <- h_pilot$child_health_immunization_p & h_pilot$child_health_yn == 'yes'
h_pilot$immunization.hepb_immunization <- h_pilot$child_health_immunization_p & h_pilot$child_health_yn == 'yes'
h_pilot$immunization.bcg_immunization  <- h_pilot$child_health_immunization_p & h_pilot$child_health_yn == 'yes'
h_pilot$immunization.yellow_fever_immun <- h_pilot$child_health_immunization_p & h_pilot$child_health_yn == 'yes'
h_pilot$immunization.csm_immunization <- h_pilot$child_health_immunization_p & h_pilot$child_health_yn == 'yes'

h_pilot$delivery_services_yn <- as.logical(revalue(h_pilot$emergency_obstetrics_yn,
                                                     c('yes' = TRUE, 'no' = FALSE))) 

h_pilot$supplies.insecticide_treated_bednets <- (h_pilot$malaria_treatment_yn == 'yes' & 
                                                   (h_pilot$malaria_treatment_srvcs_itn == 'yes' |
                                                      h_pilot$supplies_available_bednets))

h_pilot$antenatal_care_malaria_prlx <- ifelse((h_pilot$malaria_treatment_sulphadoxine & 
                                                  h_pilot$antenatal_care_yn == 'yes'),
                                                    TRUE, NA)

h_pilot$power_access_and_functional <- (((h_pilot$power_sources_generator &
                                        h_pilot$generator_funct_yn == 'yes') |
                                           (h_pilot$power_sources_solar &
                                              h_pilot$solar_funct_yn == 'yes') |
                                           (h_pilot$power_sources_grid &
                                              h_pilot$grid_funct_yn == 'yes')) &
                                          (h_pilot$days_no_electricity <= 7))

h_pilot$power_sources_solar_functional <- ((h_pilot$power_sources_solar &
                                              h_pilot$solar_funct_yn == 'yes') &
                                          (h_pilot$days_no_electricity <= 7))

h_pilot$power_sources_generator_functional <- ((h_pilot$power_sources_generator &
                                                h_pilot$generator_funct_yn == 'yes') &
                                              (h_pilot$days_no_electricity <= 7))

h_pilot$power_sources_alternative_functional <- (h_pilot$power_sources_generator_functional |
                                                  h_pilot$power_sources_solar_functional)


##################################
##combining 661, 113 & pilot
health_total <- rbind.fill(h_661, h_113, h_pilot)

###############################################
##mapping values and standardize the type

yes_no_columns <- c("private_yn", "road_yn", "all_weather_road_yn", "generator_funct_yn", "medication_folic_acid",
                    "solar_funct_yn", "grid_funct_yn", "oxygen_funct_yn",  
                    "flush_improved_functional_yn", "flush_unimproved_functional_yn", "malaria_treatment_artemisinin",
                    "vip_latrine_functional_yn", "slab_pit_latrine_functional_yn", "syringes_stockout_yn",
                    "open_pit_latrine_functional_yn", "bucket_system_functional_yn", "zinc_stockout_yn",
                    "placenta_pit_yn", "med_waste_separated_yn", "midwivery_service_scheme_yn",
                    "delivery_services_yn", "delivery_skilled_birth_247_yn", "delivery_staff_csection_yn", 
                    "facility_open_247_yn", "staff_quarters_yn", "staff_paid_1mths_yn", "antenatal_care_malaria_prlx",
                    "staff_paid_3mths_yn", "tb_treatment_yn", "antenatal_care_yn", "antishock_funct_yn",
                    "emergency_obstetrics_yn", "emoc_uterotonics_yn", "emoc_parenteral_anticonvulsant_yn", 
                    "emoc_vacuum_extractor_yn", "emoc_sba_placenta_removal_yn", "c_section_yn", 
                    "family_planning_yn", "inpatient_care_yn", "laboratory_yn", "laboratory_functional_yn", 
                    "staff_outreach_particip_yn", "thermometer_funct_yn", "weighing_scale_funct_yn", 
                    "bp_machine_funct_yn", "sterilizer_funct_yn", "xray_funct_yn", "gloves_stockout_yn",
                    "ultra_sound_funct_yn", "aspirator_funct_yn", "extractor_funct_yn", "compr_oc_blood_transfusions",
                    "ambubag_funct_yn", "neonatal_mask_funct_yn", "iv_kits_funct_yn", "iud_stockout_yn",
                    "suction_funct_yn", "antibiotics_oral_stockout_yn", "antibiotics_musc_stockout_yn", 
                    "antibiotics_iv_stockout_yn", "iv_fliud_stockout_yn", "ort_stockout_yn", "emoc_available_24_7",
                    "uterotonics_stockout_yn", "antidiarrheal_stockout_yn", "antipyretics_stockout_yn", 
                    "act_stockout_yn", "sulphadoxine_stockout_yn", "arvs_stockout_yn", "family_planning_pill",
                    "nevirapine_stockout_yn", "azt_stockout_yn", "tb_meds_stockout_yn", "family_planning_injectables",
                    "sedatives_stockout_yn", "antihistamines_stockout_yn", "anticonvulsants_stockout_yn", 
                    "oral_contacept_stockout_yn", "inject_contacept_stockout_yn", "implants_stockout_yn",
                    "iron_stockout_yn", "folic_acid_stockout_yn", "vitamin_a_stockout_yn", "family_planning_sterilization_m",
                    "bcg_immun_stockout_yn", "opv_immun_stockout_yn", "measles_immun_stockout_yn", 
                    "dpt_immun_stockout_yn", "yfever_immun_stockout_yn", "csm_immun_stockout_yn", "vaccines_stored_yn",  
                    "hepb_immun_stockout_yn", "tetanus_immun_stockout_yn", "adult_tx_fees_yn", "gender_separated_toilets_yn",
                    "separate_toilets_for_staff_yn", "med_waste_separated_yn", "landline_funct_yn", "child_tx_fees_yn", 
                    "mobile_phone_funct_yn", "mobile_signal_funct_yn", "computer_funct_yn", "family_planning_sterilization_f", 
                    "internet_funct_yn", "printer_funct_yn", "staff_quarters_sufficient_yn", "water_sources_yn_p",
                    "staff_paid_lastmth_yn", "staff_paid_6mths_yn", "primary_routine_care_yn", "compr_oc_available_24_7",
                    "malaria_treatment_yn", "sti_treatment_yn", "hiv_treatment_yn", "forceps_funct_yn",
                    "child_health_yn", "comprehensive_obstetrics_yn", "inpatient_care_enough_beds_yn", 
                    "surgical_theatre_funct_yn", "refrigerator_funct_yn", "public_transport_funct_yn", 
                    "public_transport_shared_yn", "antimalarials_stockout_yn", "antimalarials_expired_yn", 
                    "antidiarrheal_expired_yn", "antipyretics_expired_yn", "arvs_expired_yn", "daily_pub_transport_p",
                    "tb_meds_expired_yn", "antibiotics_stockout_yn", "antibiotics_expired_yn", 
                    "iron_tablets_stockout_yn", "iron_tablets_expired_yn", "folic_acid_expired_yn", 
                    "iv_fliud_expired_yn", "cough_meds_stockout_yn", "cough_meds_expired_yn", 
                    "antihistamines_expired_yn", "oral_contracept_stockout_yn", "oral_contracept_expired_yn", 
                    "injectables_stockout_yn", "injectables_expired_yn", "oxytocin_stockout_yn", 
                    "oxytocin_expired_yn", "bednets_stockout_yn", "condoms_stockout_yn")
     
numeric_column_list <- c("medical_records_officers_active", "medical_records_officers_posted", "pharmacists_active",  
                         "pharmacists_posted", "pharma_technicians_active", "pharma_technicians_posted",
                         "lab_technicians_active", "environmental_health_officers_posted", "num_pit_w_slab",
                         "num_flush_or_pour_flush_piped", "num_flush_other", "num_vip_latrine", "days_no_electricity",
                         "num_open_pit_latrine", "num_bucket_system", "num_toilets_notimproved_p",
                         "num_doctors_posted", "num_midwives_posted", "num_nurses_posted", "generator_months_broken",
                         "num_nursemidwives_posted", "num_cho_posted", "num_chews_posted", "grid_months_broken",
                         "num_junior_chews_posted", "num_doctors_active", "num_midwives_active", 
                         "num_nurses_active", "num_nursemidwives_active", "num_cho_active", "contraceptives_price",
                         "num_chews_active", "num_junior_chews_active", "num_pharm_techs_fulltime", 
                         "num_med_assistants_fulltime", "num_med_rcrds_officers_fulltime", "km_to_referral_facility",
                         "num_facilities", "num_midwives_fulltime", "num_toilets_improved_p", "days_no_potable_water_pastmth")

#logical type conversion and ASSERTION
health_total <- yes_no_batch(health_total, yes_no_columns)
check_type <- batch_type(health_total, yes_no_columns)
stopifnot(all(check_type %in% c("logical")))

#numeric type conversion and ASSERTION
health_total <- numeric_batch(health_total, numeric_column_list)
check_type <- batch_type(health_total, numeric_column_list)
stopifnot(all(check_type %in% c("integer", "numeric")))

###################
##output 

## final cleaning remove lga_id = NA and duplicated UUID rows
health_total <- subset(health_total, !(duplicated(health_total$uuid) | is.na(health_total$lga_id)))

lgas <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv")
lgas <- subset(lgas, select=-c(latitude, longitude))

health_total <- merge_non_redundant(lgas, health_total, by="lga_id")
saveRDS(health_total, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/normalized/Health_774_normalized.rds")

