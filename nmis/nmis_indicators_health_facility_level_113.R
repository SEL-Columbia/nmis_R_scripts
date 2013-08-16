#Health 113: facility level
setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
source("~/Code/nmis_R_scripts/base_scripts/InstallFormhub.R")
source("~/Code/nmis_R_scripts/source_scripts/NMIS_Functions.R")

#######
##113##
#######
#importing data
h_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Health_113_outliercleaned.csv")

h <- h_113
hh <- subset(h_113, select=c("uuid", "lga_id", "photo", "X_id", "facility_name", "facility_type", 
                             "facility_open_247_yn", "comprehensive_obstetrics_yn", "num_nurses_fulltime", 
                             "num_nursemidwives_fulltime", "num_doctors_fulltime", "num_lab_techs_fulltime", 
                             "staff_paid_lastmth_yn", "medication_iron_tablets", "medication_folic_acid", 
                             "equipment_bp_machine", "mobile_signal_funct_yn", "emoc_vacuum_extractor", 
                             "emoc_forceps", "compr_oc_c_sections", "emoc_parenteral_anticonvulsant", 
                             "compr_oc_blood_transfusions", "child_health_growth_monitor", 
                             "equipment_refrigerator", "malaria_treatment_artemisinin", "malaria_treatment_sulphadoxine", 
                             "paid_services_malaria_treatment", "medication_anti_malarials", 
                             "medication_antihistamines", "medication_iv_fluid", "tb_treatment_yn", 
                             "hiv_treatment_yn", "inpatient_care_yn"))
#ID basic stuff...
hh$formhub_photo_id <- h$photo
hh$mylga_zone <- h$zone
hh$mylga_state <- h$state
hh$mylga <- h$lga
hh$gps <- h$geocodeoffacility
#####################
####SNAPSHOT###
###################
hh$facility_name <- h$facility_name
hh$facility_type <- h$facility_type
hh$owner_manager <- ifelse(h$facility_owner_manager == 'lga', "public", 
                           ifelse(h$facility_owner_manager == 'federalgovrenment', "public", 
                                  ifelse(h$facility_owner_manager == 'stategovrenment', "public",
                                         ifelse(h$facility_owner_manager == 'community', "public",
                                                ifelse(h$facility_owner_manager == 'church_mission', "private", 
                                                       ifelse(h$facility_owner_manager == 'private_forprofit', "private",
                                                              ifelse(h$facility_owner_manager == 'private_notforprofit', "private",
                                                                     NA_character_)))))))
hh$maternal_health_delivery_services <- h$emergency_obstetrics_yn  == 'yes'
hh$skilled_birth_attendant <- (h$num_doctors_fulltime > 0 | 
  h$num_midwives_fulltime > 0 |
  h$num_nursemidwives_fulltime > 0)
hh$num_chews_and_chos <- rowSums(h[, c("num_chews_fulltime",
                                       "num_jr_chews_fulltime",
                                       "num_chos_fulltime")],na.rm=T)
hh$vaccines_fridge_freezer <- ifelse(h$vaccines_strg_type == "solar_refrigeration", T,
                                     ifelse(h$vaccines_strg_type == "grid_refrigeration", T,
                                            ifelse(h$vaccines_strg_type == "lpg_refrigeration", T,
                                                   ifelse(h$vaccines_strg_type == "", T,
                                                          NA_character_))))
hh$emergency_transport <- (h$emergency_transport_ambulance == T | h$emergency_transport_keke_napep == T)
hh$improved_water_supply <- (h$water_sources_borehole_tube_well == T |
  h$water_sources_tap_outside == T | 
  h$water_sources_tap_in_compound == T)
hh$improved_sanitation <- 
  (h$toilet_types_vip_latrine == T | 
  h$toilet_types_pit_w_slab == T | 
  h$toilet_types_flush_or_pour_flush == T) 
hh$phcn_electricity <- h$power_sources_grid ==T
###################
####################

hh$maternal_health_delivery_services_24_7 <- (h$emergency_obstetrics_yn=='yes' & 
                                                (h$compr_oc_available_24_7==T | h$emoc_available_24_7 == T))
hh$sulpha_and_antenatal <- (h$malaria_treatment_sulphadoxine == T & (h$antenatal_care_yn == "yes"))
hh$essential_meds_stockout <- (h$antimalarials_stockout_yn == 'yes' | h$antidiarrheal_stockout_yn == 'yes' | 
                                 h$antibiotics_stockout_yn == 'yes') 
hh$emergency_transport_currently_functioning <-  (h$equipment_emergency_transport == T & h$public_transport_funct_yn == 'yes')

hh$power_access_and_functional <- (((h$power_sources_generator == T &
                                       h$generator_funct_yn == 'yes') |
                                      (h$power_sources_solar == T &
                                         h$solar_funct_yn == 'yes') |
                                      (h$power_sources_grid == T &
                                         h$grid_funct_yn == 'yes')) &
                                     (as.numeric(h$days_no_electricity) <= 7))
hh$num_chews_total <- rowSums(h[,c('num_chews_fulltime', 
                                   'num_jr_chews_fulltime')], na.rm=T) 
hh$has_itns <- (h$malaria_treatment_yn == 'yes' & (h$malaria_treatment_srvcs_itn == T 
                                                   | h$supplies_available_bednets == T))
hh$scale_yn <- (h$equipment_scale == T | 
                  h$child_health_weighing_scale == T)  
hh$hiv_tx_srvcs_pmtct_services_calc <- (h$sti_treatment_yn == 'yes' &
                                          h$hiv_tx_srvcs_pmtct_services == T)
hh$lab_tests_hemoglobin_testing_calc <- (h$laboratory_yn == 'yes' &
                                           h$lab_tests_hemoglobin_testing == T)
hh$lab_tests_urine_testing_calc <- (h$laboratory_yn == 'yes' &
                                      h$lab_tests_urine_testing == T)
hh$iv_antibiotics_yn_calc <- (h$emoc_needles_tubing == T | 
                                (h$emoc_parenteral1 == T | h$emoc_antibiotics == T) & 
                                h$emergency_obstetrics_yn == 'yes') | 
  (h$comprehensive_obstetrics_yn == 'yes' & 
     h$emoc_antibiotics == T)                  
hh$uterotonics_yn_calc <- (h$medication_oxytocin == T |
                             ((h$emoc_uterotonic2 == T | h$emoc_oxytocin == T | h$emoc_misoprotol == T) &
                                h$emergency_obstetrics_yn == 'yes') | 
                             ((h$compr_oc_oxytocin == T |h$compr_oc_misoprotol == T) & 
                                h$comprehensive_obstetrics_yn == 'yes'))
hh$antishock_garment_yn <- ((h$emoc_antishock_garment == T & h$emoc_enough_antishock_garment == T) & 
                              h$emergency_obstetrics_yn == 'yes') | 
  ((h$emoc_antishock_garment == T & h$compr_oc_antishock_garment == T) & 
     h$comprehensive_obstetrics_yn == 'yes')  
hh$at_least_two_skilled_birth_attendants <- rowSums(h[, c('num_doctors_fulltime', 
                                                          'num_midwives_fulltime', 'num_nursemidwives_fulltime')], na.rm=T) >= 2  
hh$at_least_three_skilled_birth_attendants <- rowSums(h[, c('num_doctors_fulltime', 
                                                            'num_midwives_fulltime', 'num_nursemidwives_fulltime')], na.rm=T) >= 3  
hh$at_least_four_skilled_birth_attendants <- rowSums(h[, c('num_doctors_fulltime', 
                                                           'num_midwives_fulltime', 'num_nursemidwives_fulltime')], na.rm=T) >= 4    
hh$condoms_yn <- (h$sti_tx_srvcs_condoms == T | 
                    h$hiv_tx_srvcs_condoms == T | 
                    h$supplies_available_condoms == T)
hh$family_planning_pill_calc_calc <- (h$family_planning_pill == T  & h$family_planning_yn == 'yes') |
  (h$medication_oral_contraceptives == T)
hh$family_planning_injectables_calc_calc <- (h$family_planning_injectables == T  & h$family_planning_yn == 'yes') |
  (h$medication_injectable_contracept == T)
hh$family_planning_iud_calc <- (h$family_planning_iud == T & h$family_planning_yn == 'yes')  
hh$family_planning_implants_calc <- (h$family_planning_implants == T & h$family_planning_yn == 'yes')              
hh$sterilization_yn_calc <- (h$family_planning_sterilization_m == T & h$family_planning_yn == 'yes')                 
hh$child_health_measles_immun_calc <- (h$child_health_measles_immun == T & h$child_health_yn == 'yes')
hh$child_health_opv_immuization_calc <- (h$child_health_opv_immuization == T & h$child_health_yn == 'yes')         
hh$child_health_dpt_immunization_calc <- (h$child_health_dpt_immunization == T & h$child_health_yn == 'yes')          
hh$child_health_tetanus_immun_calc <- (h$child_health_tetanus_immun == T & h$child_health_yn == 'yes')            
hh$child_health_hepb_immunization_calc <- (h$child_health_hepb_immunization == T & h$child_health_yn == 'yes')        
hh$child_health_bcg_immunization_calc <-  (h$child_health_bcg_immunization == T & h$child_health_yn == 'yes')       
hh$child_health_yellow_fever_immun_calc <- (h$child_health_yellow_fever_immun == T & h$child_health_yn == 'yes')      
hh$child_health_csm_immunization_calc <- (h$child_health_csm_immunization == T & h$child_health_yn == 'yes')        
hh$vaccines_icepack_calc <- (h$vaccines_stored_yn == 'yes' & 
                               h$vaccines_strg_type == 'vaccine_carriers_icepacks') |
  (h$child_health_vaccine_carriers == T &
     h$child_health_yn == 'yes')
hh$malaria_testing <- (h$lab_tests_malaria_rdt == T| h$lab_tests_malaria_microscopy == T) & 
  (h$laboratory_yn == "yes")                           
hh$oral_antibiotics_calc <-  ((h$sti_tx_srvcs_penicilling == T | 
                                 h$sti_tx_srvcs_doxycycline == T | 
                                 h$sti_tx_srvcs_ciprofloxacin == T) & 
                                (h$sti_treatment_yn == 'yes')) |
  ((h$child_health_ampicillin == T | 
      h$child_health_ciprofloxain == T) & 
     h$child_health_yn == 'yes') | 
  (h$medication_anti_biotics == T)
hh$lab_tests_pregnancy_calc <- (h$lab_tests_pregnancy == T & h$laboratory_yn == 'yes')   
hh$lab_tests_stool_calc <- (h$lab_tests_pregnancy == T & h$laboratory_yn == 'yes')                         
hh$potable_water_access <- (as.numeric(h$days_no_potable_water_pastmth)< 23)

hh$improved_sanitation_and_functional <- (h$toilet_types_vip_latrine == T | h$toilet_types_pit_w_slab == T | 
                                            (h$toilet_types_flush_or_pour_flush == T & h$flush_toilet_drain_to == 'improved')) & 
  ((h$toilet_types_vip_latrine == T & (as.numeric(h$vip_latrine_not_working)< 7)) | 
     (h$toilet_types_pit_w_slab == T & (as.numeric(h$slab_pit_latrine_not_working)< 7)) | 
     (h$toilet_types_flush_or_pour_flush == T & 
        h$flush_toilet_drain_to == 'improved' & (as.numeric(h$flush_toilet_not_working)< 7)))

hh$lab_tests_tb_microscopy_calc <- (h$lab_tests_tb_microscopy == T & h$laboratory_yn == 'yes')               
hh$lab_tests_hiv_testing_calc <- (h$lab_tests_hiv_testing == T & h$laboratory_yn == 'yes') 
hh$health_no_user_fees <- (h$paid_services_routine_visit == T |
                             h$paid_services_lab_testing == T | h$paid_services_inpatient_stay == T |
                             h$paid_services_medication == T | h$paid_services_registration == T |
                             h$paid_services_routine_anc_visit == T | h$paid_services_contraceptives == T |
                             h$paid_services_anc_delivery == T | h$paid_services_child_health == T |
                             h$paid_services_immunization == T | h$paid_services_hiv_treatment == T |
                             h$paid_services_tb_treatment == T | h$paid_services_malaria_treatment == T)  
hh$iv_medications_yn <- h$medication_iv_fluid == T | 
  ((h$emoc_needles_tubing == T & h$emergency_obstetrics_yn == 'yes') | 
     (h$emoc_parenteral1 == T | h$emoc_antibiotics == T))

#writing out
write.csv(boundary_clean(hh,"mylga_state", "gps"), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Health_113_NMIS_Facility.csv", row.names=F)
write.csv(boundary_clean(cbind(h_113, hh), "mylga_state", "gps"), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Health_113_ALL_FACILITY_INDICATORS.csv", row.names=F)


