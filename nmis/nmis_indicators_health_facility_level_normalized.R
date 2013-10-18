source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")

#reading in data
h <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Health_661_outliercleaned.rds")
lga_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/661.csv")
##throw out all values from 113 LGAs that were resampled in 661
h <- merge(h, lga_661, by="lga_id")

#new data set
hh <- subset(h, select=c("uuid", "mylga", "mylga_state", "mylga_zone", "lga_id", "photo",
                         "unique_lga"))
h$`_id` <- h$uuid
h$formhub_photo_id <- h$photo
h$gps <- h_total$geocodeoffacility


nm_661 <- names(h)[! names(h) %in% names(hh)]
nm_661 <- c(nm_661, "uuid")
h_661_left <- subset(h, select=nm_661)
rm(nm_661)


####################
##### SNAPSHOT #####
####################
h$facility_name <- h_total$facility_name
h$facility_type <- h_total$facility_type
h$owner_manager <- ifelse(h_total$facility_owner_manager == 'lga', "public", 
                          ifelse(h_total$facility_owner_manager == 'federalgovrenment', "public", 
                                 ifelse(h_total$facility_owner_manager == 'stategovrenment', "public",
                                        ifelse(h_total$facility_owner_manager == 'community', "public",
                                               ifelse(h_total$facility_owner_manager == 'church_mission', "private", 
                                                      ifelse(h_total$facility_owner_manager == 'private_forprofit', "private",
                                                             ifelse(h_total$facility_owner_manager == 'private_notforprofit', "private",
                                                                    NA_character_)))))))
h$maternal_health_delivery_services <- ifelse(h_total$src != pilot,
                            h_total$delivery_services_yn == 'yes',
                                h_total$maternal_health_delivery_services)
h$skilled_birth_attendant <- 
  (rowSums(cbind(h_total$num_doctors_posted, 
                 h_total$num_nurses_posted,
                 h_total$num_nursemidwives_posted), na.rm=T) > 0)
h$num_chews_and_chos <-
  (rowSums(cbind(h_total$num_chews_posted,
                 h_total$num_junior_chews_posted,
                 h_total$num_cho_posted), na.rm=T))
h$vaccines_fridge_freezer <- ifelse(h_total$src != 'pilot',
  (h_total$vaccine_storage_type.refrigerator == T & 
     h_total$vaccine_storage_type.freezer == T ),
                 h_total$vaccines_fridge_freezer) 
h$emergency_transport <- 
  (h_total$transport_to_referral %in% c('ambulance', 'keke'))
h$improved_water_supply <- ifelse(h_total$src != 'pilot',
                            (h_total$water_sources.tap_in_compound == T | 
                              h_total$water_sources.tap_outside == T | 
                              h_total$water_sources.borehole_tube_well == T),
                                  h_total$improved_water_supply)
h$improved_sanitation <- ifelse(h_total$src != 'pilot',
  (h_total$num_vip_latrine > 0) | 
  (h_total$num_pit_w_slab > 0) | 
  (h_total$num_flush_or_pour_flush_piped > 0),
                        h_total$improved_sanitation)

h$phcn_electricity <- h_total$power_sources_grid == T

#######################
#########################

h$maternal_health_delivery_services_24_7 <- ifelse(h_total$src != "pilot",
                                                   (h_total$facility_open_247_yn == 'yes' & 
                                                      h_total$delivery_services_yn == 'yes' & 
                                                      h_total$delivery_skilled_birth_247_yn == 'yes'),
                                                   h$maternal_health_delivery_services_24_7)

h$facility_open_247_yn <- h_total$facility_open_247_yn == 'yes'   

h$essential_meds_stockout <- ifelse(h$total != "pilot",
                                    (h_total$antibiotics_oral_stockout_yn == 'yes' |
                                       h_total$antibiotics_musc_stockout_yn == 'yes' | 
                                       h_total$antibiotics_iv_stockout_yn == 'yes' |
                                       h_total$iv_fliud_stockout_yn == 'yes' |
                                       h_total$ort_stockout_yn == 'yes' |
                                       h_total$uterotonics_stockout_yn == 'yes' |
                                       h_total$antidiarrheal_stockout_yn == 'yes' |
                                       h_total$antipyretics_stockout_yn == 'yes' |
                                       h_total$act_stockout_yn == 'yes' |
                                       h_total$sulphadoxine_stockout_yn == 'yes' |
                                       h_total$arvs_stockout_yn == 'yes' |
                                       h_total$nevirapine_stockout_yn == 'yes' |
                                       h_total$azt_stockout_yn == 'yes' |
                                       h_total$tb_meds_stockout_yn == 'yes' |
                                       h_total$sedatives_stockout_yn == 'yes' |
                                       h_total$antihistamines_stockout_yn == 'yes' |
                                       h_total$anticonvulsants_stockout_yn == 'yes' |
                                       h_total$oral_contacept_stockout_yn == 'yes' |
                                       h_total$inject_contacept_stockout_yn == 'yes' |
                                       h_total$implants_stockout_yn == 'yes' |
                                       h_total$iud_stockout_yn == 'yes'),
                                    h_total$essential_meds_stockout)                


h$emergency_transport_currently_functioning <- ifelse(h_total$src != 'pilot',
                                                      (h_total$transport_to_referral != 'none'), 
                                                      h_total$emergency_transport_currently_functioning)

h$power_access_and_functional <- ifelse(h_total$src != "pilot",
                                        h_total$power_sources.none != F, 
                                        h_total$power_access_and_functional)

h$comprehensive_obstetrics_yn <- h_total$emergency_obstetrics_yn == 'yes' & h_total$c_section_yn == 'yes'

####################
##### STAFFING #####
####################

h$num_chews_total <- 
  (rowSums(cbind(h_total$num_chews_posted,
                 h_total$num_junior_chews_posted), na.rm=T))

h$num_nurses_fulltime <- h_total$num_nurses_posted

h$num_nursemidwives_fulltime <- 
  (rowSums(cbind(h_total$num_midwives_posted,
                 h_total$num_nursemidwives_posted), na.rm=T))

h$num_doctors_fulltime <- 
  h_total$num_doctors_posted

h$num_lab_techs_fulltime <- 
  h_total$lab_technicians_posted

h$staff_paid_lastmth_yn <-   
  (h_total$staff_paid_1mths_yn == 'yes' | 
     h_total$staff_paid_3mths_yn == 'yes')

###############################
##### M.HEALTH: ANTENATAL #####
###############################

h$sulpha_and_antenatal <- ifelse(h_total$src != "pilot",
                      h_total$antenatal_care_malaria_prlx == 'yes',
                            h_total$sulpha_and_antenatal)    

h$has_itns <- ifelse(h_total$src != "pilot",
                     h_total$supplies.insecticide_treated_bednets == T,
                     h_total$has_itns)

h$medication_iron_tablets <- h_total$supplements.iron == T

h$medication_folic_acid <- h_total$medication_folic_acid == 'yes'  

h$scale_yn <- (h_total$equipment.scale == T & 
                 h_total$weighing_scale_funct_yn == "yes")

h$equipment_bp_machine <- (h_total$equipment.bp_machine == T & 
                             h_total$bp_machine_funct_yn == "yes")   

h$hiv_tx_srvcs_pmtct_services_calc <- ifelse(h_total$src != "pilot",
                                             (h_total$medication.nevirapine == T | 
                                                h_total$medication.arvs == T |
                                                h_total$medication.azt == T),
                                             h_total$hiv_tx_srvcs_pmtct_services_calc)

h$lab_tests_hemoglobin_testing_calc <- ifelse(h_total$src != "pilot",
                                              h_total$lab_tests.hemoglobin_testing == T,
                                              h_total$lab_tests_hemoglobin_testing_calc)

h$lab_tests_urine_testing_calc <- ifelse(h_total$src != "pilot",
                                         h_total$lab_tests.urine_testing == T,
                                         h_total$lab_tests_urine_testing_calc)                    

##################################
##### M.HEALTH: OBSTETRICS 1 #####
##################################

h$mobile_signal_funct_yn <-  
  (h_total$phone_signal_strength == 'low_signal_strength' | 
     h_total$phone_signal_strength == 'high_signal_strength') &  
  h_total$info_tech_available.mobile_facility == T

h$iv_antibiotics_yn_calc <- ifelse(h_total$src != "pilot",
                                   (h_total$emoc_antibiotics == T | h_total$medication.antibiotic_iv == T),
                                   h_total$iv_antibiotics_yn_calc)

h$emoc_vacuum_extractor <- h_total$emoc_vacuum_extractor_yn == "yes"

h$emoc_forceps <- (h_total$equipment.emoc_forceps == T & 
                     h_total$forceps_funct_yn == "yes")

h$compr_oc_c_sections <- h_total$c_section_yn == 'yes' 

##################################
##### M.HEALTH: OBSTETRICS 2 #####
##################################

h$uterotonics_yn_calc <- h_total$emoc_uterotonics_yn == 'yes'   

h$antishock_garment_yn <- h_total$equipment.emoc_antishock_garment == T

h$emoc_parenteral_anticonvulsant <- h_total$emoc_parenteral_anticonvulsant_yn == 'yes'

#h$skilled_birth_attendant <- 
#       (rowSums(cbind(h_total$num_doctors_posted, 
#      h_total$num_nurses_posted,
#     h_total$num_nursemidwives_posted), na.rm=T) > 0)

h$at_least_two_skilled_birth_attendants <- 
  (rowSums(cbind(h_total$num_doctors_posted, 
                 h_total$num_nurses_posted,
                 h_total$num_nursemidwives_posted), na.rm=T) > 1)

h$at_least_three_skilled_birth_attendants <- 
  (rowSums(cbind(h_total$num_doctors_posted, 
                 h_total$num_nurses_posted,
                 h_total$num_nursemidwives_posted), na.rm=T) > 2)

h$at_least_four_skilled_birth_attendants <- 
  (rowSums(cbind(h_total$num_doctors_posted, 
                 h_total$num_nurses_posted,
                 h_total$num_nursemidwives_posted), na.rm=T) > 3)

h$compr_oc_blood_transfusions <- h_total$compr_oc_blood_transfusions == 'yes'

#####################################
##### M.HEALTH: FAMILY PLANNING #####
#####################################
h$condoms_yn <- ifelse(h_total$src != "pilot",
                       h_total$supplies.condoms == T,
                       h_total$condoms_yn)     

h$family_planning_pill_calc_calc <- ifelse(h_total$src != "pilot",
                                           h_total$family_planning_pill == 'yes',
                                           h_total$family_planning_pill_calc_calc)               

h$family_planning_injectables_calc_calc <- ifelse(h_total$src != "pilot",
                                                  h_total$family_planning_injectables == 'yes',
                                                  h_total$family_planning_injectables_calc_calc)

h$family_planning_iud_calc <- ifelse(h_total$src != "pilot",
                                     h_total$medication.iud == T,
                                     h_total$family_planning_iud_calc)                         

h$family_planning_implants_calc <- ifelse(h_total$src != "pilot",
                                          h_total$medication.implants == T,
                                          h_total$family_planning_implants_calc)

h$sterilization_yn_calc <- ifelse(h_total$src != "pilot",
                                  (h_total$family_planning_sterilization_m == 'yes' | 
                                     h_total$family_planning_sterilization_f == 'yes'),
                                  h_total$sterilization_yn_calc)

###########################
##### CHILD NUTRITION #####
###########################

h$child_health_growth_monitor <- 
  (h_total$weighing_scale_funct_yn == "yes" & 
     h_total$equipment.scale == T &
     h_total$supplies.muac_tape == T)

##############################
##### CHILD IMMUNIZATION #####
##############################

h$child_health_measles_immun_calc <- ifelse(h_total$src != "pilot",
                                            h_total$immunization.measles_immun,
                                            h_total$child_health_measles_immun_calc)

h$child_health_opv_immuization_calc <- ifelse(h_total$src != "pilot",
                                              h_total$immunization.opv_immuization,
                                              h_total$child_health_opv_immuization_calc)

h$child_health_dpt_immunization_calc <- ifelse(h_total$src != "pilot",
                                               h_total$immunization.dpt_immunization,
                                               h_total$child_health_dpt_immunization_calc)

h$child_health_tetanus_immun_calc <- ifelse(h_total$src != "pilot",
                                            h_total$immunization.tetanus_immun,
                                            h_total$child_health_tetanus_immun_calc)

h$child_health_hepb_immunization_calc <- ifelse(h_total$src != "pilot",
                                                h_total$immunization.hepb_immunization,
                                                h_total$child_health_hepb_immunization_calc)

h$child_health_bcg_immunization_calc <- ifelse(h_total$src != "pilot",
                                               h_total$immunization.bcg_immunization,
                                               h_total$child_health_bcg_immunization_calc)

h$child_health_yellow_fever_immun_calc <- ifelse(h_total$src != "pilot",
                                                 h_total$immunization.yellow_fever_immun,
                                                 h_total$child_health_yellow_fever_immun_calc)

h$child_health_csm_immunization_calc <- ifelse(h_total$src != "pilot",
                                               h_total$immunization.csm_immunization,
                                               h_total$child_health_csm_immunization_calc)

h$vaccines_icepack_calc <- 
  (h_total$vaccine_storage_type.cold_chain_box == T & 
     h_total$vaccine_storage_type.vaccine_carrier == T )



h$equipment_refrigerator <- h_total$vaccine_storage_type.refrigerator

###################
##### MALARIA #####
###################
h$malaria_testing <- ifelse(h_total$src != "pilot", 
  (h_total$lab_tests.malaria_rdt == T | 
     h_total$lab_tests.malaria_microscopy == T),
                h$malaria_testing)                            

h$malaria_treatment_artemisinin <- h_total$malaria_treatment_artemisinin == 'yes'

h$malaria_treatment_sulphadoxine <- h_total$medication.sulphadoxine

h$paid_services_malaria_treatment <- 
  (h_total$fees_adults.paid_services_malaria_treatment == F | 
     h_total$fees_children.ch_paid_malaria_treatment == F)

################
##### MEDS #####
################

h$medication_anti_malarials <- h_total$medication.act == T

h$oral_antibiotics_calc <- h_total$medication.antibiotic_oral == T

h$medication_antihistamines <- h_total$medication.antihistamines == T

h$medication_iron_tablets <- h_total$supplements.iron == T

h$medication_folic_acid <- h_total$supplements.folic_acid == T

h$medication_iv_fluid <- h_total$medication.iv_fluid == T

h$uterotonics_yn_calc <- h_total$medication.uterotonics == T

#######################
##### DIAGNOSTICS #####
#######################

h$lab_tests_pregnancy_calc <- ifelse(h_total$src != "pilot", 
                                h_total$lab_tests.pregnancy,
                                     h_total$lab_tests_pregnancy_calc)

h$lab_tests_stool_calc <- ifelse(h_total$src != "pilot", 
                                h_total$lab_tests.stool,
                                     h_total$lab_tests_stool_calc)

h$lab_tests_hemoglobin_testing_calc <- h_total$lab_tests.hemoglobin_testing

h$num_lab_techs_fulltime <- h_total$lab_technicians_posted

##########################
##### INFRASTRUCTURE #####
##########################

#zero_na <- function(x) ifelse(is.na(x), x, 0)
h$potable_water_access <- (as.numeric(h_total$days_no_potable_water_pastmth) <= 23)
h$improved_sanitation_and_functional <- (h_total$vip_latrine_functional_yn == 'yes' & 
                                           h_total$num_vip_latrine > 0) | 
  (h_total$slab_pit_latrine_functional_yn == 'yes' & 
     h_total$num_pit_w_slab > 0) | 
  (h_total$flush_improved_functional_yn == 'yes' &   
     h_total$num_flush_or_pour_flush_piped > 0)

########################
##### TUBERCULOSIS #####
########################

h$lab_tests_tb_microscopy_calc <- ifelse(h_total$src != "pilot", 
                                    h_total$lab_tests.tb_microscopy == T,
                                     h_total$lab_tests_tb_microscopy_calc)

h$tb_treatment_yn <- h_total$tb_treatment_yn == 'yes'

###############
##### HIV #####
###############

h$lab_tests_hiv_testing_calc <- ifelse(h_total$src != "pilot", 
                                    h_total$lab_tests.hiv_testing == T,
                                       h_total$lab_tests_hiv_testing_calc)

h$hiv_treatment_yn <- h_total$medication.arvs == T

#########################
##### CURATIVE CARE #####
#########################

h$health_no_user_fees <- ifelse(h_total$src != "pilot", 
                            h_total$adult_tx_fees_yn == 'yes' |                                          
                            h_total$child_tx_fees_yn == 'yes',
                              h_total$health_no_user_fees)    

h$iv_medications_yn <- h_total$medication.iv_fluid   

h$inpatient_care_yn <- h_total$inpatient_care_yn == 'yes'   




#Adding distant to every facility
#combining calculated result back to original data
hh <- lga_boudary_dist(h, gps_col="gps")
health_661_comp <- h
h_661 <- merge(h_total, h_661_left, by="uuid")


#Delete all those have dist >= 35 km
health_661_comp <- subset(health_661_comp, dist_fake <= 35 | is.na(dist_fake))
h_661 <- subset(h_661, dist_fake <= 35 | is.na(dist_fake))


saveRDS(x_y_killa(health_661_comp), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Health_661_NMIS_Facility.rds")
saveRDS(x_y_killa(h_661), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Health_661_ALL_FACILITY_INDICATORS.rds")

