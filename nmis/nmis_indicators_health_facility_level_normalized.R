#####################################################################################################################
##Normalized Health NMIS Facility Level #############################################################################
#####################################################################################################################
source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")

#Reading in Data
health_outlier <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Health_774_outliercleaned.rds")

#new data set "health_sub"
health_sub <- subset(health_outlier, select=c("uuid", "mylga", "mylga_state", "mylga_zone", "lga_id", "photo",
                         "unique_lga", "facility_name", "facility_type"))

health_sub$formhub_photo_id <- health_outlier$photo
health_sub$gps <- health_outlier$geocodeoffacility


nm_774 <- names(health_outlier)[! names(health_outlier) %in% names(health_sub)]
nm_774 <- c(nm_774, "uuid")
h_774_left <- subset(health_outlier, select=nm_774)
rm(nm_774)


####################
##### SNAPSHOT #####
####################
health_sub$owner_manager <- recodeVar(health_outlier$facility_owner_manager,
                                        c('lga', 'community', 'federalgovernment','stategovrenment', 
                                                'church_mission', 'private_forprofit', 'private_notforprofit'), 
                                          c('public', 'public', 'public','public', 
                                              'private', 'private', 'private'), default=NA)  

health_sub$maternal_health_delivery_services <- ifelse(health_outlier$src == '661',
                                                        health_outlier$delivery_services_yn,
                                                    health_outlier$emergency_obstetrics_yn)
                                                                 
health_sub$skilled_birth_attendant <- 
  (rowSums(cbind(health_outlier$num_doctors_posted, 
                 health_outlier$num_nurses_posted,
                 health_outlier$num_nursemidwives_posted), na.rm=T) > 0)

health_sub$num_chews_and_chos <-
  (rowSums(cbind(health_outlier$num_chews_posted,
                 health_outlier$num_junior_chews_posted,
                 health_outlier$num_cho_posted), na.rm=T))

health_sub$vaccines_fridge_freezer <- ifelse(health_outlier$src == '661',
                                          (health_outlier$vaccine_storage_type.refrigerator & 
                                           health_outlier$vaccine_storage_type.freezer),
                                        ifelse(health_outlier$src == '113',     
                                           as.logical(recodeVar(health_outlier$vaccines_strg_type,
                                                       c('solar_refrigeration', 'grid_refrigeration', 
                                                         'lpg_refrigeration', 'vaccine_carriers_icepacks'),
                                                       c(TRUE, TRUE, TRUE, TRUE), default = NA)),
                                          ifelse(health_outlier$src == 'pilot',     
                                                 health_outlier$vaccines_stored_yn, NA)))
                                                                                         
health_sub$emergency_transport <- 
  health_outlier$transport_to_referral %in% c('ambulance', 'keke')

health_sub$improved_water_supply <- ifelse(health_outlier$src == 'pilot',
                                              water_sources_yn_p,                                         
                                    (health_outlier$water_sources.tap_in_compound | 
                                      health_outlier$water_sources.tap_outside | 
                                      health_outlier$water_sources.borehole_tube_well))

health_sub$improved_sanitation <- ifelse(health_outlier$src == 'pilot',
                                         num_toilets_improved_p > 0,  
                                  (health_outlier$num_vip_latrine > 0) | 
                                  (health_outlier$num_pit_w_slab > 0) | 
                                  (health_outlier$num_flush_or_pour_flush_piped > 0))                                

health_sub$phcn_electricity <- health_outlier$power_sources_grid

#######################
#########################

health_sub$maternal_health_delivery_services_24_7 <- ifelse(health_outlier$src == '661',
                                                         (health_outlier$facility_open_247_yn & 
                                                            health_outlier$delivery_services_yn & 
                                                             health_outlier$delivery_skilled_birth_247_yn),
                                                     health_outlier$emergency_obstetrics_yn & 
                                                       (health_outlier$compr_oc_available_24_7 | 
                                                          health_outlier$emoc_available_24_7))                                                  

health_sub$facility_open_247_yn <- health_outlier$facility_open_247_yn 

health_sub$essential_meds_stockout <- ifelse(health_subtotal == '661',
                                    (health_outlier$antibiotics_oral_stockout_yn |
                                       health_outlier$antibiotics_musc_stockout_yn | 
                                       health_outlier$antibiotics_iv_stockout_yn |
                                       health_outlier$iv_fliud_stockout_yn |
                                       health_outlier$ort_stockout_yn |
                                       health_outlier$uterotonics_stockout_yn |
                                       health_outlier$antidiarrheal_stockout_yn |
                                       health_outlier$antipyretics_stockout_yn |
                                       health_outlier$act_stockout_yn |
                                       health_outlier$sulphadoxine_stockout_yn |
                                       health_outlier$arvs_stockout_yn |
                                       health_outlier$nevirapine_stockout_yn |
                                       health_outlier$azt_stockout_yn |
                                       health_outlier$tb_meds_stockout_yn |
                                       health_outlier$sedatives_stockout_yn |
                                       health_outlier$antihistamines_stockout_yn |
                                       health_outlier$anticonvulsants_stockout_yn |
                                       health_outlier$oral_contacept_stockout_yn |
                                       health_outlier$inject_contacept_stockout_yn |
                                       health_outlier$implants_stockout_yn |
                                       health_outlier$iud_stockout_yn),
                                 ifelse(health_subtotal == '113',       
                                          (health_outlier$antimalarials_stockout_yn | 
                                             health_outlier$antidiarrheal_stockout_yn | 
                                             health_outlier$antibiotics_stockout_yn),
                                 ifelse(health_subtotal == 'pilot',                   
                                          (health_outlier$antimalarials_stockout_yn | 
                                             health_outlier$antibiotics_stockout_yn), NA)))  

health_sub$emergency_transport_currently_functioning <- ifelse(health_outlier$src == '661',
                                                            (health_outlier$transport_to_referral != 'none'), 
                                                       ifelse(health_outlier$src == '113',
                                                               (health_outlier$equipment_emergency_transport & 
                                                                 health_outlier$public_transport_funct_yn), 
                                                       ifelse(health_outlier$src == 'pilot',
                                                               (health_outlier$equipment_emergency_transport & 
                                                                 health_outlier$daily_pub_transport_p), NA)))
                                               

health_sub$power_access_and_functional <- ifelse(health_outlier$src == '661',
                                        health_outlier$power_sources.none != F, 
                                                 health_sub$power_access_and_functional)  

health_sub$comprehensive_obstetrics_yn <- health_outlier$emergency_obstetrics_yn & health_outlier$c_section_yn


####################
##### STAFFING #####
####################

health_sub$num_chews_total <- 
  (rowSums(cbind(health_outlier$num_chews_posted,
                 health_outlier$num_junior_chews_posted), na.rm=T))

health_sub$num_nurses_fulltime <- health_outlier$num_nurses_posted

health_sub$num_nursemidwives_fulltime <- 
  (rowSums(cbind(health_outlier$num_midwives_posted,
                 health_outlier$num_nursemidwives_posted), na.rm=T))

health_sub$num_doctors_fulltime <- health_outlier$num_doctors_posted

health_sub$num_lab_techs_fulltime <- health_outlier$lab_technicians_posted

health_sub$staff_paid_lastmth_yn <- health_outlier$staff_paid_1mths_yn | health_outlier$staff_paid_3mths_yn

###############################
##### M.HEALTH: ANTENATAL #####
###############################

health_sub$sulpha_and_antenatal <- ifelse(health_outlier$src == "113",
                                          (health_outlier$malaria_treatment_sulphadoxine & 
                                             health_outlier$antenatal_care_yn),   
                                        health_outlier$antenatal_care_malaria_prlx)

health_sub$has_itns <- health_outlier$supplies.insecticide_treated_bednets

health_sub$medication_iron_tablets <- health_outlier$supplements.iron

health_sub$medication_folic_acid <- health_outlier$medication_folic_acid 

health_sub$scale_yn <- health_outlier$equipment.scale & health_outlier$weighing_scale_funct_yn

health_sub$equipment_bp_machine <- health_outlier$equipment.bp_machine & health_outlier$bp_machine_funct_yn

health_sub$hiv_tx_srvcs_pmtct_services_calc <- ifelse(health_outlier$src == '661',
                                                       (health_outlier$medication.nevirapine | 
                                                        health_outlier$medication.arvs |
                                                        health_outlier$medication.azt),
                                                ifelse(health_outlier$src == '113',
                                                      (h_113$sti_treatment_yn & h_113$hiv_tx_srvcs_pmtct_services),
                                                   ifelse(health_outlier$src == 'pilot',
                                                      h_pilot$sti_treatment_yn, NA)))

health_sub$lab_tests_hemoglobin_testing_calc <- health_outlier$lab_tests.hemoglobin_testing
                                              
health_sub$lab_tests_urine_testing_calc <- health_outlier$lab_tests.urine_testing

##################################
##### M.HEALTH: OBSTETRICS 1 #####
##################################

health_sub$mobile_signal_funct_yn <-  (health_outlier$phone_signal_strength == 'low_signal_strength' | 
                                          health_outlier$phone_signal_strength == 'high_signal_strength') &  
                                            health_outlier$info_tech_available.mobile_facility 

health_sub$iv_antibiotics_yn_calc <- ifelse(health_outlier$src == '661',
                                              (health_outlier$emoc_antibiotics | 
                                               health_outlier$medication.antibiotic_iv),
                                      ifelse(health_outlier$src == '113', 
                                            (health_outlier$emoc_needles_tubing | 
                                                (health_outlier$emoc_parenteral1 | 
                                                   health_outlier$emoc_antibiotics) & 
                                               health_outlier$emergency_obstetrics_yn) | 
                                            (health_outlier$comprehensive_obstetrics_yn & 
                                               health_outlier$emoc_antibiotics),  
                                       ifelse(health_outlier$src == 'pilot',
                                            (health_outlier$comprehensive_obstetrics_yn & 
                                               health_outlier$emoc_antibiotics), NA)))     

health_sub$emoc_vacuum_extractor <- health_outlier$emoc_vacuum_extractor_yn

health_sub$emoc_forceps <- health_outlier$equipment.emoc_forceps & health_outlier$forceps_funct_yn

health_sub$compr_oc_c_sections <- health_outlier$c_section_yn

##################################
##### M.HEALTH: OBSTETRICS 2 #####
##################################

health_sub$uterotonics_yn_calc <- ifelse(health_outlier$src == '113',
                                         (health_outlier$medication_oxytocin |
                                            ((health_outlier$emoc_uterotonic2 | 
                                                health_outlier$emoc_oxytocin | 
                                                  health_outlier$emoc_misoprotol) &
                                               health_outlier$emergency_obstetrics_yn) | 
                                            ((health_outlier$compr_oc_oxytocin | 
                                                health_outlier$compr_oc_misoprotol) & 
                                               health_outlier$comprehensive_obstetrics_yn)),
                                         health_outlier$emoc_uterotonics_yn)

health_sub$antishock_garment_yn <- health_outlier$equipment.emoc_antishock_garment
                                 
health_sub$emoc_parenteral_anticonvulsant <- health_outlier$emoc_parenteral_anticonvulsant_yn 

health_sub$at_least_two_skilled_birth_attendants <- 
  (rowSums(cbind(health_outlier$num_doctors_posted, 
                 health_outlier$num_nurses_posted,
                 health_outlier$num_nursemidwives_posted), na.rm=T) > 1)

health_sub$at_least_three_skilled_birth_attendants <- 
  (rowSums(cbind(health_outlier$num_doctors_posted, 
                 health_outlier$num_nurses_posted,
                 health_outlier$num_nursemidwives_posted), na.rm=T) > 2)

health_sub$at_least_four_skilled_birth_attendants <- 
  (rowSums(cbind(health_outlier$num_doctors_posted, 
                 health_outlier$num_nurses_posted,
                 health_outlier$num_nursemidwives_posted), na.rm=T) > 3)

health_sub$compr_oc_blood_transfusions <- health_outlier$compr_oc_blood_transfusions

#####################################
##### M.HEALTH: FAMILY PLANNING #####
#####################################
health_sub$condoms_yn <- health_outlier$supplies.condoms                         

health_sub$family_planning_pill_calc_calc <- ifelse(health_outlier$src == '661'
                                                    health_outlier$family_planning_pill,
                                              ifelse(health_outlier$src == '113',                                                    
                                                    (health_outlier$family_planning_pill & 
                                                       health_outlier$family_planning_yn) |
                                                      (health_outlier$medication_oral_contraceptives),
                                              ifelse(health_outlier$src == 'pilot',    
                                                    (health_outlier$family_planning_pill & 
                                                      health_outlier$family_planning_yn), NA)))               
                                                    
health_sub$family_planning_injectables_calc_calc <- ifelse(health_outlier$src != c('pilot', '113'),
                                                  health_outlier$family_planning_injectables,
                                                    health_outlier$family_planning_injectables_calc_calc)

health_sub$family_planning_iud_calc <- health_outlier$medication.iud

health_sub$family_planning_implants_calc <- health_outlier$medication.implants

health_sub$sterilization_yn_calc <- ifelse(health_outlier$src != c('pilot', '113'),
                                              (health_outlier$family_planning_sterilization_m | 
                                               health_outlier$family_planning_sterilization_f),
                                             health_outlier$sterilization_yn_calc)

###########################
##### CHILD NUTRITION #####
###########################

health_sub$child_health_growth_monitor <- health_outlier$child_health_growth_monitor
  
##############################
##### CHILD IMMUNIZATION #####
##############################

health_sub$child_health_measles_immun_calc <- health_outlier$immunization.measles_immun

health_sub$child_health_opv_immuization_calc <- health_outlier$immunization.opv_immuization

health_sub$child_health_dpt_immunization_calc <- health_outlier$immunization.dpt_immunization

health_sub$child_health_tetanus_immun_calc <- health_outlier$immunization.tetanus_immun
                                            
health_sub$child_health_hepb_immunization_calc <- health_outlier$immunization.hepb_immunization                                                

health_sub$child_health_bcg_immunization_calc <- health_outlier$immunization.bcg_immunization

health_sub$child_health_yellow_fever_immun_calc <- health_outlier$immunization.yellow_fever_immun
                                                 
health_sub$child_health_csm_immunization_calc <- health_outlier$immunization.csm_immunization

health_sub$vaccines_icepack_calc <- ifelse(health_outlier$src != '113',
                                              (health_outlier$vaccine_storage_type.cold_chain_box & 
                                              health_outlier$vaccine_storage_type.vaccine_carrier),
                                            health_outlier$health_subvaccines_icepack_calc)

health_sub$equipment_refrigerator <- health_outlier$vaccine_storage_type.refrigerator

###################
##### MALARIA #####
###################
health_sub$malaria_testing <- ifelse(health_outlier$src != c('pilot', '113'), 
                              (health_outlier$lab_tests.malaria_rdt | 
                               health_outlier$lab_tests.malaria_microscopy),
                                  health_submalaria_testing)                            

health_sub$malaria_treatment_artemisinin <- health_outlier$malaria_treatment_artemisinin 

health_sub$malaria_treatment_sulphadoxine <- health_outlier$medication.sulphadoxine

health_sub$paid_services_malaria_treatment <- 
  (health_outlier$fees_adults.paid_services_malaria_treatment == F | 
     health_outlier$fees_children.ch_paid_malaria_treatment == F)

################
##### MEDS #####
################

health_sub$medication_anti_malarials <- health_outlier$medication.act 

health_sub$oral_antibiotics_calc <- health_outlier$medication.antibiotic_oral
                                  
health_sub$medication_antihistamines <- health_outlier$medication.antihistamines 

health_sub$medication_iron_tablets <- health_outlier$supplements.iron 

health_sub$medication_folic_acid <- health_outlier$supplements.folic_acid 

health_sub$medication_iv_fluid <- health_outlier$medication.iv_fluid 

health_sub$uterotonics_yn_calc <- health_outlier$medication.uterotonics 

#######################
##### DIAGNOSTICS #####
#######################

health_sub$lab_tests_pregnancy_calc <- health_outlier$lab_tests.pregnancy

health_sub$lab_tests_stool_calc <- health_outlier$lab_tests.stool

health_sub$num_lab_techs_fulltime <- health_outlier$lab_technicians_posted

##########################
##### INFRASTRUCTURE #####
##########################

health_sub$potable_water_access <- (as.numeric(health_outlier$days_no_potable_water_pastmth) <= 23)
health_sub$improved_sanitation_and_functional <- ifelse(health_outlier$src != c('pilot', '113') 
                                          (health_outlier$vip_latrine_functional_yn & 
                                             health_outlier$num_vip_latrine > 0) | 
                                          (health_outlier$slab_pit_latrine_functional_yn & 
                                             health_outlier$num_pit_w_slab > 0) | 
                                          (health_outlier$flush_improved_functional_yn &   
                                             health_outlier$num_flush_or_pour_flush_piped > 0),
                                        health_subimproved_sanitation_and_functional)           

########################
##### TUBERCULOSIS #####
########################

health_sub$lab_tests_tb_microscopy_calc <- health_outlier$lab_tests.tb_microscopy

health_sub$tb_treatment_yn <- health_outlier$tb_treatment_yn

###############
##### HIV #####
###############

health_sub$lab_tests_hiv_testing_calc <- ifelse(health_outlier$src != c('pilot', '113'), 
                                    health_outlier$lab_tests.hiv_testing,
                                       health_outlier$lab_tests_hiv_testing_calc)

health_sub$hiv_treatment_yn <- health_outlier$medication.arvs

#########################
##### CURATIVE CARE #####
#########################

health_sub$health_no_user_fees <- ifelse(health_outlier$src != c('pilot', '113'), 
                            health_outlier$adult_tx_fees_yn |                                          
                            health_outlier$child_tx_fees_yn,
                              health_outlier$health_no_user_fees)    

health_sub$iv_medications_yn <- health_outlier$medication.iv_fluid     

health_sub$inpatient_care_yn <- health_outlier$inpatient_care_yn

#For LGA level
health_sub$routine_immunization <- (health_outlier$immunization.bcg_immunization | 
                                     health_outlier$immunization.bcg_immunization |
                                     health_outlier$immunization.opv_immuization |
                                     health_outlier$immunization.measles_immun |
                                     health_outlier$immunization.dpt_immunization |
                                     health_outlier$immunization.yellow_fever_immun |
                                     health_outlier$immunization.csm_immunization |
                                     health_outlier$immunization.hepb_immunization |
                                     health_outlier$immunization.tetanus_immun)

health_sub$health_no_delivery_user_fees <- health_outlier$fees_adults.paid_services_anc_delivery == F

health_sub$health_no_child_user_fees <- health_outlier$child_tx_fees_yn == F


#Adding distant to every facility
#combining calculated result back to original data
health_sub <- lga_boudary_dist(health_sub, gps_col="gps")
health_774 <- merge_non_redundant(health_sub, h_774_left, by="uuid")

#Delete all those have dist >= 35 km
health_sub <- subset(health_sub, dist_fake <= 35 | is.na(dist_fake))
health_774 <- subset(health_774, dist_fake <= 35 | is.na(dist_fake))


saveRDS(x_y_killa(health_sub), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Health_661_NMIS_Facility.rds")
saveRDS(x_y_killa(health_774), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Health_661_ALL_FACILITY_INDICATORS.rds")

