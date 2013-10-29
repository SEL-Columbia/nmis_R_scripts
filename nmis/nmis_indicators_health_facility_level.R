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
hh$`_id` <- hh$uuid
hh$formhub_photo_id <- hh$photo
hh$gps <- h$geocodeoffacility


nm_661 <- names(h)[! names(h) %in% names(hh)]
nm_661 <- c(nm_661, "uuid")
h_661_left <- subset(h, select=nm_661)
rm(nm_661)


####################
##### SNAPSHOT #####
####################
hh$facility_name <- h$facility_name
hh$facility_type <- h$facility_type
hh$owner_manager <- as.character(ifelse(h$facility_owner_manager.federalgovernment,
                                        "public",
                                 ifelse(h$facility_owner_manager.stategovernment,
                                        "public",
                                  ifelse(h$facility_owner_manager.lga,
                                         "public",
                                         ifelse(h$facility_owner_manager.private_forprofit,
                                                "private",
                                                ifelse(h$facility_owner_manager.charitable_ngo,
                                                       "private",
                                                       ifelse(h$facility_owner_manager.religious_org,
                                                              "private",
                                          NA_character_)))))))
hh$maternal_health_delivery_services <- 
  (h$delivery_services_yn == 'yes')
hh$skilled_birth_attendant <- 
  (rowSums(cbind(h$num_doctors_posted, 
                 h$num_nurses_posted,
                 h$num_nursemidwives_posted), na.rm=T) > 0)
hh$num_chews_and_chos <-
  (rowSums(cbind(h$num_chews_posted,
                 h$num_junior_chews_posted,
                 h$num_cho_posted), na.rm=T))
hh$vaccines_fridge_freezer <- 
  (h$vaccine_storage_type.refrigerator == T & 
  h$vaccine_storage_type.freezer == T )
hh$emergency_transport <- 
  (h$transport_to_referral %in% c('ambulance', 'keke'))
hh$improved_water_supply <- (h$water_sources.tap_in_compound == T | 
  h$water_sources.tap_outside == T | 
  h$water_sources.borehole_tube_well == T)    
hh$improved_sanitation <- 
  (h$num_vip_latrine > 0) | 
  (h$num_pit_w_slab > 0) | 
  (h$num_flush_or_pour_flush_piped > 0)
hh$phcn_electricity <-
  (h$grid_proximity == 'connected_to_grid' | 
  h$local_grid_proximity == 'connected_to_local_grid')
#######################
#########################

hh$maternal_health_delivery_services_24_7 <- 
 (h$facility_open_247_yn == 'yes' & h$delivery_services_yn == 'yes' & h$delivery_skilled_birth_247_yn == 'yes')



hh$facility_open_247_yn <- h$facility_open_247_yn == 'yes'   

hh$essential_meds_stockout <- (h$antibiotics_oral_stockout_yn == 'yes' |
      h$antibiotics_musc_stockout_yn == 'yes' | 
      h$antibiotics_iv_stockout_yn == 'yes' |
      h$iv_fliud_stockout_yn == 'yes' |
      h$ort_stockout_yn == 'yes' |
      h$uterotonics_stockout_yn == 'yes' |
      h$antidiarrheal_stockout_yn == 'yes' |
      h$antipyretics_stockout_yn == 'yes' |
      h$act_stockout_yn == 'yes' |
      h$sulphadoxine_stockout_yn == 'yes' |
      h$arvs_stockout_yn == 'yes' |
      h$nevirapine_stockout_yn == 'yes' |
      h$azt_stockout_yn == 'yes' |
      h$tb_meds_stockout_yn == 'yes' |
      h$sedatives_stockout_yn == 'yes' |
      h$antihistamines_stockout_yn == 'yes' |
      h$anticonvulsants_stockout_yn == 'yes' |
      h$oral_contacept_stockout_yn == 'yes' |
      h$inject_contacept_stockout_yn == 'yes' |
      h$implants_stockout_yn == 'yes' |
      h$iud_stockout_yn == 'yes')
 
hh$emergency_transport_currently_functioning <- 
      (h$transport_to_referral != 'none')



hh$power_access_and_functional <-
    h$power_sources.none != F

hh$comprehensive_obstetrics_yn <- h$emergency_obstetrics_yn == 'yes' & h$c_section_yn == 'yes'

####################
##### STAFFING #####
####################

hh$num_chews_total <- 
  (rowSums(cbind(h$num_chews_posted,
                 h$num_junior_chews_posted), na.rm=T))

hh$num_nurses_fulltime <- h$num_nurses_posted
  
hh$num_nursemidwives_fulltime <- 
  (rowSums(cbind(h$num_midwives_posted,
   h$num_nursemidwives_posted), na.rm=T))
   
hh$num_doctors_fulltime <- 
   h$num_doctors_posted
   
hh$num_lab_techs_fulltime <- 
   h$lab_technicians_posted
      
hh$staff_paid_lastmth_yn <-   
   (h$staff_paid_1mths_yn == 'yes' | 
   h$staff_paid_3mths_yn == 'yes')
   
###############################
##### M.HEALTH: ANTENATAL #####
###############################

hh$sulpha_and_antenatal <- h$antenatal_care_malaria_prlx == 'yes'

hh$has_itns <- h$supplies.insecticide_treated_bednets == T
   
hh$medication_iron_tablets <- h$supplements.iron == T
   
hh$medication_folic_acid <- h$medication_folic_acid == 'yes'  
   
hh$scale_yn <- (h$equipment.scale == T & 
  h$weighing_scale_funct_yn == "yes")
   
hh$equipment_bp_machine <- (h$equipment.bp_machine == T & 
  h$bp_machine_funct_yn == "yes")   

hh$hiv_tx_srvcs_pmtct_services_calc <- (h$medication.nevirapine == T | 
          h$medication.arvs == T |
          h$medication.azt == T)

hh$lab_tests_hemoglobin_testing_calc <- h$lab_tests.hemoglobin_testing == T
  
hh$lab_tests_urine_testing_calc <- h$lab_tests.urine_testing == T
  
##################################
##### M.HEALTH: OBSTETRICS 1 #####
##################################
   
hh$mobile_signal_funct_yn <-  
  (h$phone_signal_strength == 'low_signal_strength' | 
  h$phone_signal_strength == 'high_signal_strength') &  
  h$info_tech_available.mobile_facility == T

hh$iv_antibiotics_yn_calc <- h$emoc_antibiotics == T | h$medication.antibiotic_iv == T
   
hh$emoc_vacuum_extractor <- h$emoc_vacuum_extractor_yn == "yes"

hh$emoc_forceps <- (h$equipment.emoc_forceps == T & 
    h$forceps_funct_yn == "yes")

hh$compr_oc_c_sections <- h$c_section_yn == 'yes' 
   
##################################
##### M.HEALTH: OBSTETRICS 2 #####
##################################

hh$uterotonics_yn_calc <- h$emoc_uterotonics_yn == 'yes'   

hh$antishock_garment_yn <- h$equipment.emoc_antishock_garment == T
  
hh$emoc_parenteral_anticonvulsant <- h$emoc_parenteral_anticonvulsant_yn == 'yes'
  
#hh$skilled_birth_attendant <- 
 #       (rowSums(cbind(h$num_doctors_posted, 
  #      h$num_nurses_posted,
   #     h$num_nursemidwives_posted), na.rm=T) > 0)

hh$at_least_two_skilled_birth_attendants <- 
  (rowSums(cbind(h$num_doctors_posted, 
                 h$num_nurses_posted,
                 h$num_nursemidwives_posted), na.rm=T) > 1)

hh$at_least_three_skilled_birth_attendants <- 
  (rowSums(cbind(h$num_doctors_posted, 
                 h$num_nurses_posted,
                 h$num_nursemidwives_posted), na.rm=T) > 2)

hh$at_least_four_skilled_birth_attendants <- 
  (rowSums(cbind(h$num_doctors_posted, 
                 h$num_nurses_posted,
                 h$num_nursemidwives_posted), na.rm=T) > 3)

hh$compr_oc_blood_transfusions <- h$compr_oc_blood_transfusions == 'yes'
   
#####################################
##### M.HEALTH: FAMILY PLANNING #####
#####################################
hh$condoms_yn <- h$supplies.condoms == T
   
hh$family_planning_pill_calc_calc <- h$family_planning_pill == 'yes'
   
hh$family_planning_injectables_calc_calc <- h$family_planning_injectables == 'yes'
   
hh$family_planning_iud_calc <- h$medication.iud == T
  
hh$family_planning_implants_calc <- h$medication.implants == T

hh$sterilization_yn_calc <- 
  (h$family_planning_sterilization_m == 'yes' | 
   h$family_planning_sterilization_f == 'yes')
   
###########################
##### CHILD NUTRITION #####
###########################
   
hh$child_health_growth_monitor <- 
  (h$weighing_scale_funct_yn == "yes" & 
  h$equipment.scale == T &
   h$supplies.muac_tape == T)
   
##############################
##### CHILD IMMUNIZATION #####
##############################

hh$child_health_measles_immun_calc <- h$immunization.measles_immun

hh$child_health_opv_immuization_calc <- h$immunization.opv_immuization

hh$child_health_dpt_immunization_calc <- h$immunization.dpt_immunization

hh$child_health_tetanus_immun_calc <- h$immunization.tetanus_immun

hh$child_health_hepb_immunization_calc <- h$immunization.hepb_immunization

hh$child_health_bcg_immunization_calc <- h$immunization.bcg_immunization

hh$child_health_yellow_fever_immun_calc <- h$immunization.yellow_fever_immun

hh$child_health_csm_immunization_calc <- h$immunization.csm_immunization

hh$vaccines_icepack_calc <- 
      (h$vaccine_storage_type.cold_chain_box == T & 
     h$vaccine_storage_type.vaccine_carrier == T )



hh$equipment_refrigerator <- h$vaccine_storage_type.refrigerator
   
###################
##### MALARIA #####
###################
hh$malaria_testing <- 
  (h$lab_tests.malaria_rdt == T | 
   h$lab_tests.malaria_microscopy == T) 
   
hh$malaria_treatment_artemisinin <- h$malaria_treatment_artemisinin == 'yes'

hh$malaria_treatment_sulphadoxine <- h$medication.sulphadoxine

hh$paid_services_malaria_treatment <- 
  (h$fees_adults.paid_services_malaria_treatment == F | 
  h$fees_children.ch_paid_malaria_treatment == F)
   
################
##### MEDS #####
################

hh$medication_anti_malarials <- h$medication.act == T

hh$oral_antibiotics_calc <- h$medication.antibiotic_oral == T

hh$medication_antihistamines <- h$medication.antihistamines == T
   
hh$medication_iron_tablets <- h$supplements.iron == T

hh$medication_folic_acid <- h$supplements.folic_acid == T
     
hh$medication_iv_fluid <- h$medication.iv_fluid == T
          
hh$uterotonics_yn_calc <- h$medication.uterotonics == T
     
#######################
##### DIAGNOSTICS #####
#######################
   
hh$lab_tests_pregnancy_calc <- h$lab_tests.pregnancy    
   
hh$lab_tests_stool_calc <- h$lab_tests.stool
 
hh$lab_tests_hemoglobin_testing_calc <- h$lab_tests.hemoglobin_testing
  
hh$num_lab_techs_fulltime <- h$lab_technicians_posted
   
##########################
##### INFRASTRUCTURE #####
##########################

#zero_na <- function(x) ifelse(is.na(x), x, 0)
hh$potable_water_access <- (as.numeric(h$days_no_potable_water_pastmth) <= 23)
hh$improved_sanitation_and_functional <- (h$vip_latrine_functional_yn == 'yes' & 
                                      h$num_vip_latrine > 0) | 
                                      (h$slab_pit_latrine_functional_yn == 'yes' & 
                                      h$num_pit_w_slab > 0) | 
                                      (h$flush_improved_functional_yn == 'yes' &   
                                      h$num_flush_or_pour_flush_piped > 0)
                                                        
########################
##### TUBERCULOSIS #####
########################

hh$lab_tests_tb_microscopy_calc <- h$lab_tests.tb_microscopy == T   
   
hh$tb_treatment_yn <- h$tb_treatment_yn == 'yes'
   
###############
##### HIV #####
###############

hh$lab_tests_hiv_testing_calc <- h$lab_tests.hiv_testing == T
   
hh$hiv_treatment_yn <- h$medication.arvs == T
   
#########################
##### CURATIVE CARE #####
#########################

hh$health_no_user_fees <- 
          h$adult_tx_fees_yn == 'yes' |                                          
          h$child_tx_fees_yn == 'yes'                                         

hh$iv_medications_yn <- h$medication.iv_fluid   
   
hh$inpatient_care_yn <- h$inpatient_care_yn == 'yes'   




#Adding distant to every facility
#combining calculated result back to original data
hh <- lga_boudary_dist(hh, gps_col="gps")
health_661_comp <- hh
h_661 <- merge(hh, h_661_left, by="uuid")


#Delete all those have dist >= 35 km
health_661_comp <- subset(health_661_comp, dist_fake <= 35 | is.na(dist_fake))
h_661 <- subset(h_661, dist_fake <= 35 | is.na(dist_fake))


saveRDS(x_y_killa(health_661_comp), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Health_661_NMIS_Facility.rds")
saveRDS(x_y_killa(h_661), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Health_661_ALL_FACILITY_INDICATORS.rds")


