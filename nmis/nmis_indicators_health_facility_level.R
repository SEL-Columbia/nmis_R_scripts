source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")

#reading in data
h <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Health_661_outliercleaned.csv")
lga_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/661.csv")
##throw out all values from 113 LGAs that were resampled in 661
h <- merge(h, lga_661, by="lga_id")

#new data set
hh <- subset(h, select=c("uuid", "mylga", "mylga_state", "mylga_zone", "lga_id", "photo",
                         "unique_lga"))
hh$`_id` <- hh$uuid
hh$formhub_photo_id <- hh$photo
hh$gps <- h$geocodeoffacility
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
  (rowSums(cbind(h$medical_staff_posted.num_doctors_posted, 
                 h$medical_staff_posted.num_nurses_posted,
                 h$medical_staff_posted.num_nursemidwives_posted), na.rm=T) > 0)
hh$num_chews_and_chos <-
  (rowSums(cbind(h$medical_staff_posted.num_chews_posted,
                 h$medical_staff_posted.num_junior_chews_posted,
                 h$medical_staff_posted.num_cho_posted), na.rm=T))
hh$vaccines_fridge_freezer <- 
  (h$not_for_private_1.vaccine_storage_type.refrigerator == T & 
  h$not_for_private_1.vaccine_storage_type.freezer == T )
hh$emergency_transport <- 
  (h$transport_to_referral %in% c('ambulance', 'keke'))
hh$improved_water_supply <- (h$not_for_private_1.water_sources.tap_in_compound == T | 
  h$not_for_private_1.water_sources.tap_outside == T | 
  h$not_for_private_1.water_sources.borehole_tube_well == T)    
hh$improved_sanitation <- 
  (h$not_for_private_1.toilets_available.num_vip_latrine > 0) | 
  (h$not_for_private_1.toilets_available.num_pit_w_slab > 0) | 
  (h$not_for_private_1.toilets_available.num_flush_or_pour_flush_piped > 0)
hh$phcn_electricity <-
  (h$not_for_private_1.grid_proximity == 'connected_to_grid' | 
  h$not_for_private_1.local_grid_proximity == 'connected_to_local_grid')
#######################
#########################

hh$maternal_health_delivery_services_24_7 <- 
 (h$facility_open_247_yn == 'yes' & h$delivery_services_yn == 'yes' & h$delivery_skilled_birth_247_yn == 'yes')



hh$facility_open_247_yn <- h$facility_open_247_yn == 'yes'   

hh$essential_meds_stockout <- (h$not_for_private_2.medication_out_of_stock.antibiotics_oral_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.antibiotics_musc_stockout_yn == 'yes' | 
      h$not_for_private_2.medication_out_of_stock.antibiotics_iv_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.iv_fliud_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.ort_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.uterotonics_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.antidiarrheal_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.antipyretics_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.act_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.sulphadoxine_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.arvs_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.nevirapine_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.azt_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.tb_meds_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.sedatives_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.antihistamines_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.anticonvulsants_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.oral_contacept_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.inject_contacept_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.implants_stockout_yn == 'yes' |
      h$not_for_private_2.medication_out_of_stock.iud_stockout_yn == 'yes')
 
hh$emergency_transport_currently_functioning <- 
      (h$transport_to_referral != 'none')



hh$power_access_and_functional <-
    h$not_for_private_1.power_sources.none == F

hh$comprehensive_obstetrics_yn <- h$emergency_obstetrics_yn == 'yes' & h$c_section_yn == 'yes'

####################
##### STAFFING #####
####################

hh$num_chews_total <- 
  (rowSums(cbind(h$medical_staff_posted.num_chews_posted,
                 h$medical_staff_posted.num_junior_chews_posted), na.rm=T))

hh$num_nurses_fulltime <- h$medical_staff_posted.num_nurses_posted
  
hh$num_nursemidwives_fulltime <- 
  (rowSums(cbind(h$medical_staff_posted.num_midwives_posted,
   h$medical_staff_posted.num_nursemidwives_posted), na.rm=T))
   
hh$num_doctors_fulltime <- 
   h$medical_staff_posted.num_doctors_posted
   
hh$num_lab_techs_fulltime <- 
   h$medical_staff_posted.lab_technicians_posted
      
hh$staff_paid_lastmth_yn <-   
   (h$staff_paid_1mths_yn == 'yes' | 
   h$staff_paid_3mths_yn == 'yes')
   
###############################
##### M.HEALTH: ANTENATAL #####
###############################

hh$sulpha_and_antenatal <- h$antenatal_care_services.antenatal_care_malaria_prlx == 'yes'

hh$has_itns <- h$not_for_private_2.supplies.insecticide_treated_bednets == T
   
hh$medication_iron_tablets <- h$not_for_private_2.supplements.iron == T
   
hh$medication_folic_acid <- h$antenatal_care_services.medication_folic_acid == 'yes'  
   
hh$scale_yn <- (h$not_for_private_2.equipment.scale == T & 
  h$not_for_private_2.equipment_functional.weighing_scale_funct_yn == "yes")
   
hh$equipment_bp_machine <- (h$not_for_private_2.equipment.bp_machine == T & 
  h$not_for_private_2.equipment_functional.bp_machine_funct_yn == "yes")   

hh$hiv_tx_srvcs_pmtct_services_calc <- (h$not_for_private_2.medication.nevirapine == T | 
          h$not_for_private_2.medication.arvs == T |
          h$not_for_private_2.medication.azt == T)

hh$lab_tests_hemoglobin_testing_calc <- h$not_for_private_2.lab_tests.hemoglobin_testing == T
  
hh$lab_tests_urine_testing_calc <- h$not_for_private_2.lab_tests.urine_testing == T
  
##################################
##### M.HEALTH: OBSTETRICS 1 #####
##################################
   
hh$mobile_signal_funct_yn <-  
  (h$not_for_private_1.phone_signal_strength == 'low_signal_strength' | 
  h$not_for_private_1.phone_signal_strength == 'high_signal_strength') &  
  h$not_for_private_1.info_tech_available.mobile_facility == T

hh$iv_antibiotics_yn_calc <- h$emergency_antenatal.emoc_antibiotics_yn == 'yes' | h$not_for_private_2.medication.antibiotic_iv == T
   
hh$emoc_vacuum_extractor <- h$emergency_antenatal.emoc_vacuum_extractor_yn == "yes"

hh$emoc_forceps <- (h$not_for_private_2.equipment.emoc_forceps == T & 
    h$not_for_private_2.equipment_functional.forceps_funct_yn == "yes")

hh$compr_oc_c_sections <- h$c_section_yn == 'yes' 
   
##################################
##### M.HEALTH: OBSTETRICS 2 #####
##################################

hh$uterotonics_yn_calc <- h$emergency_antenatal.emoc_uterotonics_yn == 'yes'   

hh$antishock_garment_yn <- h$not_for_private_2.equipment.emoc_antishock_garment == T
  
hh$emoc_parenteral_anticonvulsant <- h$emergency_antenatal.emoc_parenteral_anticonvulsant_yn == 'yes'
  
#hh$skilled_birth_attendant <- 
 #       (rowSums(cbind(h$medical_staff_posted.num_doctors_posted, 
  #      h$medical_staff_posted.num_nurses_posted,
   #     h$medical_staff_posted.num_nursemidwives_posted), na.rm=T) > 0)

hh$at_least_two_skilled_birth_attendants <- 
  (rowSums(cbind(h$medical_staff_posted.num_doctors_posted, 
                 h$medical_staff_posted.num_nurses_posted,
                 h$medical_staff_posted.num_nursemidwives_posted), na.rm=T) > 1)

hh$at_least_three_skilled_birth_attendants <- 
  (rowSums(cbind(h$medical_staff_posted.num_doctors_posted, 
                 h$medical_staff_posted.num_nurses_posted,
                 h$medical_staff_posted.num_nursemidwives_posted), na.rm=T) > 2)

hh$at_least_four_skilled_birth_attendants <- 
  (rowSums(cbind(h$medical_staff_posted.num_doctors_posted, 
                 h$medical_staff_posted.num_nurses_posted,
                 h$medical_staff_posted.num_nursemidwives_posted), na.rm=T) > 3)

hh$compr_oc_blood_transfusions <- h$cesarean_section.compr_oc_blood_transfusions == 'yes'
   
#####################################
##### M.HEALTH: FAMILY PLANNING #####
#####################################
hh$condoms_yn <- h$not_for_private_2.supplies.condoms == T
   
hh$family_planning_pill_calc_calc <- h$family_planning.family_planning_pill == 'yes'
   
hh$family_planning_injectables_calc_calc <- h$family_planning.family_planning_injectables == 'yes'
   
hh$family_planning_iud_calc <- h$not_for_private_2.medication.iud == T
  
hh$family_planning_implants_calc <- h$not_for_private_2.medication.implants == T

hh$sterilization_yn_calc <- 
  (h$family_planning.family_planning_sterilization_m == 'yes' | 
   h$family_planning.family_planning_sterilization_f == 'yes')
   
###########################
##### CHILD NUTRITION #####
###########################
   
hh$child_health_growth_monitor <- 
  (h$not_for_private_2.equipment_functional.weighing_scale_funct_yn == "yes" & 
  h$not_for_private_2.equipment.scale == T &
   h$not_for_private_2.supplies.muac_tape == T)
   
##############################
##### CHILD IMMUNIZATION #####
##############################

hh$child_health_measles_immun_calc <- h$not_for_private_2.immunization.measles_immun

hh$child_health_opv_immuization_calc <- h$not_for_private_2.immunization.opv_immuization

hh$child_health_dpt_immunization_calc <- h$not_for_private_2.immunization.dpt_immunization

hh$child_health_tetanus_immun_calc <- h$not_for_private_2.immunization.tetanus_immun

hh$child_health_hepb_immunization_calc <- h$not_for_private_2.immunization.hepb_immunization

hh$child_health_bcg_immunization_calc <- h$not_for_private_2.immunization.bcg_immunization

hh$child_health_yellow_fever_immun_calc <- h$not_for_private_2.immunization.yellow_fever_immun

hh$child_health_csm_immunization_calc <- h$not_for_private_2.immunization.csm_immunization

hh$vaccines_icepack_calc <- 
      (h$not_for_private_1.vaccine_storage_type.cold_chain_box == T & 
     h$not_for_private_1.vaccine_storage_type.vaccine_carrier == T )



hh$equipment_refrigerator <- h$not_for_private_1.vaccine_storage_type.refrigerator
   
###################
##### MALARIA #####
###################
hh$malaria_testing <- 
  (h$not_for_private_2.lab_tests.malaria_rdt == T | 
   h$not_for_private_2.lab_tests.malaria_microscopy == T) 
   
hh$malaria_treatment_artemisinin <- h$malaria_treatment_artemisinin == 'yes'

hh$malaria_treatment_sulphadoxine <- h$not_for_private_2.medication.sulphadoxine

hh$paid_services_malaria_treatment <- 
  (h$not_for_private_2.fees_adults.paid_services_malaria_treatment == F | 
  h$not_for_private_2.fees_children.ch_paid_malaria_treatment == F)
   
################
##### MEDS #####
################

hh$medication_anti_malarials <- h$not_for_private_2.medication.act == T

hh$oral_antibiotics_calc <- h$not_for_private_2.medication.antibiotic_oral == T

hh$medication_antihistamines <- h$not_for_private_2.medication.antihistamines == T
   
hh$medication_iron_tablets <- h$not_for_private_2.supplements.iron == T

hh$medication_folic_acid <- h$not_for_private_2.supplements.folic_acid == T
     
hh$medication_iv_fluid <- h$not_for_private_2.medication.iv_fluid == T
          
hh$uterotonics_yn_calc <- h$not_for_private_2.medication.uterotonics == T
     
#######################
##### DIAGNOSTICS #####
#######################
   
hh$lab_tests_pregnancy_calc <- h$not_for_private_2.lab_tests.pregnancy    
   
hh$lab_tests_stool_calc <- h$not_for_private_2.lab_tests.stool
 
hh$lab_tests_hemoglobin_testing_calc <- h$not_for_private_2.lab_tests.hemoglobin_testing
  
hh$num_lab_techs_fulltime <- h$medical_staff_posted.lab_technicians_posted
   
##########################
##### INFRASTRUCTURE #####
##########################

#zero_na <- function(x) ifelse(is.na(x), x, 0)
#hh$potable_water_access <- zero_na(h$not_for_private_1.days_no_potable_water_pastmth) <= 23    
hh$potable_water_access <- (as.numeric(h$not_for_private_1.days_no_potable_water_pastmth) <= 23)
hh$improved_sanitation_and_functional <- (h$not_for_private_1.vip_latrine_functional_yn == 'yes' & 
                                      h$not_for_private_1.toilets_available.num_vip_latrine > 0) | 
                                      (h$not_for_private_1.slab_pit_latrine_functional_yn == 'yes' & 
                                      h$not_for_private_1.toilets_available.num_pit_w_slab > 0) | 
                                      (h$not_for_private_1.flush_improved_functional_yn == 'yes' &   
                                      h$not_for_private_1.toilets_available.num_flush_or_pour_flush_piped > 0)

                                                        
########################
##### TUBERCULOSIS #####
########################

hh$lab_tests_tb_microscopy_calc <- h$not_for_private_2.lab_tests.tb_microscopy == T   
   
hh$tb_treatment_yn <- h$tb_treatment_yn == 'yes'
   
###############
##### HIV #####
###############

hh$lab_tests_hiv_testing_calc <- h$not_for_private_2.lab_tests.hiv_testing == T
   
hh$hiv_treatment_yn <- h$not_for_private_2.medication.arvs == T
   
#########################
##### CURATIVE CARE #####
#########################

hh$health_no_user_fees <- 
          h$not_for_private_2.adult_tx_fees_yn == 'yes' |                                          
          h$not_for_private_2.child_tx_fees_yn == 'yes'                                         

hh$iv_medications_yn <- h$not_for_private_2.medication.iv_fluid   
   
hh$inpatient_care_yn <- h$inpatient_care_yn == 'yes'   
     
write.csv(boundary_clean(hh, "mylga_state", "gps"), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Health_661_NMIS_Facility.csv", row.names=F)
write.csv(boundary_clean(cbind(hh, h), "mylga_state", "gps"), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Health_661_ALL_FACILITY_INDICATORS.csv", row.names=F)

#str(hh)
#head(hh)
#summary(hh)

