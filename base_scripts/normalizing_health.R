##########
##Health##
##########
setwd("~/Code/nmis_R_scripts/")
source("source_scripts/Normailize_Functions.R")

#back to formhub.read()
h_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Health_661_Merged.csv", 
                  stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999"))
h_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Health_PhII_RoundI&II&III_Clean_2011.10.21.csv",
                  stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999"))
h_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Data_Health_Clean_2011.11.18.csv",
                    stringsAsFactors=F, na.strings = c("NA", "n/a", "999", "9999"))

#adding source column 
h_661$src <- "661"
h_113$src <- "113"
h_pilot$src <- "pilot"

#adding uuid to 113 + pilot
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

mapped_661 <- c("paid_services_routine_visit", "paid_services_lab_testing", "paid_services_medication",
                "paid_services_registration", "paid_services_routine_anc_visit", "paid_services_contraceptives",
                "paid_services_anc_delivery", "paid_services_immunization", "paid_services_malaria_treatment")

# 113
#go through cleaning 999's => onwards

##113
h_113 <- rename(h_113, c("bucket_system_number" = "num_bucket_system",
                         "flush_toilet_number" = "num_flush_other", 
                         "vip_latrine_number" = "num_vip_latrine", 
                         "slab_pit_latrine_number" = "num_pit_w_slab", 
                         "open_pit_latrine_number" = "num_open_pit_latrine",
                         "equipment_scale" = "equipment.scale",
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
                         "paid_services_routine_visit" = "fees_adults.paid_services_routine_visit",
                         "medication_iv_fluid" = "medication.iv_fluid"
                         ))

mapped_113 <- c("num_bucket_system", "num_flush_other", "num_vip_latrine", "num_pit_w_slab", "lab_tests.pregnancy",
                "num_open_pit_latrine", "equipment.scale", "equipment.bp_machine", "lab_tests.hemoglobin_testing",
                "lab_tests_urine_testing", "supplies.needles_and_tubing", "equipment.emoc_antishock_garment",
                "medication.oral_contraceptives", "medication.injectable_contracept", "immunization.measles_immun",
                "immunization.opv_immuization", "immunization.dpt_immunization", "immunization.tetanus_immun",
                "immunization.csm_immunization", "lab_tests.malaria_rdt", "lab_tests.malaria_microscopy", 
                "lab_tests.tb_microscopy", "lab_tests.hiv_testing", "medication_iv_fluid")

newname_113 <- c("antibiotics_stockout_yn", "antimalarials_stockout_yn", 
                 "malaria_treatment_sulphadoxine", "emoc_parenteral1","paid_services_tb_treatment",
                 "emoc_available_24_7", "toilet_types_flush_or_pour_flush",
                 "power_sources_grid", "compr_oc_available_24_7", "toilet_types_pit_w_slab", 
                 "toilet_types_vip_latrine", "water_sources_tap_in_compound",
                 "water_sources_tap_outside", "water_sources_borehole_tube_well", 
                 "emergency_transport_keke_napep", "emergency_transport_ambulance", 
                 "vaccines_strg_type", "num_chos_fulltime", "num_nursemidwives_fulltime",
                 "num_jr_chews_fulltime", "num_chews_fulltime", "facility_owner_manager", 
                 "num_doctors_fulltime", "num_midwives_fulltime", "child_health_yn"
                 "hiv_tx_srvcs_pmtct_services", "sti_treatment_yn", "malaria_treatment_yn", 
                 "malaria_treatment_srvcs_itn", "equipment_emergency_transport", 
                 "public_transport_funct_yn", "power_sources_generator", "power_sources_grid", 
                 "power_sources_solar", "comprehensive_obstetrics_yn", "emoc_antibiotics",
                 "medication_oxytocin", "emoc_uterotonic2", "emoc_misoprotol", "emoc_oxytocin", 
                 "compr_oc_oxytocin", "compr_oc_misoprotol", "compr_oc_antishock_garment", 
                 "sti_tx_srvcs_condoms", "hiv_tx_srvcs_condoms", "supplies_available_condoms",
                 "child_health_yn", "child_health_vaccine_carriers", "sti_tx_srvcs_penicilling", 
                 "sti_tx_srvcs_doxycycline", "sti_tx_srvcs_ciprofloxacin", "flush_toilet_drain_to",
                 "child_health_ampicillin", "sti_treatment_yn", "child_health_ciprofloxain", 
                 "medication_anti_biotics", "toilet_types_vip_latrine", "toilet_types_pit_w_slab",
                 "flush_toilet_drain_to", "toilet_types_flush_or_pour_flush", "vip_latrine_not_working", 
                 "toilet_types_flush_or_pour_flush", "slab_pit_latrine_not_working",
                 "flush_toilet_not_working", "paid_services_child_health", "paid_services_hiv_treatment"
                      )


##pilot facility scriptin (somewhere else?)
#be sure to include in NEW list below if you decide to leave here + delete ones that are mentioned in comments

#TODO: health follow up: all NA
h_pilot$sulpha_and_antenatal <- as.character(ifelse((h_pilot$malaria_treatment_sulphadoxine == T & 
                                                       h_pilot$antenatal_care_yn == "yes"),
                                                    "yes", NA_character_))
#TODO: health... make sure the two are the same exact formula 
h_pilot$lab_tests_pregnancy_calc <- (h_pilot$lab_tests_pregnancy == T & 
                                       h_pilot$laboratory_yn == 'yes')   
h_pilot$lab_tests_stool_calc <- h_pilot$lab_tests_pregnancy_calc

h_pilot$essential_meds_stockout <- (hp$antimalarials_stockout_yn == 'yes' |
                                       hp$antibiotics_stockout_yn == 'yes') 
h_pilot$emergency_transport_currently_functioning <- 
                    (h_pilot$equipment_emergency_transport == T & h_pilot$daily_pub_transport_p == 'yes')
h_pilot$power_access_and_functional <- (((h_pilot$power_sources_generator == T &
                                            h_pilot$generator_funct_yn == 'yes') |
                                       (h_pilot$power_sources_solar == T &
                                          h_pilot$solar_funct_yn == 'yes') |
                                       (h_pilot$power_sources_grid == T &
                                          h_pilot$grid_funct_yn == 'yes')) &
                                      (as.numeric(h_pilot$days_no_electricity) <= 7)) 
h_pilot$has_itns <- (h_pilot$malaria_treatment_yn == 'yes' & (h_pilot$malaria_treatment_srvcs_itn == T |
                        h_pilot$supplies_available_bednets == T))       
h_pilot$hiv_tx_srvcs_pmtct_services_calc <- h_pilot$sti_treatment_yn == 'yes'
h_pilot$delivery_services_yn <- as.character(recodeVar(h_pilot$emergency_obstetrics_yn,
                                                       c('yes', 'no'),
                                                       c('yes', 'no')))
h_pilot$maternal_health_delivery_services_24_7 <- 
  (h_pilot$emergency_obstetrics_yn=='yes' & 
     (h_pilot$compr_oc_available_24_7==T | h_pilot$emoc_available_24_7 == T))
h_pilot$lab_tests_hemoglobin_testing_calc <- (h_pilot$laboratory_yn == 'yes' &
                                                h_pilot$lab_tests_hemoglobin_testing == T)
h_pilot$lab_tests_urine_testing <- (h_pilot$laboratory_yn == 'yes' &
                                      h_pilot$lab_tests_urine_testing == T)
h_pilot$iv_antibiotics_yn_calc <- (h_pilot$comprehensive_obstetrics_yn == 'yes' & 
                                 h_pilot$emoc_antibiotics == T)     
h_pilot$condoms_yn <- (h_pilot$sti_tx_srvcs_condoms == T | 
                     h_pilot$hiv_tx_srvcs_condoms == T | 
                     h_pilot$supplies_available_condoms == T)
h_pilot$family_planning_pill_calc_calc <- (h_pilot$family_planning_pill == T  & h_pilot$family_planning_yn == 'yes') 
h_pilot$family_planning_injectables_calc_calc <- (h_pilot$family_planning_pill == T & h_pilot$family_planning_yn == 'yes')                                                
h_pilot$family_planning_iud_calc <- (h_pilot$family_planning_iud == T & h_pilot$family_planning_yn == 'yes')  
h_pilot$family_planning_implants_calc <- (h_pilot$family_planning_implants == T & h_pilot$family_planning_yn == 'yes')
h_pilot$sterilization_yn_calc <- (h_pilot$family_planning_sterilization_m == T & h_pilot$family_planning_yn == 'yes')                                     
h_pilot$child_health_measles_immun_calc <- (h_pilot$child_health_immunization_p == T & h_pilot$child_health_yn == 'yes')  
h_pilot$child_health_opv_immuization_calc <- (h_pilot$child_health_immunization_p == T & h_pilot$child_health_yn == 'yes')  
h_pilot$child_health_dpt_immunization_calc <- (h_pilot$child_health_immunization_p == T & h_pilot$child_health_yn == 'yes')  
h_pilot$child_health_tetanus_immun_calc <- (h_pilot$child_health_immunization_p == T & h_pilot$child_health_yn == 'yes')  
h_pilot$child_health_hepb_immunization_calc <- (h_pilot$child_health_immunization_p == T & h_pilot$child_health_yn == 'yes')  
h_pilot$child_health_bcg_immunization_calc  <- (h_pilot$child_health_immunization_p == T & h_pilot$child_health_yn == 'yes')       
h_pilot$child_health_yellow_fever_immun_calc <- (h_pilot$child_health_immunization_p == T & h_pilot$child_health_yn == 'yes')  
h_pilot$child_health_csm_immunization_calc <- (h_pilot$child_health_immunization_p == T & h_pilot$child_health_yn == 'yes')
h_pilot$vaccines_fridge_freezer <- h_pilot$vaccines_stored_yn == "yes"
h_pilot$improved_water_supply <-  h_pilot$water_sources_yn_p == "yes"
h_pilot$improved_sanitation <- h_pilot$num_toilets_improved_p > 0
h_pilot$malaria_testing <- (h_pilot$lab_tests_malaria_rdt == T | 
                              h_pilot$lab_tests_malaria_microscopy == T) & 
  (h_pilot$laboratory_yn == "yes") 
h_pilot$lab_tests_tb_microscopy_calc <- (h_pilot$lab_tests_tb_microscopy == T & 
                                           h_pilot$laboratory_yn == 'yes')
h_pilot$lab_tests_hiv_testing_calc <- (h_pilot$lab_tests_hiv_testing == T & 
                                         h_pilot$laboratory_yn == 'yes')
h_pilot$health_no_user_fees <- (h_pilot$paid_services_routine_visit == T | h_pilot$paid_services_lab_testing == T | 
                              h_pilot$paid_services_inpatient_stay == T | h_pilot$paid_services_medication == T | 
                              h_pilot$paid_services_routine_anc_visit == T | h_pilot$paid_services_contraceptives == T | 
                              h_pilot$paid_services_anc_delivery == T | h_pilot$paid_services_child_health == T | 
                              h_pilot$paid_services_hiv_treatment == T | h_pilot$paid_services_tb_treatment == T | 
                              h_pilot$paid_services_malaria_treatment == T)  




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



##these stay here...
h_pilot$transport_to_referral <-  as.character(ifelse(h_pilot$transport_to_referral_ambulance == T,
                                                      "ambulance",
                                                      ifelse(h_pilot$transport_to_referral_keke == T,
                                                             "keke",
                                                             NA_character_)))
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
                             "num_lab_techs_fulltime" = "lab_technicians_posted"))

#lga level stuff...
newname_pilot <- c("emoc_available_24_7", "paid_services_anc_delivery", 
                   "compr_oc_available_24_7", 
                   "paid_services_malaria_treatment", "paid_services_child_health")

#######
#adding/subtracting vars before cleaning 999
#######
#661
h_661$facility_owner_manager <- as.character(ifelse(h$facility_owner_manager.federalgovernment,
                                                    "federalgovrenment",
                                                    ifelse(h$facility_owner_manager.stategovernment,
                                                           "stategovrenment",
                                                           ifelse(h$facility_owner_manager.lga,
                                                                  "lga",
                                                                  ifelse(h$facility_owner_manager.private_forprofit,
                                                                         "private_forprofit",
                                                                         ifelse(h$facility_owner_manager.charitable_ngo,
                                                                                "private_notforprofit",
                                                                                ifelse(h$facility_owner_manager.religious_org,
                                                                                       "church_mission",
                                                                                       NA_character_)))))))

h_661$power_sources_grid <- (h_661$grid_proximity == 'connected_to_grid' | 
                               h_661$local_grid_proximity == 'connected_to_local_grid')

h_661$emoc_antibiotics_yn <- as.logical(recodeVar(h_661$emoc_antibiotics_yn, 
                                                  c('yes', 'no'),
                                                  c(TRUE, FALSE)))   

#droppin'
h_661 <- subset(h_661, select=-c(facility_owner_manager.private_forprofit, facility_owner_manager.charitable_ngo,
                                facility_owner_manager.religious_org, facility_owner_manager.stategovernment,
                                facility_owner_manager.lga, facility_owner_manager.none,
                                facility_owner_manager_other, facility_owner_manager.federalgovernment))      
h_pilot <-                  


##################################
#### combining 661, 113 & pilot
#################################
# edu_total <- rbind.fill(edu_661, edu_113, edu_pilot)




###############################################
####mapping values and standardize the type####
###############################################





  
  
  
  