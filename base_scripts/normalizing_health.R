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

#adding source column 
h_661$src <- "661"
h_113$src <- "113"
h_pilot$src <- "pilot"

#adding uuid to 113 + pilot
h_113$uuid <- sapply(paste(h_113$gps, h_113$photo), FUN=digest)
h_pilot$uuid <- sapply(paste(h_pilot$gps, h_pilot$photo), FUN=digest)


####
common_slugs_113 <- names(h_661)[(which(names(h_661) %in% names(h_113)))]
h_common <- common_slug(c("h_113", "h_661", "h_pilot"))
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
                         "fees_adults.paid_services_malaria_treatment" = "paid_services_malaria_treatment"
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



##pilot

h_pilot <- rename(h_pilot, c("" = "", 
                             
                             
                             
                             ))


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

#######
#Adding Few vars before cleaning 999
#######
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

##drop facility_owner_manager.federalgovernment etc. etc.



#fix!

h_661$emoc_antibiotics_yn <- as.logical(recodeVar(h_661$emoc_antibiotics_yn, 
                                                c('yes', 'no'),
                                                  c(TRUE, FALSE)))   


##################################
#### combining 661, 113 & pilot
#################################
# edu_total <- rbind.fill(edu_661, edu_113, edu_pilot)




###############################################
####mapping values and standardize the type####
###############################################





  
  
  
  