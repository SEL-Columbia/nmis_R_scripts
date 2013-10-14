##########
##Health##
##########
setwd("~/Code/nmis_R_scripts/")
require(plyr)
require(doBy)
require(digest)

h_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Health_661_Merged.csv", stringsAsFactors=F)
h_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Health_PhII_RoundI&II&III_Clean_2011.10.21.csv",
                  stringsAsFactors=F, na.strings = c("NA", "n/a"))
h_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Data_Health_Clean_2011.11.18.csv",
                    stringsAsFactors=F, na.strings = c("NA", "n/a"))

#adding source column 
h_661$src <- "661"
h_113$src <- "113"
h_pilot$src <- "pilot"

#adding uuid to 113 + pilot
h_113$uuid <- sapply(paste(h_113$gps, h_113$photo), FUN=digest)
h_pilot$uuid <- sapply(paste(h_pilot$gps, h_pilot$photo), FUN=digest)

####
common_slugs_113 <- names(h_661)[(which(names(h_661) %in% names(h_113)))]


slugsearch <- function(nm, df=h_661){
  names(df)[grep(nm, names(df), ignore.case=T)]
}
see <- function(nm, df=h_113)
{
  table(df[,nm],exclude=NULL)
}

na_num <- function(vec) length(which(is.na(vec)))

na_prop <- function(vec) {
  print(class(vec)) 
  na_num(vec)/length(vec)
}

common_slug <- function(slug, df_names = c("h_113", "h_661", "h_pilot"))
{
  dfs <- lapply(df_names, function(x) get(x))
  names(dfs) <- df_names
  
  flgs <- sapply(dfs, function(x) slug %in% names(x))
  
  if(all(flgs) == T){
    sprintf("%s is contained in all data sets",slug)
  }
  else{
    sprintf("%s does NOT have slug:   %s", paste(names(dfs)[!flgs], collapse=", "), slug)
  }
}


# length(which(names(h_113) %in% names(h_661)))
# length(which(names(h_pilot) %in% names(h_113)))



#### adding new variable to 661
####
### mapping 113 names


#go through cleaning 999's and run commun_slug


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


#go back to rest of pipeline and do these changes?? nah. 

mapped_113 <- c("num_bucket_system", "num_flush_other", "num_vip_latrine", "num_pit_w_slab", "lab_tests.pregnancy",
                "num_open_pit_latrine", "equipment.scale", "equipment.bp_machine", "lab_tests.hemoglobin_testing",
                "lab_tests_urine_testing", "supplies.needles_and_tubing", "equipment.emoc_antishock_garment",
                "medication.oral_contraceptives", "medication.injectable_contracept", "immunization.measles_immun",
                "immunization.opv_immuization", "immunization.dpt_immunization", "immunization.tetanus_immun",
                "immunization.csm_immunization", "lab_tests.malaria_rdt", "lab_tests.malaria_microscopy", 
                "lab_tests.tb_microscopy", "lab_tests.hiv_testing", "medication_iv_fluid")


newname_113 <- 
facility_owner_manager
num_doctors_fulltime
num_midwives_fulltime
num_nursemidwives_fulltime
num_chews_fulltime
num_jr_chews_fulltime
num_chos_fulltime
vaccines_strg_type  
emergency_transport_ambulance
emergency_transport_keke_napep
water_sources_borehole_tube_well
water_sources_tap_outside
water_sources_tap_in_compound
toilet_types_vip_latrine
toilet_types_pit_w_slab
toilet_types_flush_or_pour_flush
power_sources_grid
compr_oc_available_24_7
emoc_available_24_7
malaria_treatment_sulphadoxine
antimalarials_stockout_yn
antibiotics_stockout_yn
equipment_emergency_transport
public_transport_funct_yn
power_sources_generator
power_sources_solar
power_sources_grid
malaria_treatment_yn
malaria_treatment_srvcs_itn
sti_treatment_yn
hiv_tx_srvcs_pmtct_services
emoc_parenteral1
emoc_antibiotics #yn equivalent in 661 (this is TF) => emoc_antibiotics_yn
comprehensive_obstetrics_yn
emoc_antibiotics
medication_oxytocin
emoc_uterotonic2
emoc_oxytocin
emoc_misoprotol
compr_oc_oxytocin
compr_oc_misoprotol
compr_oc_antishock_garment
sti_tx_srvcs_condoms
hiv_tx_srvcs_condoms
supplies_available_condoms
child_health_yn
vaccines_stored_yn #yn equivalent in 661 (this is TF) => vaccine_storage_yn
paid_services_inpatient_stay #yn equiv => inpatient_care_yn
child_health_vaccine_carriers
sti_tx_srvcs_penicilling
sti_tx_srvcs_doxycycline
sti_tx_srvcs_ciprofloxacin
sti_treatment_yn
child_health_ampicillin
child_health_ciprofloxain
child_health_yn
medication_anti_biotics
toilet_types_vip_latrine
toilet_types_pit_w_slab
toilet_types_flush_or_pour_flush
flush_toilet_drain_to
vip_latrine_not_working
slab_pit_latrine_not_working
toilet_types_flush_or_pour_flush
flush_toilet_drain_to
flush_toilet_not_working
paid_services_child_health
paid_services_hiv_treatment
paid_services_tb_treatment







  
  
  
  