#Health Pilot: facility level

source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")

#######
#pilot#
#######
h_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Health_pilot_outliercleaned.csv",
                    stringsAsFactors=F)
h_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Health_661_NMIS_Facility.csv",
                  stringsAsFactors=F)
#to do list

subnm <- names(h_661)[which(names(h_661) %in% names(h_pilot))]
h_p <- subset(h_pilot, select=subnm)

nm_p <- names(h_pilot)[! names(h_pilot) %in% subnm]
nm_p <- c(nm_p, "uuid")

h_p_left <- subset(h_pilot, select=nm_p)
rm(subnm, nm_p)



#todo <- names(h_661)
hp <- h_pilot

#ID and other basic stuff...
h_p$mylga <- hp$lga
h_p$mylga_state <- hp$state 
h_p$mylga_zone <- hp$zone                                                              
h_p$formhub_photo_id <- hp$photo
h_p$gps <- hp$geocodeoffacility  

#################
####SNAPSHOT#####
###################
h_p$facility_name <- hp$facility_name
h_p$facility_type <- hp$facility_type
h_p$owner_manager <- ifelse(hp$facility_owner_manager == "stategovernment", "public", 
                            ifelse(hp$facility_owner_manager == "federalgovrenment", "public",
                                   ifelse(hp$facility_owner_manager == "lga", "public",
                                          ifelse(hp$facility_owner_manager == "private_forprofit", "private",
                                                 ifelse(hp$facility_owner_manager == "private_notforprofit", "private",
                                                        NA_character_)))))
h_p$maternal_health_delivery_services <- hp$emergency_obstetrics_yn=='yes'
h_p$skilled_birth_attendant <- (as.numeric(hp$num_doctors_fulltime) > 0 | 
  as.numeric(hp$num_midwives_fulltime) > 0 |
  as.numeric(hp$num_nursemidwives_fulltime) > 0)
h_p$num_chews_and_chos <- rowSums(hp[, c("num_chews_fulltime",
                                         "num_jr_chews_fulltime",
                                         "num_chos_fulltime")],na.rm=T)
h_p$vaccines_fridge_freezer <- hp$vaccines_stored_yn == "yes"
h_p$emergency_transport <- (hp$transport_to_referral_ambulance == T | hp$transport_to_referral_keke == T)
h_p$improved_water_supply <-   (hp$water_sources_yn_p == "yes")
h_p$improved_sanitation <- hp$num_toilets_improved_p >0
h_p$phcn_electricity <- hp$power_sources_grid == T
######################
####################

h_p$maternal_health_delivery_services_24_7 <- (hp$emergency_obstetrics_yn=='yes' & 
                                            (hp$compr_oc_available_24_7==T | hp$emoc_available_24_7 == T))

h_p$essential_meds_stockout <- (hp$antimalarials_stockout_yn == 'yes' | #hp$antidiarrheal_stockout_yn == 'yes' | 
                                  hp$antibiotics_stockout_yn == 'yes') 
h_p$emergency_transport_currently_functioning <- (hp$equipment_emergency_transport == T & hp$daily_pub_transport_p == 'yes')
                                     

hp$num_jr_chews_fulltime <- as.numeric(hp$num_jr_chews_fulltime)
hp$num_chews_fulltime <- as.numeric(hp$num_chews_fulltime)
hp$num_chos_fulltime <- as.numeric(hp$num_chos_fulltime)
               
h_p$power_access_and_functional <- (((hp$power_sources_generator == T &
                                        hp$generator_funct_yn == 'yes') |
                                       (hp$power_sources_solar == T &
                                          hp$solar_funct_yn == 'yes') |
                                       (hp$power_sources_grid == T &
                                          hp$grid_funct_yn == 'yes')) &
                                      (as.numeric(hp$days_no_electricity) <= 7)) 
#other indicators  
h_p$num_chews_total <-  rowSums(hp[,c('num_chews_fulltime', 
                                      'num_jr_chews_fulltime')], na.rm=T)               
h_p$sulpha_and_antenatal <- (hp$malaria_treatment_sulphadoxine == T & (hp$antenatal_care_yn == "yes"))
h_p$has_itns <- (hp$malaria_treatment_yn == 'yes' & (hp$malaria_treatment_srvcs_itn == T 
                                                     | hp$supplies_available_bednets == T))                                 
#h_p$medication_iron_tablets
#h_p$medication_folic_acid 
#h_p$scale_yn                   
#h_p$equipment_bp_machine
h_p$hiv_tx_srvcs_pmtct_services_calc <-  hp$sti_treatment_yn == 'yes' 
h_p$lab_tests_hemoglobin_testing_calc <- (hp$laboratory_yn == 'yes' &
                                            hp$lab_tests_hemoglobin_testing == T)       
h_p$lab_tests_urine_testing_calc <- (hp$laboratory_yn == 'yes' &
                                       hp$lab_tests_urine_testing == T)
h_p$iv_antibiotics_yn_calc <- (hp$comprehensive_obstetrics_yn == 'yes' & 
                                 hp$emoc_antibiotics == T)                  
#h_p$emoc_vacuum_extractor                    
#h_p$emoc_forceps
#h_p$compr_oc_c_sections
#h_p$uterotonics_yn_calc                      
#h_p$antishock_garment_yn
#h_p$emoc_parenteral_anticonvulsant
hp$num_doctors_fulltime <- as.numeric(hp$num_doctors_fulltime)
hp$num_midwives_fulltime <- as.numeric(hp$num_midwives_fulltime)
hp$num_nursemidwives_fulltime <- as.numeric(hp$num_nursemidwives_fulltime)
h_p$at_least_two_skilled_birth_attendants <- rowSums(hp[, c('num_doctors_fulltime', 
                                                            'num_midwives_fulltime', 'num_nursemidwives_fulltime')], na.rm=T) >= 2     
h_p$at_least_three_skilled_birth_attendants <- rowSums(hp[, c('num_doctors_fulltime', 
                                                              'num_midwives_fulltime', 'num_nursemidwives_fulltime')], na.rm=T) >= 3     
h_p$at_least_four_skilled_birth_attendants <- rowSums(hp[, c('num_doctors_fulltime', 
                                                             'num_midwives_fulltime', 'num_nursemidwives_fulltime')], na.rm=T) >= 4     
#h_p$compr_oc_blood_transfusions              
h_p$condoms_yn <- (hp$sti_tx_srvcs_condoms == T | 
                     hp$hiv_tx_srvcs_condoms == T | 
                     hp$supplies_available_condoms == T)
h_p$family_planning_pill_calc_calc <- (hp$family_planning_pill == T  & hp$family_planning_yn == 'yes') 
h_p$family_planning_injectables_calc_calc <- (hp$family_planning_pill == T & hp$family_planning_yn == 'yes')                                                
h_p$family_planning_iud_calc <- (hp$family_planning_iud == T & hp$family_planning_yn == 'yes')  
h_p$family_planning_implants_calc <- (hp$family_planning_implants == T & hp$family_planning_yn == 'yes')
h_p$sterilization_yn_calc <- (hp$family_planning_sterilization_m == T & hp$family_planning_yn == 'yes')                                     
h_p$child_health_measles_immun_calc <- (hp$child_health_immunization_p == T & hp$child_health_yn == 'yes')  
h_p$child_health_opv_immuization_calc <- (hp$child_health_immunization_p == T & hp$child_health_yn == 'yes')  
h_p$child_health_dpt_immunization_calc <- (hp$child_health_immunization_p == T & hp$child_health_yn == 'yes')  
h_p$child_health_tetanus_immun_calc <- (hp$child_health_immunization_p == T & hp$child_health_yn == 'yes')  
h_p$child_health_hepb_immunization_calc <- (hp$child_health_immunization_p == T & hp$child_health_yn == 'yes')  
h_p$child_health_bcg_immunization_calc  <- (hp$child_health_immunization_p == T & hp$child_health_yn == 'yes')       
h_p$child_health_yellow_fever_immun_calc <- (hp$child_health_immunization_p == T & hp$child_health_yn == 'yes')  
h_p$child_health_csm_immunization_calc <- (hp$child_health_immunization_p == T & hp$child_health_yn == 'yes')  
#h_p$vaccines_icepack_calc
h_p$malaria_testing <- (hp$lab_tests_malaria_rdt == T| hp$lab_tests_malaria_microscopy == T) & (hp$laboratory_yn == "yes")
#h_p$oral_antibiotics_calc
#h_p$medication_antihistamines                
#h_p$medication_iv_fluid
h_p$lab_tests_pregnancy_calc <- (hp$lab_tests_pregnancy == T & hp$laboratory_yn == 'yes')   
h_p$lab_tests_stool_calc <- (hp$lab_tests_pregnancy == T & hp$laboratory_yn == 'yes')                                              
h_p$potable_water_access <-  (as.numeric(hp$days_no_potable_water_pastmth)< 23)


#h_p$iv_medications_yn        
h_p$lab_tests_tb_microscopy_calc <- (hp$lab_tests_tb_microscopy == T & hp$laboratory_yn == 'yes')                           
h_p$lab_tests_hiv_testing_calc <- (hp$lab_tests_hiv_testing == T & hp$laboratory_yn == 'yes') 
h_p$health_no_user_fees <- (hp$paid_services_routine_visit == T | hp$paid_services_lab_testing == T | 
                              hp$paid_services_inpatient_stay == T | hp$paid_services_medication == T | 
                              hp$paid_services_routine_anc_visit == T | hp$paid_services_contraceptives == T | 
                              hp$paid_services_anc_delivery == T | hp$paid_services_child_health == T | 
                              hp$paid_services_hiv_treatment == T | hp$paid_services_tb_treatment == T | 
                              hp$paid_services_malaria_treatment == T)  



#Adding distant to every facility
#combining calculated result back to original data
h_p <- lga_boudary_dist(h_p, gps_col="gps")
h_pilot_comp <- h_p
hp <- merge(h_p, h_p_left, by="uuid")


#Delete all those have dist >= 35 km
h_pilot_comp <- subset(h_pilot_comp, dist_fake <= 35 | is.na(dist_fake))
hp <- subset(hp, dist_fake <= 35 | is.na(dist_fake))





#writing
write.csv(x_y_killa(h_pilot_comp), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Health_Pilot_NMIS_Facility.csv", row.names=F)
write.csv(x_y_killa(hp),"~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Health_Pilot_ALL_FACILITY_INDICATORS.csv", row.names=F)

