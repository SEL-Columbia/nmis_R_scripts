#Health 113: lga level

## ALIASES / PREP ##
# slugs are at https://github.com/mvpdev/nmis/blob/develop/uis_r_us/indicators/overview.json
setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
source("scripts/InstallFormhub.R")
source("scripts/source_scripts/NMIS_Functions.R")

################
######113#######
################
hh <- read.csv("in_process_data/nmis/data_113/Health_113_ALL_FACILITY_INDICATORS.csv", stringsAsFactors=FALSE)
h <- hh
pop <- read.csv("lgas.csv")
# population figure within ddply is between the , and the last ) below
lgah_summaries <- ddply(h, .(lga_id), summarize, population = pop[as.character(lga_id[[1]]),'pop_2006'])
#other columns necessary for indicators
h$routine_immunization <- (h$child_health_bcg_immunization_calc == T | 
                             h$child_health_opv_immuization_calc == T |
                             h$child_health_measles_immun_calc == T |
                             h$child_health_dpt_immunization_calc == T |
                             h$child_health_yellow_fever_immun_calc == T |
                             h$child_health_csm_immunization_calc == T |
                             h$child_health_hepb_immunization_calc == T |
                             h$child_health_tetanus_immun_calc == T)

# h$health_no_child_user_fees <-                                           
#   h$not_for_private_2.child_tx_fees_yn == 'no'
# h$health_no_delivery_user_fees <-
#   h$not_for_private_2.fees_adults.paid_services_anc_delivery == F
h$antenatal <-
  h$antenatal_care_yn == 'yes'
h$family <-
  h$family_planning_yn == 'yes'
h$separated <- h$med_waste_separated_yn == 'yes'
ih <- idata.frame(h)
# h <- subset(h, lga_id == 309 | lga_id == 310 |  
#               lga_id == 541 | lga_id == 715 | lga_id == 739)


#####indicators#####
lgah_facilities <- ddply(ih, .(lga_id), 
                         function(df) {
                           data.frame(
                             num_level_1_health_facilities = icount(df$facility_type %in% c('dispensary',
                                                                                            'healthpost')),
                             num_level_2_health_facilities = icount(df$facility_type ==
                                                                      'primaryhealthclinic'),       
                             num_level_3_health_facilities = icount(df$facility_type ==
                                                                      'primaryhealthcarecentre'),
                             num_level_4_health_facilities = icount(df$facility_type ==
                                                                      'comprehensivehealthcentre'),
                             num_level_other_health_facilities = icount(df$facility_type %in%
                                                                          c('cottagehospital', 'specialisthospital',
                                                                            'wardmodelprimaryhealthcarecentre',
                                                                            'maternity', 'generalhospital', 'other',
                                                                            'federalmedicalcare', 'private', 
                                                                            'dentalclinic')), 
                             num_health_facilities = icount(df$facility_type %in%
                                                              c('cottagehospital', 'specialisthospital', 'healthpost',
                                                                'dispensary', 'other', 'generalhospital',
                                                                'wardmodelprimaryhealthcarecentre', 'maternity',
                                                                'federalmedicalcentre', 'comprehensivehealthcentre',
                                                                'primaryhealthcarecentre', 'primaryhealthclinic'
                                                              )),
                             proportion_health_facilities_inpatient_care = 
                               bool_proportion(df$inpatient_care_yn, TRUE),
                             proportion_health_facilities_open_24_7 = 
                               bool_proportion(df$facility_open_247_yn, TRUE),              
                             num_doctors = sum(as.numeric(df$num_doctors_fulltime), na.rm = TRUE),            
                             num_nursemidwives_midwives = sum(as.numeric(df$num_nursemidwives_fulltime), na.rm = TRUE) + 
                               sum(as.numeric(df$num_midwives_fulltime), na.rm = TRUE),            
                             num_nurses = sum(as.numeric(df$num_nurses_fulltime), na.rm = TRUE),            
                             num_chews = sum(as.numeric(df$num_chews_fulltime), na.rm = TRUE) + 
                               sum(as.numeric(df$num_jr_chews_fulltime), na.rm = TRUE),            
                             num_lab_techs = sum(as.numeric(df$num_lab_techs_fulltime), na.rm = TRUE),             
                             proportion_staff_paid = bool_proportion(df$staff_paid_lastmth_yn, TRUE),            
                             proportion_health_facilities_routine_immunization =
                               bool_proportion(df$routine_immunization == T, TRUE),                     
                             proportion_growth_monitoring = 
                               bool_proportion(df$child_health_growth_monitor == T, TRUE),                                         
                             proportion_deworming = bool_proportion(df$child_health_deworming == T, TRUE),                             
                             proportion_no_user_fees_child_health = 
                               bool_proportion(df$health_no_user_fees == T, TRUE),
                             proportion_at_least_1_sba = 
                               bool_proportion(df$skilled_birth_attendant == T, TRUE),                    
                             proportion_antenatal = 
                               bool_proportion(df$antenatal == T, TRUE),                    
                             proportion_delivery_24_7 =
                               bool_proportion(df$compr_oc_available_24_7 == 'yes' & 
                                                 df$emoc_available_24_7 == 'yes' , TRUE),   
                             #num_health_facilities_c_sections = icount(df$compr_oc_c_sections),                    
                             proportion_access_functional_emergency_transport = 
                               bool_proportion(df$emergency_transport_currently_functioning == T, TRUE),                    
                             proportion_family_planning = 
                               bool_proportion(df$family == T, TRUE),                       
                             proportion_delivery_no_user_fees = 
                               bool_proportion(df$health_no_user_fees == T, TRUE),        
                             proportion_health_facilities_hiv_testing =
                               bool_proportion(df$lab_tests_hiv_testing_calc == T, TRUE),                    
                             proportion_malaria_testing = 
                               bool_proportion(df$malaria_testing == T, TRUE),  
                             proportion_health_facilities_art_treatment =
                               bool_proportion(df$malaria_treatment_artemisinin, TRUE),    
                             proportion_act_treatment_for_malaria = 
                               bool_proportion(df$medication_anti_malarials == T, TRUE),
                             proportion_malaria_prevention_pregnancy = 
                               bool_proportion(df$sulpha_and_antenatal == T, TRUE),
                             proportion_offer_bednets = 
                               bool_proportion(df$has_itns == T, TRUE),                    
                             proportion_no_user_fees_malaria = 
                               bool_proportion(df$paid_services_malaria_treatment == T, FALSE),    
                             proportion_health_facilities_tb_treatment = 
                               bool_proportion(df$tb_treatment_yn , TRUE),                           
                             proportion_health_facilities_tb_testing = 
                               bool_proportion(df$lab_tests_tb_microscopy_calc == T, TRUE),
                             proportion_any_power_access = 
                               bool_proportion(df$power_access_and_functional == T, TRUE),                      
                             proportion_improved_water_source = 
                               bool_proportion(df$potable_water_access == T, TRUE),                        
                            proportion_functional_sanitation = 
                              bool_proportion(df$improved_sanitation_and_functional, TRUE),                        
                             proportion_mobile_coverage = 
                               bool_proportion(df$mobile_signal_funct_yn == T, TRUE),                    
                             proportion_health_facilities_med_waste_separated = 
                               bool_proportion(df$separated == T, TRUE),    
                            proportion_stockout_essential_meds = 
                              bool_proportion(df$essential_meds_stockout == F, TRUE),                        
                             num_skilled_health_providers_per_1000 = 
                               (sum(as.numeric(df$num_doctors_fulltime), na.rm = TRUE) + 
                                  sum(as.numeric(df$num_nursemidwives_fulltime), na.rm = TRUE) +
                                  sum(as.numeric(df$num_nurses_fulltime), na.rm = TRUE) +
                                  sum(as.numeric(df$num_midwives_fulltime), na.rm = TRUE)) /
                               (pop[as.character(df$lga_id[[1]]),1]/1000),                    
                             num_chews_per_1000 = 
                               (sum(as.numeric(df$num_chews_fulltime), na.rm = TRUE) + 
                                  sum(as.numeric(df$num_jr_chews_fulltime), na.rm = TRUE)) /
                               (pop[as.character(df$lga_id[[1]]),1]/1000)
                             
                           )})                     

##########################
###### SUMMING UP ########
##########################
lga_health_all <- lgah_facilities
lga_health_all <- rename(lga_health_all, c("lga_id"="lga_id"))
lgas <- subset(read.csv("lgas.csv"), select=c("lga_id", "lga", "state", "zone"))
lga_health_all1 <- merge(lga_health_all, lgas, by="lga_id")

write.csv(lga_health_all1, "in_process_data/nmis/data_113/Health_LGA_level_113.csv", row.names=F)



