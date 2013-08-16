## ALIASES / PREP ##
# slugs are at https://github.com/mvpdev/nmis/blob/develop/uis_r_us/indicators/overview.json
setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
source("scripts/InstallFormhub.R")
source("scripts/source_scripts/NMIS_Functions.R")

hh <- read.csv("in_process_data/nmis/data_661/Health_661_ALL_FACILITY_INDICATORS.csv", stringsAsFactors=FALSE)

#getting rid of erroneous lgas surveyed (that were part of 113)
# hh <- subset(hh, !(lga_id %in% c(133, 2, 208, 253, 415, 618, 699, 742, 753, 773)))


h <- hh
popu <- read.csv("lgas.csv")
row.names(popu) <- as.character(popu$lga_id)
# population figure within ddply is between the , and the last ) below
lgah_summaries <- ddply(h, .(lga_id), summarize, population = popu[as.character(lga_id[[1]]),'pop_2006'])
#other columns necessary for indicators
h$routine_immunization <- (h$not_for_private_2.immunization.bcg_immunization == T | 
                             h$not_for_private_2.immunization.bcg_immunization == T |
                             h$not_for_private_2.immunization.opv_immuization == T |
                             h$not_for_private_2.immunization.measles_immun == T |
                             h$not_for_private_2.immunization.dpt_immunization == T |
                             h$not_for_private_2.immunization.yellow_fever_immun == T |
                             h$not_for_private_2.immunization.csm_immunization == T |
                             h$not_for_private_2.immunization.hepb_immunization == T |
                             h$not_for_private_2.immunization.tetanus_immun == T)
h$health_no_child_user_fees <-                                           
  h$not_for_private_2.child_tx_fees_yn == 'no'
h$health_no_delivery_user_fees <-
  h$not_for_private_2.fees_adults.paid_services_anc_delivery == F
h$antenatal <-
  h$antenatal_care_yn == 'yes'
h$family <-
  h$family_planning_yn == 'yes'
h$separated <- h$not_for_private_1.waste_disposal.sharps_separated_yn == 'yes'

ih <- idata.frame(h)
# h <- subset(h, lga_id == 309 | lga_id == 310 |  
#               lga_id == 541 | lga_id == 715 | lga_id == 739)

####################
#####indicators#####
####################
#####Facilities#####
lgah_facilities <- ddply(ih, .(lga_id), 
          function(df) {
          data.frame(
            num_level_1_health_facilities = icount(df$facility_type == 'healthpostdispensary'),
            num_level_2_health_facilities = icount(df$facility_type ==
                                                           'primaryhealthclinic'),       
            num_level_3_health_facilities = icount(df$facility_type ==
                                                           'primaryhealthcarecentre'),
            num_level_4_health_facilities = icount(df$facility_type ==
                                                           'comprehensivehealthcentre'),
            num_level_other_health_facilities = icount(df$facility_type %in%
                                                               c('cottagehospital', 'specialisthospital',
                                                                 'wardmodelphccentre', 'teachinghospital',
                                                                 'dentalclinic', 'maternity', 'federalmedicalcentre',
                                                                 'generalhospital')), 
            num_health_facilities = icount(df$facility_type %in%
                                                   c('cottagehospital', 'specialisthospital', 'healthpostdispensary',
                                                     'wardmodelphccentre', 'teachinghospital',
                                                     'dentalclinic', 'maternity', 'federalmedicalcentre',
                                                     'generalhospital', 'comprehensivehealthcentre',
                                                     'primaryhealthcarecentre', 'primaryhealthclinic'
                                                   )),
            proportion_health_facilities_inpatient_care = 
              bool_proportion(df$inpatient_care_yn, TRUE),
            proportion_health_facilities_open_24_7 = 
              bool_proportion(df$facility_open_247_yn, TRUE),              
      num_doctors = sum(df$medical_staff_posted.num_doctors_posted, na.rm = TRUE),            
      num_nursemidwives_midwives = sum(df$medical_staff_posted.num_nursemidwives_posted, na.rm = TRUE) + 
              sum(df$medical_staff_posted.num_midwives_posted, na.rm = TRUE),            
      num_nurses = sum(df$medical_staff_posted.num_nurses_posted, na.rm = TRUE),            
      num_chews = sum(df$medical_staff_posted.num_chews_posted, na.rm = TRUE) + 
              sum(df$medical_staff_posted.num_junior_chews_posted, na.rm = TRUE),            
      num_lab_techs = sum(df$medical_staff_posted.lab_technicians_posted, na.rm = TRUE),             
      proportion_staff_paid = bool_proportion(df$staff_paid_lastmth_yn, TRUE),            
                        proportion_health_facilities_routine_immunization =
                          bool_proportion(df$routine_immunization, TRUE),                     
                        proportion_growth_monitoring = 
                          bool_proportion(df$child_health_growth_monitor, TRUE),            
                        #proportion_deworming =   NO DATA!
                        # ratio()            
                        proportion_no_user_fees_child_health = 
                          bool_proportion(df$health_no_child_user_fees, TRUE),                               
      proportion_delivery_24_7 =
        bool_proportion(df$maternal_health_delivery_services_24_7, TRUE),           
      proportion_at_least_1_sba = 
        bool_proportion(df$skilled_birth_attendant, TRUE),                    
      proportion_antenatal = 
        bool_proportion(df$antenatal, TRUE),                    
      num_health_facilities_c_sections = icount(df$compr_oc_c_sections),                    
      proportion_access_functional_emergency_transport = 
        bool_proportion(df$emergency_transport_currently_functioning, TRUE),                    
      proportion_family_planning = 
        bool_proportion(df$family, TRUE),                       
      proportion_delivery_no_user_fees = 
      bool_proportion(df$health_no_delivery_user_fees, TRUE),        
                    proportion_health_facilities_hiv_testing =
                      bool_proportion(df$lab_tests_hiv_testing_calc, TRUE),                    
                    proportion_malaria_testing = 
                      bool_proportion(df$malaria_testing, TRUE),  
                    proportion_act_treatment_for_malaria = 
                      bool_proportion(df$medication_anti_malarials, TRUE),
                    proportion_malaria_prevention_pregnancy = 
                      bool_proportion(df$sulpha_and_antenatal, TRUE),
                    proportion_offer_bednets = 
                      bool_proportion(df$has_itns, TRUE),                    
                    proportion_no_user_fees_malaria = 
                      bool_proportion(df$paid_services_malaria_treatment, FALSE),
            proportion_health_facilities_art_treatment =
              bool_proportion(df$malaria_treatment_artemisinin, TRUE),    
    proportion_health_facilities_tb_treatment = 
      bool_proportion(df$tb_treatment_yn, TRUE),                           
    proportion_health_facilities_tb_testing = 
      bool_proportion(df$lab_tests_tb_microscopy_calc, TRUE),
                    proportion_any_power_access = 
                      bool_proportion(df$power_access_and_functional, TRUE),                      
                    proportion_improved_water_source = 
                      bool_proportion(df$potable_water_access, TRUE),                        
                    proportion_functional_sanitation = 
                      bool_proportion(df$improved_sanitation_and_functional, TRUE),                        
                    proportion_mobile_coverage = 
                      bool_proportion(df$mobile_signal_funct_yn, TRUE),                    
                    proportion_health_facilities_med_waste_separated = 
                      bool_proportion(df$separated, TRUE),    
    proportion_stockout_essential_meds = 
    bool_proportion(df$essential_meds_stockout, TRUE),                        
                    num_skilled_health_providers_per_1000 = 
                      (sum(df$medical_staff_posted.num_doctors_posted, na.rm = TRUE) + 
                         sum(df$medical_staff_posted.num_nursemidwives_posted, na.rm = TRUE) +
                         sum(df$medical_staff_posted.num_nurses_posted, na.rm = TRUE) +
                         sum(df$medical_staff_posted.num_midwives_posted, na.rm = TRUE)) /
                          (popu[as.character(df$lga_id[[1]]),1]/1000),                    
                    num_chews_per_1000 = 
                      as.numeric((sum(df$medical_staff_posted.num_chews_posted, na.rm = TRUE) + 
                         sum(df$medical_staff_posted.num_junior_chews_posted, na.rm = TRUE))) /
                      as.numeric((popu[as.character(df$lga_id[[1]]),1]/1000))                    
                           )})                     

##########################
###### SUMMING UP ########
##########################
lga_health_all <- lgah_facilities
lgas <- subset(read.csv("lgas.csv"), select=c("lga_id", "lga", "state", "zone"))
lga_health_all <- merge(lga_health_all, lgas, by="lga_id")

write.csv(lga_health_all, "in_process_data/nmis/data_661/Health_LGA_level_661.csv", row.names=F)

