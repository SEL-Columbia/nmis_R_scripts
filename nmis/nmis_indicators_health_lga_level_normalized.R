#####################################################################################################################
##Normalized Health NMIS LGA Level ##################################################################################
#####################################################################################################################
# slugs are at https://github.com/mvpdev/nmis/blob/develop/uis_r_us/indicators/overview.json
source("base_scripts/InstallFormhub.R")
source("source_scripts/NMIS_Functions.R")

health_774 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Normalized/Health_774_ALL_FACILITY_INDICATORS.rds")

#reading in data for population based calculations 
popu <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv")
row.names(popu) <- as.character(popu$lga_id)
  # population figure within ddply is between the , and the last ) below
lgah_summaries <- ddply(health_774, .(lga_id), summarize, population = popu[as.character(lga_id[[1]]),'pop_2006'])

#changing into idata.frame
ihealth774 <- idata.frame(health_774)

####################
#####indicators#####
####################

lga_health_data <- ddply(ihealth774, .(lga_id), function(df) {
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
      num_doctors = sum(df$num_doctors_posted, na.rm = TRUE),            
      num_nursemidwives_midwives = sum(df$num_nursemidwives_posted, na.rm = TRUE) + 
              sum(df$num_midwives_posted, na.rm = TRUE),            
      num_nurses = sum(df$num_nurses_posted, na.rm = TRUE),            
      num_chews = sum(df$num_chews_posted, na.rm = TRUE) + 
              sum(df$num_junior_chews_posted, na.rm = TRUE),            
      num_lab_techs = sum(df$lab_technicians_posted, na.rm = TRUE),             
      proportion_staff_paid = bool_proportion(df$staff_paid_lastmth_yn, TRUE),            
            proportion_health_facilities_routine_immunization =
              bool_proportion(df$routine_immunization, TRUE),                     
            proportion_growth_monitoring = 
              bool_proportion(df$child_health_growth_monitor, TRUE),            
            proportion_deworming = 
              bool_proportion(df$child_health_deworming, TRUE),
            proportion_no_user_fees_child_health = 
              bool_proportion(df$health_no_child_user_fees, TRUE),                               
      proportion_delivery_24_7 =
        bool_proportion(df$maternal_health_delivery_services_24_7, TRUE),           
      proportion_at_least_1_sba = 
        bool_proportion(df$skilled_birth_attendant, TRUE),                    
      proportion_antenatal = 
        bool_proportion(df$antenatal_care_yn, TRUE),                    
      num_health_facilities_c_sections = icount(df$compr_oc_c_sections),                    
      proportion_access_functional_emergency_transport = 
        bool_proportion(df$emergency_transport_currently_functioning, TRUE),                    
      proportion_family_planning = 
        bool_proportion(df$family_planning_yn, TRUE),                       
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
          bool_proportion(df$paid_services_malaria_treatment, TRUE),
            proportion_health_facilities_art_treatment =
              bool_proportion(df$malaria_treatment_artemisinin, TRUE),    
    proportion_health_facilities_tb_treatment = 
      bool_proportion(df$tb_treatment_yn, TRUE),                           
    proportion_health_facilities_tb_testing = 
      bool_proportion(df$lab_tests_tb_microscopy_calc, TRUE),
            proportion_any_power_access = 
              bool_proportion(df$power_access_and_functional, TRUE),                      
            proportion_improved_water_supply =
              bool_proportion(df$improved_water_supply, TRUE),                        
            proportion_improved_sanitation = 
              bool_proportion(df$improved_sanitation, TRUE),                        
            proportion_mobile_coverage = 
              bool_proportion(df$mobile_signal_funct_yn, TRUE),                    
            proportion_health_facilities_med_waste_separated = 
              bool_proportion(df$med_waste_separated_yn, TRUE),    
    proportion_stockout_essential_meds = 
    bool_proportion(df$essential_meds_stockout, TRUE),                        
          num_skilled_health_providers_per_1000 = 
            (sum(df$num_doctors_posted, na.rm = TRUE) + 
               sum(df$num_nursemidwives_posted, na.rm = TRUE) +
               sum(df$num_nurses_posted, na.rm = TRUE) +
               sum(df$num_midwives_posted, na.rm = TRUE)) /
                (popu[as.character(df$lga_id[[1]]),1]/1000),                    
          num_chews_per_1000 = 
            (sum(df$num_chews_posted, na.rm = TRUE) + 
               sum(df$num_junior_chews_posted, na.rm = TRUE)) /
            (popu[as.character(df$lga_id[[1]]),1]/1000)
                           )})                     

###### SUMMING UP #########
lga_health_all <- lga_health_data

##writing out##
saveRDS(x_y_killa(lga_health_all), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Normalized/normalized_final/Health_LGA_level_774.rds")




