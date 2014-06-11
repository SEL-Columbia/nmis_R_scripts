# slugs are at https://github.com/mvpdev/nmis/blob/develop/uis_r_us/indicators/overview.json
source("source_scripts/NMIS_Functions.R")
source("1_base_scripts/InstallFormhub.R")

health_774 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/normalized/Health_774_ALL_FACILITY_INDICATORS.rds")

#changing into idata.frame
ihealth774 <- idata.frame(health_774)

####################
#####indicators#####
####################

lga_health_data <- ddply(ihealth774, .(lga_id), function(df) {
          data.frame(
            num_level_1_health_facilities = sum(df$facility_type == 'healthpostdispensary', na.rm=T),
            num_level_2_health_facilities = sum(df$facility_type ==
                                                           'primaryhealthclinic', na.rm=T),       
            num_level_3_health_facilities = sum(df$facility_type %in%
                                                           c('primaryhealthcarecentre', 
                                                             'wardmodelphccentre'), na.rm=T),
            num_level_4_health_facilities = sum(df$facility_type ==
                                                           'comprehensivehealthcentre', na.rm=T),
            num_level_other_health_facilities = sum(df$facility_type %in%
                                                               c('cottagehospital', 'specialisthospital',
                                                                 'teachinghospital', 'dentalclinic', 
                                                                 'maternity', 'federalmedicalcentre',
                                                                 'generalhospital'), na.rm=T), 
            num_health_facilities = sum(df$facility_type %in%
                                                   c('cottagehospital', 'specialisthospital', 'healthpostdispensary',
                                                     'wardmodelphccentre', 'teachinghospital',
                                                     'dentalclinic', 'maternity', 'federalmedicalcentre',
                                                     'generalhospital', 'comprehensivehealthcentre',
                                                     'primaryhealthcarecentre', 'primaryhealthclinic'), 
                                                    na.rm=T),
            proportion_health_facilities_inpatient_care = 
              mean(df$inpatient_care_yn, na.rm=T),
            proportion_health_facilities_open_24_7 = 
              mean(df$facility_open_247_yn, na.rm=T),              
      num_doctors = sum(df$num_doctors_posted, na.rm = TRUE),            
      num_nursemidwives_midwives = sum(df$num_nursemidwives_posted, na.rm = TRUE) + 
              sum(df$num_midwives_posted, na.rm = TRUE),            
      num_nurses = sum(df$num_nurses_posted, na.rm = TRUE),            
      num_chews = sum(df$num_chews_posted, na.rm = TRUE) + 
              sum(df$num_junior_chews_posted, na.rm = TRUE),            
      num_lab_techs = sum(df$lab_technicians_posted, na.rm = TRUE),             
      proportion_staff_paid = mean(df$staff_paid_lastmth_yn, na.rm=T),            
            proportion_health_facilities_routine_immunization =
              mean(df$routine_immunization, na.rm=T),                     
            proportion_growth_monitoring = 
              mean(df$child_health_growth_monitor, na.rm=T),            
            proportion_deworming = 
              mean(df$child_health_deworming, na.rm=T),
            proportion_no_user_fees_child_health = 
              mean(df$health_no_child_user_fees, na.rm=T),                               
      proportion_delivery_24_7 =
        mean(df$maternal_health_delivery_services_24_7, na.rm=T),           
      proportion_at_least_1_sba = 
        mean(df$skilled_birth_attendant, na.rm=T),                    
      proportion_antenatal = 
        mean(df$antenatal_care_yn, na.rm=T),                    
      num_health_facilities_c_sections = sum(df$compr_oc_c_sections, na.rm=T),                    
      proportion_access_functional_emergency_transport = 
        mean(df$emergency_transport_currently_functioning, na.rm=T),                    
      proportion_family_planning = 
        mean(df$family_planning_yn, na.rm=T),                       
      proportion_delivery_no_user_fees = 
      mean(df$health_no_delivery_user_fees, na.rm=T),        
        proportion_health_facilities_hiv_testing =
          mean(df$lab_tests_hiv_testing_calc, na.rm=T),                    
        proportion_malaria_testing = 
          mean(df$malaria_testing, na.rm=T),  
        proportion_act_treatment_for_malaria = 
          mean(df$medication_anti_malarials, na.rm=T),
        proportion_malaria_prevention_pregnancy = 
          mean(df$sulpha_and_antenatal, na.rm=T),
        proportion_offer_bednets = 
          mean(df$has_itns, na.rm=T),                    
        proportion_no_user_fees_malaria = 
          mean(df$paid_services_malaria_treatment, na.rm=T),
            proportion_health_facilities_art_treatment =
              mean(df$malaria_treatment_artemisinin, na.rm=T),    
    proportion_health_facilities_tb_treatment = 
      mean(df$tb_treatment_yn, na.rm=T),                           
    proportion_health_facilities_tb_testing = 
      mean(df$lab_tests_tb_microscopy_calc, na.rm=T),
            proportion_any_power_access = 
              mean(df$power_access_and_functional, na.rm=T),                      
            proportion_improved_water_supply =
              mean(df$improved_water_supply, na.rm=T),                        
            proportion_improved_sanitation = 
              mean(df$improved_sanitation, na.rm=T),                        
            proportion_mobile_coverage = 
              mean(df$mobile_signal_funct_yn, na.rm=T),                    
            proportion_health_facilities_med_waste_separated = 
              mean(df$med_waste_separated_yn, na.rm=T),    
    proportion_stockout_essential_meds = 
    mean(df$essential_meds_stockout, na.rm=T),                        
          num_skilled_health_providers_per_1000 = 
            (sum(df$num_doctors_posted, na.rm = TRUE) + 
               sum(df$num_nursemidwives_posted, na.rm = TRUE) +
               sum(df$num_nurses_posted, na.rm = TRUE) +
               sum(df$num_midwives_posted, na.rm = TRUE)) /
                (df$pop_2006[1]/1000),                    
          num_chews_per_1000 = 
            (sum(df$num_chews_posted, na.rm = TRUE) + 
               sum(df$num_junior_chews_posted, na.rm = TRUE)) /
            (df$pop_2006[1]/1000)
                           )})                     

###### core indicator calculations ############################################################
# Services that are provided at Hospitals only
  #subsetting data by facility type
  health_774_hospitals <- health_774[(health_774$facility_type == 'cottagehospital' |
                                   health_774$facility_type == 'specialisthospital' |
                                   health_774$facility_type == 'comprehensivehealthcentre' |
                                   health_774$facility_type == 'generalhospital' |
                                   health_774$facility_type == 'teachinghospital' |
                                   health_774$facility_type == 'federalmedicalcentre'),]                                        
   ihealth_774_hospitals<- idata.frame(health_774_hospitals)
 
 lga_health_data_core_hospital <- ddply(ihealth_774_hospitals, .(lga_id),
                                                    function(df) {data.frame(
                                                          #   Total number of hospitals in LGA
                                                         num_hospitals = length(df$uuid),
                                                          #   Percentage that perform C-sections
                                                         percent_compr_oc_c_sections = 
                                                                mean(ihealth_774_hospitals$compr_oc_c_sections, na.rm=T)
                                                        )}) 

                                   
# Services that are provided at all faciltiies except for Health Posts 
  #subsetting data by facility type
  sansHP_health_774 <- health_774[(health_774$facility_type == 'cottagehospital' |
                                    health_774$facility_type == 'specialisthospital' |
                                    health_774$facility_type == 'wardmodelphccentre' |
                                    health_774$facility_type == 'teachinghospital' |
                                    health_774$facility_type == 'dentalclinic' |
                                    health_774$facility_type == 'maternity' |
                                    health_774$facility_type == 'federalmedicalcentre' |
                                    health_774$facility_type == 'generalhospital' |
                                    health_774$facility_type == 'comprehensivehealthcentre' |
                                    health_774$facility_type == 'primaryhealthcarecentre' |
                                    health_774$facility_type == 'primaryhealthclinic'),]
 isansHP_health_774 <- idata.frame(sansHP_health_774)

lga_health_data_core_sansHP <- ddply(isansHP_health_774, .(lga_id), function(df) {
        data.frame(
          num_health_facilities_sansHP = 
            length(df$uuid),
      #     Percentage that were intended to conduct deliveries for pregnant women    
          proportion_delivery_24_7_sansHP = 
            mean(df$maternal_health_delivery_services_24_7, na.rm=T),
          proportion_vaccines_fridge_freezer_sansHP = 
            mean(df$vaccines_fridge_freezer, na.rm=T)          
      )}) 

#rest of core indicators:


lga_health_data_core <- ddply(ihealth774, .(lga_id), function(df) {
    data.frame( 
      # Services that are provided at all facilities including Health Posts
      proportion_measles = 
        mean(df$child_health_measles_immun_calc, na.rm=T),
      # Infrastructure -- All facilities                
      proportion_phcn_electricity = 
        mean(df$phcn_electricity, na.rm=T),
      proportion_alternative_power = 
        mean(df$phcn_electricity, na.rm=T),
      proportion_power_alternative_functional = 
        mean(df$power_sources_alternative_functional, na.rm=T),
      #Health Facilities summary 
      facilities_delivery_services_yn = 
        sum(df$delivery_services_yn, na.rm=T),
      facilities_emergency_transport = 
        sum(df$emergency_transport, na.rm=T),
      facilities_skilled_birth_attendant = 
        sum(df$skilled_birth_attendant, na.rm=T),
      facilities_measles = 
        sum(df$child_health_measles_immun_calc, na.rm=T)
  )}) 

lga_health <- merge(lga_health_data, lga_health_data_core, by="lga_id", all=TRUE)
lga_health <- merge(lga_health, lga_health_data_core_sansHP, by="lga_id", all=TRUE)
lga_health <- merge(lga_health, lga_health_data_core_hospital, by="lga_id", all=TRUE)

###### SUMMING UP #########
##writing out##
saveRDS(lga_health, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/Health_LGA_level_774.rds")



