#####################################################################################################################
##Normalized Health Outlier Cleaning ################################################################################
#####################################################################################################################
source('base_scripts/InstallFormhub.R')
source('source_scripts/NMIS_Functions.R')
source('cleaning_outliers/outlier_functions.R')

#Reading in Data
health_999 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Health_774_999Cleaned.rds")

#Adding necessary indicators:
health_999$improved_sanitation_and_functional[health_999$src == '113'] <- 
                                             (health_999$toilet_types_vip_latrine[health_999$src == '113'] | 
                                                 health_999$toilet_types_pit_w_slab[health_999$src == '113'] | 
                                               (health_999$toilet_types_flush_or_pour_flush[health_999$src == '113'] & 
                                                  health_999$flush_toilet_drain_to[health_999$src == '113'] == 'improved')) & 
                                            ((health_999$toilet_types_vip_latrine[health_999$src == '113'] & 
                                                (as.numeric(health_999$vip_latrine_not_working[health_999$src == '113']) < 7)) | 
                                               (health_999$toilet_types_pit_w_slab[health_999$src == '113'] & 
                                                  (as.numeric(health_999$slab_pit_latrine_not_working[health_999$src == '113']) < 7)) | 
                                               (health_999$toilet_types_flush_or_pour_flush[health_999$src == '113'] & 
                                                  health_999$flush_toilet_drain_to[health_999$src == '113'] == 'improved' & 
                                                  (as.numeric(health_999$flush_toilet_not_working[health_999$src == '113']) < 7)))

health_999$power_access_and_functional <- (((health_999$power_sources_generator[health_999$src == '113'] &
                                          health_999$generator_funct_yn[health_999$src == '113']) |
                                         (health_999$power_sources_solar[health_999$src == '113'] &
                                            health_999$solar_funct_yn[health_999$src == '113']) |
                                         (health_999$power_sources_grid[health_999$src == '113'] &
                                            health_999$grid_funct_yn[health_999$src == '113'])) &
                                        (as.numeric(health_999$days_no_electricity[health_999$src == '113']) <= 7))

health_999$power_access_and_functional[health_999$src == 'pilot'] <- (((health_999$power_sources_generator[health_999$src == 'pilot'] &
                                            health_999$generator_funct_yn[health_999$src == 'pilot']) |
                                           (health_999$power_sources_solar[health_999$src == 'pilot'] &
                                              health_999$solar_funct_yn[health_999$src == 'pilot']) |
                                           (health_999$power_sources_grid[health_999$src == 'pilot'] &
                                              health_999$grid_funct_yn[health_999$src == 'pilot'])) &
                                          (as.numeric(health_999$days_no_electricity[health_999$src == 'pilot']) <= 7))

health_999$improved_sanitation[health_999$src == 'pilot'] <- health_999$num_toilets_improved_p[health_999$src == 'pilot'] > 0 

######################################
#### Outlier Cleaning stars here: ####
######################################

health_999 <-outlierreplace(health_999, 'num_doctors_active',
      (health_999$num_doctors_active > 12 & 
        (health_999$facility_type != "teachinghospital" & 
        health_999$facility_type != "federalmedicalcentre")))
    
health_999 <- outlierreplace(health_999, 'num_doctors_active',
      (health_999$num_doctors_active > 20 & 
        (health_999$facility_type == "teachinghospital" | 
        health_999$facility_type == "federalmedicalcentre")))
    
health_999 <- outlierreplace(health_999, 'num_nurses_active',
      (health_999$num_nurses_active > 16 & 
        (health_999$facility_type != "teachinghospital" & 
        health_999$facility_type != "federalmedicalcentre")))
    
    health_999 <- outlierreplace(health_999, 'num_nurses_active',
          (health_999$num_nurses_active > 24 & 
            (health_999$facility_type == "teachinghospital" | 
            health_999$facility_type == "federalmedicalcentre")))
    
    health_999 <- outlierreplace(health_999, 'num_midwives_active',
          (health_999$num_midwives_active > 24 & 
            (health_999$facility_type == "teachinghospital" | 
            health_999$facility_type == "federalmedicalcentre")))
    
    health_999 <- outlierreplace(health_999, 'num_nursemidwives_active',
          (health_999$num_nursemidwives_active > 16 & 
            (health_999$facility_type != "teachinghospital" & 
            health_999$facility_type != "federalmedicalcentre")))
    
    health_999 <- outlierreplace(health_999, 'num_nursemidwives_active',
          (health_999$num_nursemidwives_active > 24 & 
            (health_999$facility_type == "teachinghospital" | 
            health_999$facility_type == "federalmedicalcentre")))

    health_999 <- outlierreplace(health_999, 'num_nursemidwives_active',
          (health_999$num_nursemidwives_active > 50))
    
    health_999 <- outlierreplace(health_999, 'num_junior_chews_active',
          (health_999$num_junior_chews_active > 50))

#new data points

health_999 <- outlierreplace(health_999, 'facility_type',
                    (((health_999$num_doctors_posted < 30 & 
                      health_999$num_doctors_posted != 0) & 
                      (health_999$num_midwives_posted < 30 & 
                      health_999$num_midwives_posted != 0) &
                      (health_999$num_nurses_posted < 30 & 
                      health_999$num_nurses_posted != 0)) &
                      (health_999$facility_type == "teachinghospital" | 
                      health_999$facility_type == "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'num_doctors_posted',
      ((health_999$num_doctors_posted > 500 | 
        health_999$num_doctors_posted < 100) & 
        (health_999$facility_type == "teachinghospital"  |
        health_999$facility_type == "federalmedicalcentre")
        ))

health_999 <- outlierreplace(health_999, 'num_nurses_posted',
                    (health_999$num_nurses_posted < 100 &
                      (health_999$facility_type == "teachinghospital" | 
                      health_999$facility_type == "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'num_midwives_posted',
                    (health_999$num_midwives_posted < 100 & 
                      (health_999$facility_type == "teachinghospital" | 
                      health_999$facility_type == "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'num_nursemidwives_posted',
                    (health_999$num_nursemidwives_posted < 50 &
                      (health_999$facility_type == "teachinghospital" | 
                      health_999$facility_type == "federalmedicalcentre")))
        
health_999 <- outlierreplace(health_999, 'num_midwives_posted',
      (health_999$num_midwives_posted > 16 & 
        (health_999$facility_type != "teachinghospital" & 
        health_999$facility_type != "federalmedicalcentre")))
               
health_999 <- outlierreplace(health_999, 'num_nurses_posted',
      (health_999$num_nurses_posted > 16 & 
        (health_999$facility_type != "teachinghospital" & 
        health_999$facility_type != "federalmedicalcentre")))            

health_999 <- outlierreplace(health_999, 'num_nursemidwives_posted',
                    (health_999$num_nursemidwives_posted > 16 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'num_cho_posted',
                    (health_999$num_cho_posted > 16 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))


health_999 <- outlierreplace(health_999, 'num_cho_posted',
                    (health_999$num_cho_posted > 24 &
                      (health_999$facility_type == "teachinghospital" | 
                      health_999$facility_type == "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'num_chews_posted',
                    (health_999$num_chews_posted > 50 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'num_chews_posted',
                    (health_999$num_chews_posted > 50 &
                      (health_999$facility_type == "teachinghospital" | 
                      health_999$facility_type == "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'num_junior_chews_posted',
                    (health_999$num_junior_chews_posted > 50 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))   

health_999 <- outlierreplace(health_999, 'num_junior_chews_posted',
                    (health_999$num_junior_chews_posted > 24 &
                      (health_999$facility_type == "teachinghospital" | 
                      health_999$facility_type == "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'pharmacists_posted',
                    (health_999$pharmacists_posted > 5 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'pharmacists_posted',
                    (health_999$pharmacists_posted < 5 &
                      (health_999$facility_type == "teachinghospital" | 
                      health_999$facility_type == "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'environmental_health_officers_posted',
                    (health_999$environmental_health_officers_posted > 6 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))      

health_999 <- outlierreplace(health_999, 'lab_technicians_posted',
                    (health_999$lab_technicians_posted > 4 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))
      
health_999 <- outlierreplace(health_999, 'pharma_technicians_posted',
                    (health_999$pharma_technicians_posted > 5 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))
       
health_999 <- outlierreplace(health_999, 'medical_records_officers_posted',
                    (health_999$medical_records_officers_posted > 4 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))

# 
health_999 <- outlierreplace(health_999, 'inpatient_care_num_beds',
                    (health_999$inpatient_care_num_beds > 50 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'num_flush_or_pour_flush_piped',
                    (health_999$num_flush_or_pour_flush_piped > 20 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'num_flush_other',
                    (health_999$num_flush_other > 10 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'num_vip_latrine',
                    (health_999$num_vip_latrine > 6 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'num_pit_w_slab',
                    (health_999$num_pit_w_slab > 8 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'num_open_pit_latrine',
                    (health_999$num_open_pit_latrine > 4 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'num_bucket_system',
                    (health_999$num_bucket_system > 10 & 
                      (health_999$facility_type != "teachinghospital" & 
                      health_999$facility_type != "federalmedicalcentre")))

health_999 <- outlierreplace(health_999, 'num_bucket_system',
                    (health_999$num_bucket_system > 10 & 
                      (health_999$facility_type == "teachinghospital" | 
                      health_999$facility_type == "federalmedicalcentre")))

##writing out.rds
saveRDS(health_999, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Health_774_outliercleaned.rds")
rm(health_999)



