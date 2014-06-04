#####################################################################################################################
##Normalized Health Outlier Cleaning

source('base_scripts/InstallFormhub.R')
source('source_scripts/NMIS_Functions.R')
source('source_scripts/outlier_functions.R')

#Reading in Data
health_normalized <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/normalized/Health_774_normalized.rds")

######################################
#### Outlier Cleaning 

health_normalized <-outlierreplace(health_normalized, 'num_doctors_active',
      (health_normalized$num_doctors_active > 12 & 
        (health_normalized$facility_type != "teachinghospital" & 
        health_normalized$facility_type != "federalmedicalcentre")))
    
health_normalized <- outlierreplace(health_normalized, 'num_doctors_active',
      (health_normalized$num_doctors_active > 20 & 
        (health_normalized$facility_type == "teachinghospital" | 
        health_normalized$facility_type == "federalmedicalcentre")))
    
health_normalized <- outlierreplace(health_normalized, 'num_nurses_active',
      (health_normalized$num_nurses_active > 16 & 
        (health_normalized$facility_type != "teachinghospital" & 
        health_normalized$facility_type != "federalmedicalcentre")))
    
health_normalized <- outlierreplace(health_normalized, 'num_nurses_active',
      (health_normalized$num_nurses_active > 24 & 
        (health_normalized$facility_type == "teachinghospital" | 
        health_normalized$facility_type == "federalmedicalcentre")))
    
health_normalized <- outlierreplace(health_normalized, 'num_midwives_active',
      (health_normalized$num_midwives_active > 24 & 
        (health_normalized$facility_type == "teachinghospital" | 
        health_normalized$facility_type == "federalmedicalcentre")))
    
health_normalized <- outlierreplace(health_normalized, 'num_nursemidwives_active',
      (health_normalized$num_nursemidwives_active > 16 & 
        (health_normalized$facility_type != "teachinghospital" & 
        health_normalized$facility_type != "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_nursemidwives_active',
      (health_normalized$num_nursemidwives_active > 24 & 
        (health_normalized$facility_type == "teachinghospital" | 
        health_normalized$facility_type == "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_junior_chews_active',
      (health_normalized$num_junior_chews_active > 50))

#new data points
health_normalized <- outlierreplace(health_normalized, 'facility_type',
                    (((health_normalized$num_doctors_posted < 30 & 
                      health_normalized$num_doctors_posted != 0) & 
                      (health_normalized$num_midwives_posted < 30 & 
                      health_normalized$num_midwives_posted != 0) &
                      (health_normalized$num_nurses_posted < 30 & 
                      health_normalized$num_nurses_posted != 0)) &
                      (health_normalized$facility_type == "teachinghospital" | 
                      health_normalized$facility_type == "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_doctors_posted',
                  ((health_normalized$num_doctors_posted > 500 | 
                    health_normalized$num_doctors_posted < 100) & 
                    (health_normalized$facility_type == "teachinghospital"  |
                    health_normalized$facility_type == "federalmedicalcentre")
                    ))

health_normalized <- outlierreplace(health_normalized, 'num_nurses_posted',
                    (health_normalized$num_nurses_posted < 100 &
                      (health_normalized$facility_type == "teachinghospital" | 
                      health_normalized$facility_type == "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_midwives_posted',
                    (health_normalized$num_midwives_posted < 100 & 
                      (health_normalized$facility_type == "teachinghospital" | 
                      health_normalized$facility_type == "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_nursemidwives_posted',
                    (health_normalized$num_nursemidwives_posted < 50 &
                      (health_normalized$facility_type == "teachinghospital" | 
                      health_normalized$facility_type == "federalmedicalcentre")))
        
health_normalized <- outlierreplace(health_normalized, 'num_midwives_posted',
                (health_normalized$num_midwives_posted > 16 & 
                  (health_normalized$facility_type != "teachinghospital" & 
                  health_normalized$facility_type != "federalmedicalcentre")))
                         
health_normalized <- outlierreplace(health_normalized, 'num_nurses_posted',
                  (health_normalized$num_nurses_posted > 16 & 
                    (health_normalized$facility_type != "teachinghospital" & 
                    health_normalized$facility_type != "federalmedicalcentre")))            

health_normalized <- outlierreplace(health_normalized, 'num_nursemidwives_posted',
                    (health_normalized$num_nursemidwives_posted > 16 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_cho_posted',
                    (health_normalized$num_cho_posted > 16 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_cho_posted',
                    (health_normalized$num_cho_posted > 24 &
                      (health_normalized$facility_type == "teachinghospital" | 
                      health_normalized$facility_type == "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_chews_posted',
                    (health_normalized$num_chews_posted > 50 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_chews_posted',
                    (health_normalized$num_chews_posted > 50 &
                      (health_normalized$facility_type == "teachinghospital" | 
                      health_normalized$facility_type == "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_junior_chews_posted',
                    (health_normalized$num_junior_chews_posted > 50 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))   

health_normalized <- outlierreplace(health_normalized, 'num_junior_chews_posted',
                    (health_normalized$num_junior_chews_posted > 24 &
                      (health_normalized$facility_type == "teachinghospital" | 
                      health_normalized$facility_type == "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'pharmacists_posted',
                    (health_normalized$pharmacists_posted > 5 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'pharmacists_posted',
                    (health_normalized$pharmacists_posted < 5 &
                      (health_normalized$facility_type == "teachinghospital" | 
                      health_normalized$facility_type == "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'environmental_health_officers_posted',
                    (health_normalized$environmental_health_officers_posted > 6 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))      

health_normalized <- outlierreplace(health_normalized, 'lab_technicians_posted',
                    (health_normalized$lab_technicians_posted > 4 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))
      
health_normalized <- outlierreplace(health_normalized, 'pharma_technicians_posted',
                    (health_normalized$pharma_technicians_posted > 5 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))
       
health_normalized <- outlierreplace(health_normalized, 'medical_records_officers_posted',
                    (health_normalized$medical_records_officers_posted > 4 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))

# 
health_normalized <- outlierreplace(health_normalized, 'inpatient_care_num_beds',
                    (health_normalized$inpatient_care_num_beds > 50 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_flush_or_pour_flush_piped',
                    (health_normalized$num_flush_or_pour_flush_piped > 20 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_flush_other',
                    (health_normalized$num_flush_other > 10 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_vip_latrine',
                    (health_normalized$num_vip_latrine > 6 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_pit_w_slab',
                    (health_normalized$num_pit_w_slab > 8 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_open_pit_latrine',
                    (health_normalized$num_open_pit_latrine > 4 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))

health_normalized <- outlierreplace(health_normalized, 'num_bucket_system',
                    (health_normalized$num_bucket_system > 10 & 
                      (health_normalized$facility_type != "teachinghospital" & 
                      health_normalized$facility_type != "federalmedicalcentre")))
  

##writing out.rds
saveRDS(health_normalized, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Health_774_outliercleaned.rds")




