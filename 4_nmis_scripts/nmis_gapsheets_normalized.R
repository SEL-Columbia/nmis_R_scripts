####################################################################################################
#Gapsheets##########################################################################################
####################################################################################################
require(plyr)
source("./source_scripts/NMIS_Functions.R")

  lgas <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv")
  lgas <- lgas[,c('lga_id', 'lga', 'state', 'zone', 'unique_lga', 'pop_2006')]

#health##############################################################################################
  hh <- 
  readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/normalized/Health_774_ALL_FACILITY_INDICATORS.rds")
  h <- hh

#booleans in preperation for aggregation
  h$anypower <- h$power_sources_generator |  
                  h$power_sources_solar |
                    h$power_sources_grid |
                      h$power_sources_none == F
  h$sba <- (rowSums(cbind(h$num_doctors_posted, 
                   h$num_midwives_posted,
                   h$num_nursemidwives_posted), na.rm=T) >= 2)
  h$phcentre <- (h$num_cho_posted >= 1 & 
                  h$num_chews_posted >= 3 &
                (rowSums(cbind(h$num_midwives_posted,  
                  h$num_nursemidwives_posted), na.rm=T) >= 4))
  h$phclinic <- (h$num_chews_posted >= 2 &
                  h$num_junior_chews_posted >=2 &
                (rowSums(cbind(h$num_midwives_posted, 
                  h$num_nursemidwives_posted), na.rm=T) >= 2))
  h$hpdispensary <- (h$num_junior_chews_posted >= 1 |
                      h$num_chews_posted >= 1)
  
  h$all_facilities <- (h$facility_type %in% 
        c('primaryhealthcarecentre', 'wardmodelphccentre',
          'primaryhealthclinic', 'healthpostdispensary'))

#changing into idata.frame
  ih <- idata.frame(h)

#aggregating
  h_gap <- ddply(ih, .(lga_id), function(df) {
          data.frame(        
    #OVERVIEW SECTION
      # Total Number of Existing Primary Health Care Facilities	
        gap_sheet_total_facilities = sum(df$all_facilities, na.rm=T),                      
      # Number of Hospitals
        gap_sheet_total_hospitals = sum(df$facility_type %in% c('cottagehospital', 'generalhospital', 
                                                    'specialisthospital', 'teachinghospital'), na.rm=T),
      # Number of Primary Health Centres (PHCs)
        gap_sheet_total_phcentres = sum(df$facility_type %in%
                                   c('primaryhealthcarecentre', 'wardmodelphccentre'), na.rm=T),
      # Number of Primary Health Clinics
        gap_sheet_total_phclinics = sum(df$facility_type == 'primaryhealthclinic', na.rm=T),
      # Number of Health Posts and Dispensaries
        gap_sheet_total_dispensary = sum(df$facility_type == 'healthpostdispensary', na.rm=T),
      # Total Number of Secondary and Tertiary Facilities
        gap_sheet_total_sec_tertiary = sum(df$facility_type %in% c('maternity', 'dentalclinic', 
                                                            'comprehensivehealthcentre',
                                                            'federalmedicalcentre'), na.rm=T),                           
    #Rest of gap sheet
      # Improved and Functional Water Point
        gap_sheet_i_water_supply_numerator = sum(df$improved_water_supply, na.rm=T), 
        gap_sheet_i_water_supply_denominator = length(na.omit(df$improved_water_supply)),
        gap_sheet_i_water_supply_percent = round(100*sum(df$improved_water_supply, na.rm=T)/
                                                length(na.omit(df$improved_water_supply))),
        
      # Improved Toilet 
        gap_sheet_i_sanitation_numerator = sum(df$improved_sanitation, na.rm=T),  
        gap_sheet_i_sanitation_denominator = length(na.omit(df$improved_sanitation)),
        gap_sheet_i_sanitation_percent = round(100*sum(df$improved_sanitation, na.rm=T)/
                                              length(na.omit(df$improved_sanitation))),
        
      # Grid Power available (PHCN/NEPA)
        gap_sheet_phcn_electricity_h_numerator = sum(df$phcn_electricity, na.rm=T),
        gap_sheet_phcn_electricity_h_denominator = length(na.omit(df$phcn_electricity)),
        gap_sheet_phcn_electricity_h_percent = round(100*sum(df$phcn_electricity, na.rm=T)/
                                                       length(na.omit(df$phcn_electricity))),
        
      # Any Power Available (grid or alternative power supply) 
        gap_sheet_any_power_available_numerator = sum(df$anypower, na.rm=T),
        gap_sheet_any_power_available_denominator = length(na.omit(df$anypower)),
        gap_sheet_any_power_available_percent = round(100*sum(df$anypower, na.rm=T)/
                                                        length(na.omit(df$anypower))),
        
      # Emergency Transport available for referrals
        gap_sheet_emerg_tran_numerator = sum(df$emergency_transport, na.rm=T),
        gap_sheet_emerg_tran_denominator = length(na.omit(df$emergency_transport)),
        gap_sheet_emerg_tran_percent = round(100*sum(df$emergency_transport, na.rm=T)/
                                               length(na.omit(df$emergency_transport))),
        
      # Sufficient Skilled Birth Attendants
        gap_sheet_sba_numerator = sum(df$sba, na.rm=T),
        gap_sheet_sba_denominator = length(na.omit(df$sba)),
        gap_sheet_sba_percent = round(100*sum(df$sba, na.rm=T)/
                                        length(na.omit(df$sba))),
        
      # Delivery Services available
        gap_sheet_delivery_services_yn_numerator = sum(df$delivery_services_yn, na.rm=T),
        gap_sheet_delivery_services_yn_denominator = length(na.omit(df$delivery_services_yn)),
        gap_sheet_delivery_services_yn_percent = round(100*sum(df$delivery_services_yn, na.rm=T)/
                                               length(na.omit(df$delivery_services_yn))),
        
      # C-Sections performed
        gap_sheet_c_section_yn_numerator = sum(df$c_section_yn, na.rm=T),  
        gap_sheet_c_section_yn_denominator = length(na.omit(df$c_section_yn)),
        gap_sheet_c_section_yn_percent = round(100*sum(df$c_section_yn, na.rm=T)/
                                                 length(na.omit(df$c_section_yn))),
        
      # Antenatal Care Services are provided
        gap_sheet_antenatal_care_yn_numerator = sum(df$antenatal_care_yn, na.rm=T),
        gap_sheet_antenatal_care_yn_denominator = length(na.omit(df$antenatal_care_yn)),
        gap_sheet_antenatal_care_yn_percent = round(100*sum(df$antenatal_care_yn, na.rm=T)/
                                            length(na.omit(df$antenatal_care_yn))),
        
      # Family Planning Methods provided
        gap_sheet_family_planning_yn_numerator = sum(df$family_planning_yn, na.rm=T),
        gap_sheet_family_planning_yn_denominator = length(na.omit(df$family_planning_yn)),
        gap_sheet_family_planning_yn_percent = round(100*sum(df$family_planning_yn, na.rm=T)/
                                             length(na.omit(df$family_planning_yn))),
        
      # Measles Immunizations are provided 
        gap_sheet_child_health_measles_immun_numerator = sum(df$child_health_measles_immun, na.rm=T),    
        gap_sheet_child_health_measles_immun_denominator = length(na.omit(df$child_health_measles_immun)),
        gap_sheet_child_health_measles_immun_percent = round(100*sum(df$child_health_measles_immun, na.rm=T)/
                                                       length(na.omit(df$child_health_measles_immun))),
        
      # Refrigerator or Freezer for vaccine storage
        gap_sheet_vaccines_fridge_freezer_numerator = sum(df$vaccines_fridge_freezer, na.rm=T),
        gap_sheet_vaccines_fridge_freezer_denominator = length(na.omit(df$vaccines_fridge_freezer)),
        gap_sheet_vaccines_fridge_freezer_percent = round(100*sum(df$vaccines_fridge_freezer, na.rm=T)/
                                                  length(na.omit(df$vaccines_fridge_freezer))),
        
      # Artemisinin-based combination therapy (ACT)
        gap_sheet_medication_anti_malarials_numerator = sum(df$medication_anti_malarials, na.rm=T),
        gap_sheet_medication_anti_malarials_denominator = length(na.omit(df$medication_anti_malarials)),
        gap_sheet_medication_anti_malarials_percent = round(100*sum(df$medication_anti_malarials, na.rm=T)/
                                                    length(na.omit(df$medication_anti_malarials))),
        
      # Fully staffed primary health centres (PHCs)
        gap_sheet_phcentre_numerator = sum(df$phcentre, na.rm=T),
        gap_sheet_phcentre_denominator = length(na.omit(df$phcentre)),
        gap_sheet_phcentre_percent = round(100*sum(df$phcentre, na.rm=T)/
                                             length(na.omit(df$phcentre))),
        
      # Fully staffed primary health clinics
        gap_sheet_phclinic_numerator = sum(df$phclinic, na.rm=T),
        gap_sheet_phclinic_denominator = length(na.omit(df$phclinic)),
        gap_sheet_phclinic_percent = round(100*sum(df$phclinic, na.rm=T)/
                                             length(na.omit(df$phclinic))),
        
      # Fully staffed dispensaries
        gap_sheet_dispensary_numerator = sum(df$hpdispensary, na.rm=T),
        gap_sheet_dispensary_denominator = length(na.omit(df$hpdispensary)),
        gap_sheet_dispensary_percent = round(100*sum(df$hpdispensary, na.rm=T)/
                                               length(na.omit(df$hpdispensary)))
  )})


#education#################################################################################################
  e <- 
  readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/normalized/Education_774_ALL_FACILITY_INDICATORS.rds")
  edu <- e

#booleans in preperation for aggregation
  edu$improved_functional_water <- edu$functional_water & edu$improved_water_supply

#changing into idata.frame
 iedu <- idata.frame(edu)

  e_gap <- ddply(iedu, .(lga_id), function(df) {
      data.frame(     
    # Total number of existing primary and junior secondary schools
      gap_sheet_primary_js = sum(df$pj, na.rm=T),  
    # Total number of existing classrooms in Primary & Junior Secondary Schools	
      gap_sheet_num_existing_classrooms = sum(df$num_classrms_total, na.rm=T),
      
    # Total Number of Teachers	
      gap_sheet_total_teachers = sum(df$num_tchr_full_time, na.rm=T),
    # Total Number of Students enrolled in primary education	??
    # Total Number of Students enrolled in junior secondary education	??  
    
        
    # Schools with access to improved functional water 
      gap_sheet_improved_functional_water_numerator = sum(df$improved_functional_water, na.rm=T),
      gap_sheet_improved_functional_water_denominator = length(na.omit(df$improved_functional_water)),
      gap_sheet_improved_functional_water_percent = round(100*sum(df$improved_functional_water, na.rm=T)/
                                                            length(na.omit(df$improved_functional_water))),
      
    # Schools with access to improved sanitation
      gap_sheet_improved_sanitation_numerator = sum(df$improved_sanitation, na.rm=T),
      gap_sheet_improved_sanitation_denominator = length(na.omit(df$improved_sanitation)),
      gap_sheet_improved_sanitation_percent = round(100*sum(df$improved_sanitation, na.rm=T)/
                                                        length(na.omit(df$improved_sanitation))),
      
    # Schools connected to the national electricity grid (PHCN, NEPA)
      gap_sheet_phcn_electricity_e_numerator = sum(df$phcn_electricity, na.rm=T),
      gap_sheet_phcn_electricity_e_denominator = length(na.omit(df$phcn_electricity)),
      gap_sheet_phcn_electricity_e_percent = round(100*sum(df$phcn_electricity, na.rm=T)/
                                                  length(na.omit(df$phcn_electricity))),
    
    # Total number of classrooms with a useable chalkboard/blackboard/whiteboard
      gap_sheet_num_classrms_repairs_numerator = sum(df$num_classrms_need_maj_repairs, na.rm=T),
      gap_sheet_num_classrms_repairs_denominator = sum(df$num_classrms_need_maj_repairs, na.rm=T),
      gap_sheet_num_classrms_repairs_percent = round(100*sum(df$num_classrms_need_maj_repairs, na.rm=T)/
                                                                 sum(df$num_classrms_total, na.rm=T)),
    
   # Total number of classrooms with a useable chalkboard/blackboard/whiteboard
      gap_sheet_num_classrm_w_chalkboard_numerator = sum(df$num_classrm_w_chalkboard, na.rm=T),
      gap_sheet_num_classrm_w_chalkboard_denominator = sum(df$num_classrms_total, na.rm=T),
      gap_sheet_num_classrm_w_chalkboard_percent = round(100*sum(df$num_classrm_w_chalkboard, na.rm=T)/
                                                             sum(df$num_classrms_total, na.rm=T)),
      
    # Total number of NCE qualified teachers 
      gap_sheet_num_tchrs_with_nce_numerator = sum(df$num_tchrs_with_nce, na.rm=T),
      gap_sheet_num_tchrs_with_nce_denominator = sum(df$num_tchr_full_time, na.rm=T), 
      gap_sheet_num_tchrs_with_nce_percent = round(100*sum(df$num_tchrs_with_nce, na.rm=T)/
                                                     sum(df$num_tchr_full_time, na.rm=T))
      
  )})

#writing out############################################################################################
combined <- merge(h_gap, e_gap, by="lga_id", all=T)

#writing out data
saveRDS(combined, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/output_data/data_774/final_output/gap_sheet.RDS")



# 
#   write.csv(h_gap_final, "~/Code/nmis_ui_data_2ef92c15/data_774/health_gapsheet.csv", row.names=F)
#   write.csv(e_gap_final, "~/Code/nmis_ui_data_2ef92c15/data_774/education_gapsheet.csv", row.names=F)
#   write.csv(e_gap_final, "~/Code/nmis_ui_data_2ef92c15/data_774/All_774_LGA.csv", row.names=F)

    
