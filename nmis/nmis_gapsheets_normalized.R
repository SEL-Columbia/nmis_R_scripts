####################################################################################################
####################################################################################################
# Gapsheets

lgas <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv")

#health####################################################################################################
hh <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Health_774_ALL_FACILITY_INDICATORS.rds")
#new data set
h <- hh

#booleans in preperation for aggregation
h$anypower <- h$power_sources_generator |  
                h$power_sources_solar |
                  h$power_sources_grid |
                    h$power_sources_none == F
h$emerg_tran <- h$transport_to_referral %in% c('ambulance', 'taxi', 'keke')
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

h$denominator <- (h$facility_type %in% 
      c('cottagehospital', 'generalhospital', 
        'specialisthospital', 'teachinghospital',
        'primaryhealthcarecentre', 'wardmodelphccentre',
        'primaryhealthclinic', 'healthpostdispensary',
        'maternity', 'dentalclinic', 
        'comprehensivehealthcentre','federalmedicalcentre'))

#changing into idata.frame
ih <- idata.frame(h)

#aggregating
h_gap <- ddply(ih, .(lga_id), function(df) {
          data.frame(        
  #OVERVIEW SECTION
    # Total Number of Existing Primary Health Care Facilities	
      total_facilities = sum(df$denominator, na.rm=T),                      
    # Number of Hospitals
      total_hospitals = sum(df$facility_type %in% c('cottagehospital', 'generalhospital', 
                                                  'specialisthospital', 'teachinghospital'), na.rm=T),
    # Number of Primary Health Centres (PHCs)
      total_phcentres = sum(df$facility_type %in%
                                 c('primaryhealthcarecentre', 'wardmodelphccentre'), na.rm=T),
    # Number of Primary Health Clinics
      total_phclinics = sum(df$facility_type == 'primaryhealthclinic', na.rm=T),
    # Number of Dispensaries
      total_phclinics = sum(df$facility_type == 'healthpostdispensary', na.rm=T),
    # Total Number of Secondary and Tertiary Facilities (Health Posts + Mobile Clinics)  
      total_sec_tertiary = sum(df$facility_type %in% c('maternity', 'dentalclinic', 
                                                          'comprehensivehealthcentre',
                                                          'federalmedicalcentre'), na.rm=T),                           

  #Rest of gap sheet
    # Improved and Functional Water Point
      improved_water_supply_numerator = sum(df$improved_water_supply, na.rm=T), 
      improved_water_supply_denominator = sum(df$denominator, na.rm=T),
      improved_water_supply_percent = ceiling(100*sum(df$improved_water_supply, na.rm=T)/
                                                sum(df$denominator, na.rm=T)),
      
    # Improved Toilet 
      improved_sanitation_numerator = sum(df$improved_sanitation, na.rm=T),  
      improved_sanitation_denominator = sum(df$denominator, na.rm=T),
      improved_sanitation_percent = ceiling(100*sum(df$improved_sanitation, na.rm=T)/
                                              sum(df$denominator, na.rm=T)),
      
    # Grid Power available (PHCN/NEPA)
      phcn_electricity_numerator = sum(df$phcn_electricity, na.rm=T),
      phcn_electricity_denominator = sum(df$denominator, na.rm=T),
      phcn_electricity_percent = ceiling(100*sum(df$phcn_electricity, na.rm=T)/sum(df$denominator, na.rm=T)),
      
    # Any Power Available (grid or alternative power supply) 
      any_power_available_numerator = sum(df$anypower, na.rm=T),
      any_power_available_denominator = sum(df$denominator, na.rm=T),
      any_power_available_percent = ceiling(100*sum(df$anypower, na.rm=T)/sum(df$denominator, na.rm=T)),
      
    # Emergency Transport available for referrals
      emerg_tran_numerator = sum(df$emerg_tran, na.rm=T),
      emerg_tran_denominator = sum(df$denominator, na.rm=T),
      emerg_tran_percent = ceiling(100*sum(df$emerg_tran, na.rm=T)/sum(df$denominator, na.rm=T)),
      
    # Sufficient Skilled Birth Attendants
      sba_numerator = sum(df$sba, na.rm=T),
      sba_denominator = sum(df$denominator, na.rm=T),
      sba_percent = ceiling(100*sum(df$sba, na.rm=T)/sum(df$denominator, na.rm=T)),
      
    # Delivery Services available
      delivery_services_yn_numerator = sum(df$delivery_services_yn, na.rm=T),
      delivery_services_yn_denominator = sum(df$denominator, na.rm=T),
      delivery_services_yn_percent = ceiling(100*sum(df$delivery_services_yn, na.rm=T)/
                                               sum(df$denominator, na.rm=T)),
      
    # C-Sections performed
      c_section_yn_numerator = sum(df$c_section_yn, na.rm=T),  
      c_section_yn_denominator = sum(df$denominator, na.rm=T),
      c_section_yn_percent = ceiling(100*sum(df$c_section_yn, na.rm=T)/sum(df$denominator, na.rm=T)),
      
    # Antenatal Care Services are provided
      antenatal_care_yn_numerator = sum(df$antenatal_care_yn, na.rm=T),
      antenatal_care_yn_denominator = sum(df$denominator, na.rm=T),
      antenatal_care_yn_percent = ceiling(100*sum(df$antenatal_care_yn, na.rm=T)/
                                            sum(df$denominator, na.rm=T)),
      
    # Family Planning Methods provided
      family_planning_yn_numerator = sum(df$family_planning_yn, na.rm=T),
      family_planning_yn_denominator = sum(df$denominator, na.rm=T),
      family_planning_yn_percent = ceiling(100*sum(df$family_planning_yn, na.rm=T)/
                                             sum(df$denominator, na.rm=T)),
      
    # Measles Immunizations are provided 
      child_health_measles_immun_numerator = sum(df$child_health_measles_immun, na.rm=T),    
      child_health_measles_immun_denominator = sum(df$denominator, na.rm=T),
      child_health_measles_immun_percent = ceiling(100*sum(df$child_health_measles_immun, na.rm=T)/
                                                     sum(df$denominator, na.rm=T)),
      
    # Refrigerator or Freezer for vaccine storage
      vaccines_fridge_freezer_numerator = sum(df$vaccines_fridge_freezer, na.rm=T),
      vaccines_fridge_freezer_denominator = sum(df$denominator, na.rm=T),
      vaccines_fridge_freezer_percent = ceiling(100*sum(df$vaccines_fridge_freezer, na.rm=T)/
                                                  sum(df$denominator, na.rm=T)),
      
    # Artemisinin-based combination therapy (ACT)
      medication_anti_malarials_numerator = sum(df$medication_anti_malarials, na.rm=T),
      medication_anti_malarials_denominator = sum(df$denominator, na.rm=T),
      medication_anti_malarials_percent = ceiling(100*sum(df$medication_anti_malarials, na.rm=T)/
                                                    sum(df$denominator, na.rm=T)),
      
    # Fully staffed hospitals ?? need to follow up with Ranu
    # Fully staffed primary health centres (PHCs)
      phcentre_numerator = sum(df$phcentre, na.rm=T),
      phcentre_denominator = sum(df$denominator, na.rm=T),
      phcentre_percent = ceiling(100*sum(df$phcentre, na.rm=T)/sum(df$denominator, na.rm=T)),
      
    # Fully staffed primary health clinics
      phclinic_numerator = sum(df$phclinic, na.rm=T),
      phclinic_denominator = sum(df$denominator, na.rm=T),
      phclinic_percent = ceiling(100*sum(df$phclinic, na.rm=T)/sum(df$denominator, na.rm=T)),
      
    # Fully staffed dispensaries
      dispensary_numerator = sum(df$hpdispensary, na.rm=T),
      dispensary_denominator = sum(df$denominator, na.rm=T),
      dispensary_percent = ceiling(100*sum(df$hpdispensary, na.rm=T)/sum(df$denominator, na.rm=T))
)})

#adding basic information
h_gap_final <- merge(h_gap, lgas[,c('lga_id', 'lga', 'state', 'zone', 'pop_2006')], by="lga_id")

#writing out data
write.csv(h_gap_final, "~/Code/nmis_ui_data_2ef92c15/data_774/health_gapsheet.csv", row.names=F)


#education#################################################################################################
e <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Education_774_ALL_FACILITY_INDICATORS.rds")
#new data set
edu <- e

#booleans in preperation for aggregation
edu$improved_functonal_water <- edu$functional_water & edu$improved_water_supply

#changing into idata.frame
iedu <- idata.frame(edu)

e_gap <- ddply(iedu, .(lga_id), function(df) {
    data.frame(     
  # Total Number of Existing Schools	
    num_existing_schools = length(df$uuid),
  # Total Number of Existing Classrooms	
    num_existing_classrooms = sum(df$num_classrms_total, na.rm=T),
    
  # Total Number of Teachers	
    total_teachers = sum(df$num_tchrs_total, na.rm=T),
  # Total Number of Students enrolled in primary education	??
  # Total Number of Students enrolled in junior secondary education	??
    
  # Total number of existing primary and junior secondary schools
    primary_js = sum(df$pj, na.rm=T),
  # Total number of existing classrooms => same as above "num_existing_classrooms"
      
  # Schools with access to improved functional water 
    improved_functional_water_numerator = sum(df$improved_functonal_water, na.rm=T),
    improved_functional_water_denominator = length(df$uuid),
    improved_functional_water_percent = ceiling(100*sum(df$improved_functonal_water, na.rm=T)/
                                                  length(df$uuid)),
    
  # Schools with access to improved sanitation
    improved_sanitation_numerator = sum(df$improved_sanitation, na.rm=T),
    improved_sanitation_denominator = length(df$uuid),
    improved_sanitation_percent = ceiling(100*sum(df$improved_sanitation, na.rm=T)/length(df$uuid)),
    
  # Schools connected to the national electricity grid (PHCN, NEPA)
    phcn_electricity_numerator = sum(df$phcn_electricity, na.rm=T),
    phcn_electricity_denominator = length(df$uuid),
    phcn_electricity_percent = ceiling(100*sum(df$phcn_electricity, na.rm=T)/length(df$uuid)),
    
  # Total number of classrooms with a useable chalkboard/blackboard/whiteboard
    num_classrm_w_chalkboard_numerator = sum(df$num_classrm_w_chalkboard, na.rm=T),
    num_classrm_w_chalkboard_denominator = length(df$uuid),
    num_classrm_w_chalkboard_percent = ceiling(100*sum(df$num_classrm_w_chalkboard, na.rm=T)/
                                                 length(df$uuid)),
    
  # Total number of NCE qualified teachers 
    num_tchrs_with_nce_numerator = sum(df$num_tchrs_with_nce, na.rm=T),
    num_tchrs_with_nce_denominator = length(df$uuid),
    num_tchrs_with_nce_percent = ceiling(100*sum(df$num_tchrs_with_nce, na.rm=T)/
                                                 length(df$uuid))
    
)})

#adding basic information
e_gap_final <- merge(e_gap, lgas[,c('lga_id', 'lga', 'state', 'zone', 'pop_2006')], by="lga_id")

#writing out data
write.csv(e_gap_final, "~/Code/nmis_ui_data_2ef92c15/data_774/education_gapsheet.csv", row.names=F)
    
    
    
  