####################################################################################################
####################################################################################################
# Gapsheets
require(plyr)
source("./source_scripts/NMIS_Functions.R")

lgas <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv")
lgas <- lgas[,c('lga_id', 'lga', 'state', 'zone', 'unique_lga', 'pop_2006')]

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

h$all_facilities <- (h$facility_type %in% 
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
      total_facilities = sum(df$all_facilities, na.rm=T),                      
    # Number of Hospitals
      total_hospitals = sum(df$facility_type %in% c('cottagehospital', 'generalhospital', 
                                                  'specialisthospital', 'teachinghospital'), na.rm=T),
    # Number of Primary Health Centres (PHCs)
      total_phcentres = sum(df$facility_type %in%
                                 c('primaryhealthcarecentre', 'wardmodelphccentre'), na.rm=T),
    # Number of Primary Health Clinics
      total_phclinics = sum(df$facility_type == 'primaryhealthclinic', na.rm=T),
    # Number of Dispensaries
      total_dispensary = sum(df$facility_type == 'healthpostdispensary', na.rm=T),
    # Total Number of Secondary and Tertiary Facilities (Health Posts + Mobile Clinics)  
      total_sec_tertiary = sum(df$facility_type %in% c('maternity', 'dentalclinic', 
                                                          'comprehensivehealthcentre',
                                                          'federalmedicalcentre'), na.rm=T),                           

  #Rest of gap sheet
    # Improved and Functional Water Point
      improved_water_supply_numerator = sum(df$improved_water_supply, na.rm=T), 
      improved_water_supply_denominator = length(na.omit(df$improved_water_supply)),
      improved_water_supply_percent = round(100*sum(df$improved_water_supply, na.rm=T)/
                                              length(na.omit(df$improved_water_supply))),
      
    # Improved Toilet 
      improved_sanitation_numerator = sum(df$improved_sanitation, na.rm=T),  
      improved_sanitation_denominator = length(na.omit(df$improved_sanitation)),
      improved_sanitation_percent = round(100*sum(df$improved_sanitation, na.rm=T)/
                                            length(na.omit(df$improved_sanitation))),
      
    # Grid Power available (PHCN/NEPA)
      phcn_electricity_numerator = sum(df$phcn_electricity, na.rm=T),
      phcn_electricity_denominator = length(na.omit(df$phcn_electricity)),
      phcn_electricity_percent = round(100*sum(df$phcn_electricity, na.rm=T)/length(na.omit(df$phcn_electricity))),
      
    # Any Power Available (grid or alternative power supply) 
      any_power_available_numerator = sum(df$anypower, na.rm=T),
      any_power_available_denominator = length(na.omit(df$anypower)),
      any_power_available_percent = round(100*sum(df$anypower, na.rm=T)/length(na.omit(df$anypower))),
      
    # Emergency Transport available for referrals
      emerg_tran_numerator = sum(df$emerg_tran, na.rm=T),
      emerg_tran_denominator = length(na.omit(df$emerg_tran)),
      emerg_tran_percent = round(100*sum(df$emerg_tran, na.rm=T)/sum(df$denominator, na.rm=T)),
      
    # Sufficient Skilled Birth Attendants
      sba_numerator = sum(df$sba, na.rm=T),
      sba_denominator = length(na.omit(df$sba)),
      sba_percent = round(100*sum(df$sba, na.rm=T)/length(na.omit(df$sba))),
      
    # Delivery Services available
      delivery_services_yn_numerator = sum(df$delivery_services_yn, na.rm=T),
      delivery_services_yn_denominator = length(na.omit(df$delivery_services_yn)),
      delivery_services_yn_percent = round(100*sum(df$delivery_services_yn, na.rm=T)/
                                             length(na.omit(df$delivery_services_yn))),
      
    # C-Sections performed
      c_section_yn_numerator = sum(df$c_section_yn, na.rm=T),  
      c_section_yn_denominator = length(na.omit(df$c_section_yn)),
      c_section_yn_percent = round(100*sum(df$c_section_yn, na.rm=T)/length(na.omit(df$c_section_yn))),
      
    # Antenatal Care Services are provided
      antenatal_care_yn_numerator = sum(df$antenatal_care_yn, na.rm=T),
      antenatal_care_yn_denominator = length(na.omit(df$antenatal_care_yn)),
      antenatal_care_yn_percent = round(100*sum(df$antenatal_care_yn, na.rm=T)/
                                          length(na.omit(df$antenatal_care_yn))),
      
    # Family Planning Methods provided
      family_planning_yn_numerator = sum(df$family_planning_yn, na.rm=T),
      family_planning_yn_denominator = length(na.omit(df$family_planning_yn)),
      family_planning_yn_percent = round(100*sum(df$family_planning_yn, na.rm=T)/
                                           length(na.omit(df$family_planning_yn))),
      
    # Measles Immunizations are provided 
      child_health_measles_immun_numerator = sum(df$child_health_measles_immun, na.rm=T),    
      child_health_measles_immun_denominator = length(na.omit(df$child_health_measles_immun)),
      child_health_measles_immun_percent = round(100*sum(df$child_health_measles_immun, na.rm=T)/
                                                     length(na.omit(df$child_health_measles_immun))),
      
    # Refrigerator or Freezer for vaccine storage
      vaccines_fridge_freezer_numerator = sum(df$vaccines_fridge_freezer, na.rm=T),
      vaccines_fridge_freezer_denominator = length(na.omit(df$vaccines_fridge_freezer)),
      vaccines_fridge_freezer_percent = round(100*sum(df$vaccines_fridge_freezer, na.rm=T)/
                                                length(na.omit(df$vaccines_fridge_freezer))),
      
    # Artemisinin-based combination therapy (ACT)
      medication_anti_malarials_numerator = sum(df$medication_anti_malarials, na.rm=T),
      medication_anti_malarials_denominator = length(na.omit(df$medication_anti_malarials)),
      medication_anti_malarials_percent = round(100*sum(df$medication_anti_malarials, na.rm=T)/
                                                  df$medication_anti_malarials),
      
    # Fully staffed hospitals ?? need to follow up with Ranu
    # Fully staffed primary health centres (PHCs)
      phcentre_numerator = sum(df$phcentre, na.rm=T),
      phcentre_denominator = length(na.omit(df$phcentre)),
      phcentre_percent = round(100*sum(df$phcentre, na.rm=T)/length(na.omit(df$phcentre))),
      
    # Fully staffed primary health clinics
      phclinic_numerator = sum(df$phclinic, na.rm=T),
      phclinic_denominator = length(na.omit(df$phclinic)),
      phclinic_percent = round(100*sum(df$phclinic, na.rm=T)/length(na.omit(df$phclinic))),
      
    # Fully staffed dispensaries
      dispensary_numerator = sum(df$hpdispensary, na.rm=T),
      dispensary_denominator = length(na.omit(df$hpdispensary)),
      dispensary_percent = round(100*sum(df$hpdispensary, na.rm=T)/length(na.omit(df$hpdispensary)))
)})


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
    total_teachers = sum(df$num_tchr_full_time, na.rm=T),
  # Total Number of Students enrolled in primary education	??
  # Total Number of Students enrolled in junior secondary education	??
    
  # Total number of existing primary and junior secondary schools
    primary_js = sum(df$pj, na.rm=T),
  # Total number of existing classrooms => same as above "num_existing_classrooms"
      
  # Schools with access to improved functional water 
    improved_functional_water_numerator = sum(df$improved_functonal_water, na.rm=T),
    improved_functional_water_denominator = length(na.omit(df$improved_functonal_water)),
    improved_functional_water_percent = round(100*sum(df$improved_functonal_water, na.rm=T)/
                                                  length(na.omit(df$improved_functonal_water))),
    
  # Schools with access to improved sanitation
    improved_sanitation_numerator = sum(df$improved_sanitation, na.rm=T),
    improved_sanitation_denominator = length(na.omit(df$improved_sanitation)),
    improved_sanitation_percent = round(100*sum(df$improved_sanitation, na.rm=T)/
                                          length(na.omit(df$improved_sanitation))),
    
  # Schools connected to the national electricity grid (PHCN, NEPA)
    phcn_electricity_numerator = sum(df$phcn_electricity, na.rm=T),
    phcn_electricity_denominator = length(na.omit(df$phcn_electricity)),
    phcn_electricity_percent = round(100*sum(df$phcn_electricity, na.rm=T)/
                                       length(na.omit(df$phcn_electricity))),
    
  # Total number of classrooms with a useable chalkboard/blackboard/whiteboard
    num_classrm_w_chalkboard_numerator = sum(df$num_classrm_w_chalkboard, na.rm=T),
    num_classrm_w_chalkboard_denominator = sum(df$num_classrms_total, na.rm=T),
    num_classrm_w_chalkboard_percent = round(100*sum(df$num_classrm_w_chalkboard, na.rm=T)/
                                               sum(df$num_classrms_total, na.rm=T)),
    
  # Total number of NCE qualified teachers 
    num_tchrs_with_nce_numerator = sum(df$num_tchrs_with_nce, na.rm=T),
    num_tchrs_with_nce_denominator = sum(df$num_tchr_full_time, na.rm=T), 
    num_tchrs_with_nce_percent = round(100*sum(df$num_tchrs_with_nce, na.rm=T)/
                                         sum(df$num_tchr_full_time, na.rm=T))
    
)})


#adding basic information
h_gap_final <- merge(h_gap, lgas, by="lga_id", all=T)
#adding basic information
e_gap_final <- merge(e_gap, lgas, by="lga_id", all=T)

combined <- merge_non_redundant(h_gap_final, e_gap_final, by="lga_id")

#writing out data
write.csv(h_gap_final, "~/Code/nmis_ui_data_2ef92c15/data_774/health_gapsheet.csv", row.names=F)
write.csv(e_gap_final, "~/Code/nmis_ui_data_2ef92c15/data_774/education_gapsheet.csv", row.names=F)
write.csv(e_gap_final, "~/Code/nmis_ui_data_2ef92c15/data_774/All_774_LGA.csv", row.names=F)

    
