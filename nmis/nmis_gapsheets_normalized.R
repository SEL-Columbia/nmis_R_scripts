####################################################################################################
####################################################################################################
# Gapsheets

icount <- function(predicate) { 
  counts <- table(predicate)
  if('TRUE' %in% names(counts)) { counts['TRUE'] }
  else { 0 }
}

#health####################################################################################################
hh <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Health_774_ALL_FACILITY_INDICATORS.rds")
#new data set
h <- hh

#

#changing into idata.frame
ih <- idata.frame(h)

# LGA ID            
# LGA Name  
# State	
# Zone	
# Population of the LGA	

h_gap <- ddply(ih, .(lga_id), function(df) {
          data.frame(        
  # Total Number of Existing Primary Health Care Facilities	
    total_facilities = icount(df$facility_type %in% 
                                 c('cottagehospital', 'generalhospital', 
                                   'specialisthospital', 'teachinghospital',
                                   'primaryhealthcarecentre', 'wardmodelphccentre',
                                   'primaryhealthclinic', 'healthpostdispensary',
                                   'maternity', 'dentalclinic', 
                                   'comprehensivehealthcentre','federalmedicalcentre')),                      
  # Number of Hospitals
    total_hospitals = icount(df$facility_type %in% c('cottagehospital', 'generalhospital', 
                                                'specialisthospital', 'teachinghospital')),
  # Number of Primary Health Centres (PHCs)
    total_phcentres = icount(df$facility_type %in%
                               c('primaryhealthcarecentre', 'wardmodelphccentre')),
  # Number of Primary Health Clinics
    total_phclinics = icount(df$facility_type == 'primaryhealthclinic'),
  # Number of Dispensaries
    total_phclinics = icount(df$facility_type == 'healthpostdispensary'),
  # Total Number of Secondary and Tertiary Facilities (Health Posts + Mobile Clinics)  
    total_sec_tertiary = icount(df$facility_type %in% c('maternity', 'dentalclinic', 
                                                        'comprehensivehealthcentre',
                                                        'federalmedicalcentre')),                           

  # Improved and Functional Water Point
    improved_water_supply = icount(df$improved_water_supply) #did NOT find variable for functional water...
  # Improved Toilet 
    improved_sanitation = icount(df$improved_sanitation)  
  # Grid Power available (PHCN/NEPA)
    power_sources_grid = icount(df$power_sources_grid)
  # Any Power Available (grid or alternative power supply)
    any_power_available = icount()
  # Emergency Transport available for referrals
  # Sufficient Skilled Birth Attendants
  # Delivery Services available
  # C-Sections performed

    
    
    
# Antenatal Care Services are provided
# Family Planning Methods provided

# Measles Immunizations are provided 
# Refrigerator or Freezer for vaccine storage
# Artemisinin-based combination therapy (ACT)
# Fully staffed hospitals
# Fully staffed primary health centres (PHCs)
# Fully staffed primary health clinics
# Fully staffed dispensaries
)})

#education#################################################################################################


# LGA ID  
# LGA Name	
# State	
# Zone	
# Population of the LGA	
# Total Number of Existing Schools	
# Total Number of Existing Classrooms	
# Total Number of Teachers	
# Total Number of Students enrolled in primary education	
# Total Number of Students enrolled in junior secondary education	

# Total number of existing primary and junior secondary schools
# Total number of existing classrooms

# Schools with access to improved functional water 
# Schools with access to improved sanitation
# Schools connected to the national electricity grid (PHCN, NEPA)

# Total number of classrooms with a useable chalkboard/blackboard/whiteboard
# Total number of NCE qualified teachers 

