#Water 113: LGA level

## ALIASES / PREP ##
# slugs are at https://github.com/mvpdev/nmis/blob/develop/uis_r_us/indicators/overview.json
source("source_scripts/NMIS_Functions.R")

wat113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Water_113_ALL_FACILITY_INDICATORS.csv")

# merge in population
lgapops <- subset(read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv"), select=c("lga_id", "pop_2006"))
wat113 <- merge_strict(lgapops, wat113, by='lga_id')
wat113 <- rename(wat113, c("pop_2006" = "Population"))
iw113 <- idata.frame(wat113)

####################
#####indicators#####
####################

#####Type#####
lgaw_facilities <- ddply(iw113, .(lga_id), function(df) {
  data.frame(
    num_improved_water_points = icount(df$is_improved),
    num_taps = icount(df$water_point_type == "Tap"),       
    num_handpumps = icount(df$water_point_type %in% c('Borehole', 'Handpump')),
    num_unimproved_points = icount(! df$is_improved),                                                    
    num_total_water_points = nrow(df),
    
    percentage_functional_improved = 
      ratio(df$is_improved & df$functional== "Yes", df$is_improved),
    percentage_functional_taps =
      ratio(df$water_point_type== "Tap" & df$functional =="Yes", df$water_point_type== "Tap"),
    percentage_functional_handpumps =
      ratio(df$water_point_type== "Handpump" & df$is_improved & df$functional == "Yes", df$water_point_type== "Handpump"),
    
    #####Population Served#####National standard is 250 people per water point
    
    population_improved_water_points = 250 * icount(df$is_improved),
    population_improved_functional_water_points = 250 * icount(df$is_improved & df$functional == "Yes"),
    percentage_population_improved = ratio(250 * icount(df$is_improved), df$Population), 
    percentage_population_improved_functional = ratio(250 * icount(df$is_improved & df$functional == "Yes"), df$Population),
    
    num_diesel = icount(df$lift_mechanism == "Diesel"),
    percentage_diesel_functional =
      ratio(df$lift_mechanism == "Diesel" & df$functional == "Yes", df$lift_mechanism == "Diesel"),
    num_electric = icount(df$lift_mechanism == "Electric"),
    percentage_electric_functional =
      ratio(df$lift_mechanism == "Electric" & df$functional == "Yes", df$lift_mechanism == "Electric"),                             
    num_solar = icount(df$lift_mechanism == "Solar"),
    percentage_solar_functional = ratio(df$lift_mechanism == "Solar" & df$functional == "Yes", df$lift_mechanism == "Solar")
)})

##########################
###### SUMMING UP ########
##########################
write.csv(x_y_killa(lgaw_facilities), "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Water_LGA_level_113.csv", row.names=F)
