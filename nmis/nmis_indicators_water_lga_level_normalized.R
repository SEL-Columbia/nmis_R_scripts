# slugs are at https://github.com/mvpdev/nmis/blob/develop/uis_r_us/indicators/overview.json
source("source_scripts/NMIS_Functions.R")
water_774 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Water_774_ALL_FACILITY_INDICATORS.rds")
iw774 <- idata.frame(water_774)

####################
#####indicators#####
####################

#####Type#####
lgaw_facilities <- ddply(iw774, .(lga_id), function(df) {
    data.frame(
        #####Type#####
        num_improved_water_points = 
            sum(df$is_improved, na.rm=T),
        num_overhead_tanks = 
            sum(df$water_point_type %in%
                c('Overhead Tank', 
                  'Rainwater Harvesting System'), na.rm=T),
        num_taps = 
            sum(df$water_point_type == "Tap", na.rm=T),       
        num_handpumps =
            sum(df$water_point_type %in% c('Borehole', 'Handpump'), na.rm=T),
        num_unimproved_points =
            sum((!df$is_improved), na.rm=T),                                                    
        num_total_water_points = 
            nrow(df),
        #####Functionality#####
        percentage_functional_improved = 
            ratio(df$is_improved & df$functional, df$is_improved),
        percentage_functional_taps =
            ratio(df$water_point_type == "Tap" & df$functional, df$water_point_type== "Tap"),
        percentage_functional_handpumps =
            ratio(df$water_point_type %in% c("Handpump", "Borehole") & df$is_improved & df$functional, df$water_point_type %in% c("Handpump", "Borehole")),
        
#         #####Population Served#####National standard is 250 people per water point
#         population_improved_water_points =
#             250 * icount(df$is_improved),
#         population_improved_functional_water_points = 
#             250 * icount(df$is_improved & df$functional ),
#         percentage_population_improved =
#             ratio(250 * icount(df$is_improved), df$Population), 
#         percentage_population_improved_functional = 
#             ratio(250 * icount(df$is_improved & df$functional), df$Population),
        num_diesel = 
            sum(df$lift_mechanism == "Diesel", na.rm=T),
        percentage_diesel_functional =
            ratio(df$lift_mechanism == "Diesel" & df$functional, df$lift_mechanism == "Diesel"),
        num_electric = 
            sum(df$lift_mechanism == "Electric", na.rm=T),
        percentage_electric_functional =
            ratio(df$lift_mechanism == "Electric" & df$functional, df$lift_mechanism == "Electric"),                             
        num_solar = 
            sum(df$lift_mechanism == "Solar", na.rm=T),
        percentage_solar_functional = 
            ratio(df$lift_mechanism == "Solar" & df$functional, df$lift_mechanism == "Solar")
    )})

##########################
###### SUMMING UP ########
##########################
saveRDS(lgaw_facilities, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Water_LGA_level_774.rds")
