## ALIASES / PREP ##
setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
source("scripts/InstallFormhub.R")
source("scripts/source_scripts/NMIS_Utils.R")
# slugs are at https://github.com/mvpdev/nmis/blob/develop/uis_r_us/indicators/overview.json

w <- read.csv("in_process_data/nmis/data_661/Water_661_ALL_FACILITY_INDICATORS.csv")
iw <- idata.frame(w)


####################
#####indicators#####
####################

lgaw <- ddply(iw, .(lga_id), function(df) {
  data.frame(
    #####Type#####
      num_improved_water_points = icount(df$is_improved),
      num_overhead_tanks = icount(df$water_point_type %in%
          c('Overhead Tank (1,000)', 'Overhead Tank (10,000)', 'Rainwater Harvesting System')),
      num_taps = icount(df$water_point_type == "Tap"),       
      num_handpumps = icount(df$water_point_type %in% c('Borehole', 'Handpump')),
      num_unimproved_points = icount(df$is_improved),                                                    
      num_total_water_points = nrow(df),
    #####Functionality#####
      percentage_functional_improved = 
          ratio(df$is_improved & df$functional == "Yes", df$is_improved),
      percentage_functional_taps =
          ratio(df$water_point_type == "Tap" & df$functional =="Yes", df$water_point_type == "Tap"),
      percentage_functional_handpumps =
          ratio((df$water_point_type == "Borehole" | df$water_point_type == "Handpump") & 
                (df$is_improved & df$functional == "Yes"),
            (df$water_point_type == "Borehole"| df$water_point_type == "Handpump")),
#####Lift Mechanism Analysis Only available for the 62% of the sample that has lift mech data #####
      num_diesel = icount(df$lift_mechanism == "Diesel"),
      percentage_diesel_functional =
          ratio(df$lift_mechanism == "Diesel" & df$functional == "Yes", df$lift_mechanism == "Diesel"),
      num_electric = icount(df$lift_mechanism == "Electric"),
      percentage_electric_functional =
          ratio(df$lift_mechanism == "Electric" & df$functional == "Yes", df$lift_mechanism == "Electric"),
      num_solar = icount(df$lift_mechanism == "Solar"),
      percentage_solar_functional =
          ratio(df$lift_mechanism == "Solar" & df$functional == "Yes", df$lift_mechanism == "Solar")
)})

##########################
###### SUMMING UP ########
##########################
lga_water_all <- lgaw

#adding ID info
lga_water_all$lga_id <- as.factor(lga_water_all$lga_id)
lgas <- read.csv("lgas.csv")
lga_water_all <- merge(lga_water_all, lgas, by="lga_id")
lga_water_all <- lga_water_all[,c(19:21,1:18)]
lga_water_all <- lga_water_all[,-5]
lga_water_all <- lga_water_all[,c(4,3,2,1,5:20)]

write.csv(lga_water_all, "in_process_data/nmis/data_661/Water_LGA_level_661.csv", row.names=F)
