## ALIASES / PREP ##
setwd("~/Dropbox/Nigeria 661 Baseline Data Cleaning/")
source("~/Code/nmis_R_scripts/base_scripts/InstallFormhub.R")
# slugs are at https://github.com/mvpdev/nmis/blob/develop/uis_r_us/indicators/overview.json

library(plyr)
w <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Water_661_ALL_FACILITY_INDICATORS.csv")
w$`_lga_id` <- as.factor(w$X_lga_id)
iw <- idata.frame(w)

####################
#####functions#####
####################
icount <- function(predicate) { 
  counts <- table(predicate)
  if('TRUE' %in% names(counts)) { counts['TRUE'] }
  else { 0 }
}
ratio <- function(numerator_col, denominator_col, filter) {
  df <- data.frame(cbind(num=numerator_col, den=denominator_col))
  df <- na.omit(df)
  df[filter,]
  sum(df$num) / sum(df$den)
}

bool_proportion <- function(numerator_TF, denominator_TF) {
  df <- data.frame(cbind(num=numerator_TF, den=denominator_TF))
  df <- na.omit(df)
  icount((df$num & df$den)) / icount((df$den))
}

####################
#####indicators#####
####################

lgaw <- ddply(iw, .(`_lga_id`), function(df) {
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
            (df$water_point_type == "Borehole"| df$water_point_type == "Handpump"))
#####Lift Mechanism Analysis#####Only available for the 62% of the sample that has lift mech data
      num_diesel = icount(df$lift_mechanism == "Diesel"),
      percentage_diesel_functional =
          ratio(df$lift_mechanism == "Diesel" & df$functional == "Yes", df$lift_mechanism == "Diesel"),
      num_electric = icount(df$lift_mechanism == "Electric"),
      percentage_electric_functional =
          ratio(df$lift_mechanism == "Electric" & df$functional == "Yes", df$lift_mechanism == "Electric"),
      num_solar = icount(df$lift_mechanism == "Solar"),
      percentage_solar_functional =
          ratio(df$lift_mechanism == "Solar" & df$functional == "Yes", df$lift_mechanism == "Solar")
)

##########################
###### SUMMING UP ########
##########################
lga_water_all <- cbind(lgaw_facilities, lgaw_functional, lgaw_lift_mechanism) 

#adding ID info
lga_water_all$X_lga_id <- as.factor(lga_water_all$`_lga_id`)
lgas <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv")
lga_water_all <- merge(lga_water_all, lgas, by.x="X_lga_id", by.y="X_lga_id")
lga_water_all <- lga_water_all[,c(20:22,1:19)]
lga_water_all <- lga_water_all[,c(20:22,1:19)]
lga_water_all <- lga_water_all[,-5]
lga_water_all <- lga_water_all[,c(4,3,2,1,5:21)]

write.csv(lga_water_all, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Water_LGA_level_661.csv")



# 
# aaae <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Health_LGA_level_661.csv")
# > aaae$sector <- "health"
# > write.csv(aaae, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Health_LGA_level_661.csv")

