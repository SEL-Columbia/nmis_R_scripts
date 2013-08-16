#Water 113: LGA level

## ALIASES / PREP ##
setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
# slugs are at https://github.com/mvpdev/nmis/blob/develop/uis_r_us/indicators/overview.json
source("scripts/source_scripts/NMIS_Utils.R")

wat113 <- read.csv("in_process_data/nmis/data_113/Water_113_ALL_FACILITY_INDICATORS.csv")
ww113 <- wat113
w113 <- merge(ww113, subset(read.csv("lgas.csv"), select=c("lga_id", "pop_2006")), by="lga_id", all.x=TRUE)
w113 <- rename(w113, c("pop_2006" = "Population"))


####################
#####indicators#####
####################

#####Type#####

lgaw_facilities <- ddply(w113, .(lga_id),
                    summarize, 
                  num_improved_water_points = length(which(is_improved == 
                                              "Yes")),
                  num_taps = length(which(water_source_type ==
                                              "Tap")),       
                  num_handpumps = length(which(water_source_type ==
                                              "Borehole")),
                  num_unimproved_points = length(which(is_improved ==
                                              "No")),                                                    
                  num_total_water_points = length(which(is_improved %in%
                                              c('Yes', 'No'))),

#####Functionality#####

                  percentage_functional_improved = 
                    ratio(is_improved== "Yes" & functional== "Yes", is_improved== "Yes"),
                  percentage_functional_taps =
                    ratio(water_source_type== "Tap" & functional=="Yes", water_source_type== "Tap"),
                  percentage_functional_handpumps =
                    ratio(water_source_type== "Borehole" & is_improved == "Yes" & 
                            functional == "Yes", water_source_type== "Borehole"),
    
#####Population Served#####National standard is 250 people per water point

                    population_improved_water_points = 250*length(which(is_improved == 
                              "Yes")),
                    population_improved_functional_water_points = 250*length(which(is_improved ==
                              "Yes" & functional == "Yes")),
                    percentage_population_improved =
                      ratio(250*length(which(is_improved == "Yes")), Population), 
                    percentage_population_improved_functional =
                      ratio(250*length(which(is_improved == "Yes" & functional == "Yes")), Population),

#####Lift Mechanism Analysis#####Only available for the 62% of the sample that has lift mech data
                       
                  num_diesel = length(which(lift_mechanism == "Diesel")),
                  
                  percentage_diesel_functional =
                    ratio(lift_mechanism == "Diesel" & functional == "Yes", lift_mechanism == "Diesel"),
                 
                  num_electric = length(which(lift_mechanism == "Electric")),
                  
                  percentage_electric_functional =
                    ratio(lift_mechanism == "Electric" & functional == "Yes", lift_mechanism == "Electric"),
                             
                  num_solar = length(which(lift_mechanism == "Solar")),
                             
                  percentage_solar_functional =
                    ratio(lift_mechanism == "Solar" & functional == "Yes", lift_mechanism == "Solar")
)

##########################
###### SUMMING UP ########
##########################

lga_w113_all <- lgaw_facilities
write.csv(lga_w113_all, "in_process_data/nmis/data_113/Water_LGA_level_113.csv", row.names=F)




