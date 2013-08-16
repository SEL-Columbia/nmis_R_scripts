
h <- read.csv("in_process_data/nmis/data_774/Health_LGA_level_774.csv")
h <- subset(h, subset = state == "Enugu", select=c('lga', 'proportion_health_facilities_art_treatment', 'proportion_health_facilities_hiv_testing', 'proportion_malaria_testing', 'proportion_act_treatment_for_malaria', 'proportion_malaria_prevention_pregnancy', 'proportion_offer_bednets', 'proportion_no_user_fees_malaria', 'proportion_health_facilities_tb_testing', 'proportion_any_power_access', 'proportion_improved_water_source', 'proportion_functional_sanitation', 'proportion_mobile_coverage', 'proportion_health_facilities_med_waste_separated', 'proportion_stockout_essential_meds', 'proportion_family_planning', 'proportion_delivery_no_user_fees' , 'num_skilled_health_providers_per_1000', 'num_chews_per_1000'))
names(h) <- gsub("proportion_", "", names(h))
hfinal <- h
percen <- function(df) {
  new <- df*100
  return(new)
}

h[,c(2:17)] <- colwise(percen)(h[,c(2:17)]) 



percen <- function(df) {
            new <- df*100
            return(new)
            }

write.csv(h, "~/Desktop/maduka_ENUGU_health.csv", row.names=F)


