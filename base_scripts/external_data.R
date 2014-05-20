source("base_scripts/InstallFormhub.R")
load_packages_with_install(c("foreign", "gdata", "plyr"))

# load hnlss lga state mapping data
ref <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external_data/source_data/Nigeria Master Codes_SP.csv")
ref <- ref[,c("LGA_id", "lg_hnlss", "state_hnlss")]

#Read skilled birth data and add lga id
skilled <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external_data/source_data/08_Skilled_Birth.csv") 
skilled_b <- merge(skilled, ref, by.x=c('state', 'lg'), by.y=c('state_hnlss', 'lg_hnlss'), all=T)
skilled_b<- rename(skilled_b, c("LGA_id" = "lga_id"))
skilled_birth <- subset(skilled_b,  !is.na(skilled_b$lga_id), select=c('lga_id', 'p_births'))
skilled_birth <- rename(skilled_birth, c("p_births" = "proportion_of_births_by_skilled_health_personnel"))


#Read hiv tested data and add lga id
hiv <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external_data/source_data/10_HIV_Tested.csv")
#following line is correcting percentage figure in the raw data to fit our pattern (0.0-1.0)
hiv$percentage_of_individuals_tested_for_hiv_ever <- hiv$percentage_of_individuals_tested_for_hiv_ever/100
hiv_b <- merge(hiv, ref, by.x=c('state', 'lg'), by.y=c('state_hnlss', 'lg_hnlss'), all=T)
hiv_b<- rename(hiv_b, c("LGA_id" = "lga_id"))
hiv_tested <- subset(hiv_b, !is.na(hiv_b$lga_id), select=c('lga_id', 'percentage_of_individuals_tested_for_hiv_ever'))

#Read net_enroll_na_fixed
net_enroll_na <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external_data/source_data/net enrollment NA fixed.csv")
net_enroll_JS <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external_data/source_data/net_enroll_JS_male female.csv")
other_edu <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external_data/source_data/Other edu indicators.csv")

net_enroll_na <- subset(net_enroll_na, select=-c(state, LGA))
net_enroll_JS <- subset(net_enroll_JS, select=-c(state, LGA))
other_edu <- subset(other_edu, select=-c(X, lga, state, LGA))

net_enroll_na<- rename(net_enroll_na, c("LGA_id" = "lga_id",
                                        "net_enrollment_ratio_primary_education" = "net_enrollment_rate_pry",
                                        "net_enrollment_ratio_secondary_education" = "net_enrollment_rate_js"))

net_enroll_JS<- rename(net_enroll_JS, c("LGA_id" = "lga_id"))
other_edu<- rename(other_edu, c("LGA_id" = "lga_id"))

#fix the transition rate by divide by 100
other_edu$transition_rate_primary_to_js1_male = other_edu$transition_rate_primary_to_js1_male / 100
other_edu$transition_rate_primary_to_js1_female = other_edu$transition_rate_primary_to_js1_female / 100

#########################
####### primary #########
#########################
# Load hnlss data
sec_2 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external_data/source_data/section_2.rds") 
# select records with age == 6-11,  s2a07== 11-16 (primary)
age_6_11<- sec_2[which(sapply(sec_2[,c("age")], 
                 function(x) any(x == c(6:11)))),c("lga","lg", "stlga","age","state")]
enroll_6_11<- sec_2[which(sapply(sec_2[,c("s2a07")], 
                    function(x) any(x == c(11:16)))),c("lga","lg", "stlga","s2a07","state")]

# aggegrate by lga & st_lga
age_lga<- ddply(age_6_11, .(state, lg), summarise, count_age = length(age))
enroll_lga<- ddply(enroll_6_11, .(state, lg), summarise, count_enroll = length(s2a07))

##### Next step is simply combine the two and calculate the result 
primary <- merge(age_lga, enroll_lga, all.x=T)
primary$gross_enrollment_rate_pry = primary$count_enroll/primary$count_age 
   
##########################
####### SECONDARY ########
##########################

age_12_17<- sec_2[which(sapply(sec_2[,c("age")], 
                              function(x) any(x == c(12:17)))),c("lga","lg", "stlga","age","state")]
enroll_21_26<- sec_2[which(sapply(sec_2[,c("s2a07")], 
                                 function(x) any(x == c(21:26)))),c("lga","lg", "stlga","s2a07","state")]

# aggegrate by lga & st_lga
age_lga <- ddply(age_12_17, .(state, lg), summarise, count_age = length(age))
enroll_lga <- ddply(enroll_21_26, .(state, lg), summarise, count_enroll = length(s2a07))

##### Next step is simply combine the two and calculate the result 
secondary <- merge(age_lga, enroll_lga, all.x=T)
secondary$gross_enrollment_rate_js = secondary$count_enroll/secondary$count_age 


##############################
####### LITERACY RATE ########
##############################

age_15<- sec_2[which(sec_2[,c("age")]>=15), c("lga","lg", "stlga","age","state")]
#setting qualifications for literacy
read<- sec_2[which(sec_2[,c("age")]>=15),c("lga","lg", "stlga","s2b02","s2b03","s2b04","s2b05","state")]
read <- transform(read, leer = (s2b02 == 1 | s2b03 >= 2),
                        escribir = (s2b03 == 1 | s2b05 >= 2))
read$literate <- read$leer & read$escribir
read <- read[read$literate == T,]

# aggegrate by lga & st_lga
age_lga <- ddply(age_15, .(state, lg), summarise, count_age = length(age))
read_lga <- ddply(read, .(state ,lg), summarise, count_liter = length(literate))
literacy <- merge(age_lga, read_lga)
literacy$literacy_rate <- (literacy[,"count_liter"]/literacy[,"count_age"])

##############################
####### WATER/SANITATION ########
##############################

sec_6 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external_data/source_data/hh_final.rds")
#select only records with improved water == 1:5 | 7
water_improved <- sec_6[which(sapply(as.numeric(sec_6[,c("s6f4a")]), 
                                     function(x) any(x == c(1:5,7)))),
                        c("lga","lg", "stlga","s6f4a", "state")]

#aggregate by lga
water_improved_lga <- ddply(water_improved, .(state, lg), summarise, count_improved = length(s6f4a))
total <- ddply(sec_6, .(state, lg), summarise, count_total = length(s6f4a))

#merge & calculate
i_water <- merge(water_improved_lga,total)
i_water$percentage_households_with_access_to_improved_water_sources <- (i_water$count_improved/i_water$count_total) 

#######SANITATION######

#select only records with improved water == 1:7
san_improved <- sec_6[which(sapply(as.numeric(sec_6[,c("s6f18")]), 
                                   function(x) any(x == c(1:7)))),
                                    c("lga","lg", "stlga","s6f18", "s6f20", "state")]
san_improved_2 <- subset(san_improved, s6f20 == 2)

#aggregate by lga
san_improved_lga <- ddply(san_improved_2, .(state, lg), summarise, count_improved = length(s6f18))
total_san <- ddply(sec_6, .(state, lg), summarise, count_total = length(s6f4a))

#merge & calculate
i_sanitation <- merge(san_improved_lga,total_san)
i_sanitation$percentage_households_with_access_to_improved_sanitation <- 
  (i_sanitation$count_improved/i_sanitation$count_total)

#####################################
####### Combine 3 indicators ########
#####################################

final_l <- subset(literacy, select=c(state, lg, literacy_rate))
final_p <- subset(primary, select=c(state, lg, gross_enrollment_rate_pry))
final_s <- subset(secondary, select=c(state, lg, gross_enrollment_rate_js))
final_w <- subset(i_water, select=c(state, lg, percentage_households_with_access_to_improved_water_sources))
final_san <- subset(i_sanitation, select=c(state, lg, percentage_households_with_access_to_improved_sanitation))

final <- merge(final_l, final_p, all.x=T, all.y=T)
final <- merge(final, final_s, all.x=T, all.y=T)
final <- merge(final, final_w, all.x=T, all.y=T)
final <- merge(final, final_san, all.x=T, all.y=T)
final <- transform(final, state=as.factor(as.numeric(state)))


#merging final with ref
final_merged <- merge(final, ref, by.x=c('state', 'lg'), by.y=c('state_hnlss', 'lg_hnlss'), all=T)
final_merged<- rename(final_merged, c("LGA_id" = "lga_id"))
final_merged <- subset(final_merged, !is.na(hiv_b$lga_id), select=-c(state, lg))

######################################
###Combining with hiv & skill birth###
######################################
final_total <- merge(final_merged, hiv_tested, by='lga_id', all=T)
final_total <- merge(final_total, skilled_birth, by='lga_id', all=T)
final_total <- merge(final_total, net_enroll_na, by='lga_id', all=T)
final_total <- merge(final_total, net_enroll_JS, by='lga_id', all=T)
final_total <- merge(final_total, other_edu, by='lga_id', all=T)


final_total <- subset(final_total, !is.na(lga_id) & lga_id %in% 1:774 & !duplicated(lga_id))


######################################
###    Combining DHS 2008 data     ###
######################################
dhs_2008 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external_data/source_data/dhs_2008.csv" )

final_total <- merge(final_total, dhs_2008, by = "lga_id")

######################################
###    Adding unique_lga           ###
######################################
lgas <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv")
final_total <- merge(final_total, lgas[c("lga_id", "unique_lga")], by = "lga_id")

saveRDS(final_total, '~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external_data/output_data/external_data.rds')
write.csv(final_total, '~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external_data/output_data/external_data.csv', row.names=F)

