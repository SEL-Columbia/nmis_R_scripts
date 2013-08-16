#External Data
setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
library("foreign")
require(foreign)
require(gdata)
require(plyr)


#########Combining ALl external data indicators#########

###Merging all external data
hiv <- read.csv("external data/hiv_tested.csv", stringsAsFactors=F)
hiv <- subset(hiv, select=c(lga_id, p_tested))
hiv <- hiv[which(!duplicated(hiv)),]

sb <- read.csv("external data/Skilled_Birth.csv", stringsAsFactors=F)
sb <- subset(sb, select=c(lga_id, p_births))


other <- read.csv("external data/hnlss_indicators.csv", stringsAsFactors=F)
other <- subset(other, select=c(-state_hnlss, -lg_hnlss))


all_combined <- merge(other, hiv, all=T)
all_combined <- merge(all_combined, sb, all=T)

all_combined <- all_combined[all_combined$LGA_id != 775,]


#sec_2 <- read.dta("C:/Users/Zaiming/Desktop/nigeria/section_2.dta")
sec_2 <- read.dta("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external data/section_2.dta") 
#write.csv(sec_2, "external data/hnlss_data")


#########################
####### primary #########
#########################
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
primary$gross_enrollment_ratio_primary_education = primary$count_enroll/primary$count_age 
   
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
secondary$gross_enrollment_ratio_secondary_education = primary$count_enroll/primary$count_age 


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

##writing out csvs##
#write.csv(literacy, "external data/literacy_rate.csv")


##############################
####### WATER/SANITATION ########
##############################

#sec_6 <- read.dta("C:/Users/Zaiming/Dropbox/Raw_Data/hh_final.dta")
sec_6 <- read.dta("external data/hh_final.dta")
#write.csv(sec_2, "external data/hnlss_data_SECTION_6")

#select only records with improved water == 1:5 | 7
water_improved <- sec_6[which(sapply(as.numeric(sec_6[,c("s6f4a")]), 
                                     function(x) any(x == c(1:5,7)))),c("lga","lg", "stlga","s6f4a", "state")]

#unique(water_improved$s6f4a)

#aggregate by lga
water_improved_lga <- ddply(water_improved, .(state, lg), summarise, count_improved = length(s6f4a))
total <- ddply(sec_6, .(state, lg), summarise, count_total = length(s6f4a))

#merge & calculate
i_water <- merge(water_improved_lga,total)

i_water$percentage_households_with_access_to_improved_water_sources <- (i_water$count_improved/i_water$count_total) 

#######SANITATION######

#select only records with improved water == 1:7
san_improved <- sec_6[which(sapply(as.numeric(sec_6[,c("s6f18")]), 
                                   function(x) any(x == c(1:7)))),c("lga","lg", "stlga","s6f18", "s6f20", "state")]

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
final_p <- subset(primary, select=c(state, lg, gross_enrollment_ratio_primary_education))
final_s <- subset(secondary, select=c(state, lg, gross_enrollment_ratio_secondary_education))
final_w <- subset(i_water, select=c(state, lg, percentage_households_with_access_to_improved_water_sources))
final_san <- subset(i_sanitation, select=c(state, lg, percentage_households_with_access_to_improved_sanitation))

final <- merge(final_l, final_p, all.x=T, all.y=T)
final <- merge(final, final_s, all.x=T, all.y=T)
final <- merge(final, final_w, all.x=T, all.y=T)
final <- merge(final, final_san, all.x=T, all.y=T)
final <- transform(final, state=as.factor(as.numeric(state)))

#write.csv(final, "external data/hnlss_indicators.csv")

#lg_list <- unique(sec_2[,c("lg", "state")])
#lg_list <- sec_2[!duplicated(sec_2[,c("stlga")]), c("lg", "stlga", "state")]
#lg_list$state2 <- as.factor(as.numeric(lg_list$state))


#importing master table for lga id
# master <- read.xls("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external data/Nigeria Master Codes_SP.xlsx", colClasses="character")
master <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external data/hnlss_indicators.csv")

master <- subset(master, select=c("state_hnlss", "lg_hnlss", "lga_id"))

new_final2 <- merge(final, master, by.x=c("state", "lg"), by.y=c("state_hnlss", "lg_hnlss"))
nrow(new_final2)
#id_lga <- merge(master, lg_list, by.x= c("lg_hnlss","state_hnlss"), by.y= c("lg", "state2"), all=T)

#id_lga <- transform(id_lga,X=NULL, X.1=NULL, X.2=NULL, scale_up=NULL, lga=NULL, state.y=NULL )

#master <- transform(master, X=NULL, X.1=NULL, X.2=NULL, scale_up=NULL, lga=NULL )

#new_final <- merge(master[,c("state_hnlss", "lg_hnlss", "LGA_id")], final, by.x=c("state_hnlss","lg_hnlss"), by.y=c("state", "lg"), incomparables = NA)

master <- read.csv("lgas.csv")
master$lga <- toupper(master$lga)
final$lg <- toupper(final$lg)

new_final2 <- merge(master, final, by.x="lga", by.y="lg", incomparables = NA)

#write.csv(new_final,"C:/Users/Zaiming/Dropbox/Nigeria 661 Baseline Data Cleaning (2)/external data/hnlss_indicators.csv", row.names=F)
write.csv(new_final2,"external data/hnlss_indicators2.csv", row.names=F)
write.csv(master,"external data/hnlss_LGA_Reference_Sheet.csv", row.names=F)



id_lga <- id_lga[order(id_lga[,2]),]

id_lga[duplicated(id_lga[,4]),]
lg_list[duplicated(lg_list[,"stlga"]),]

head(lg_list)


# final2 <- merge(lg_list, final, all.x=T )
# 
# 
# lga_id <- read.csv("in_process_data/nmis/Education_774_ALL_FACILITY_INDICATORS.csv")
# x_lga <- unique(lga_id$lga_id)
# 
# 
# count1 <- 0
# for (i in 6:11)
# {
#   temp <- length(sec_2[(sec_2$age == i),1])
#   count1 <- count1 + temp
#   
# }
# 
# count2 <- 0
# for (i in 11:16)
# {
#   temp <- length(sec_2[(sec_2$age == i),1])
#   count2 <- count2 + temp
#   
# }
# b1 <- table(sec_2$s2a07, useNA= c("ifany"))
# 
# sum(b1[4:9])
# ta
# 
# dim(sec_2)[1]
# 
# 


#External Data
require(foreign)
require(gdata)
require(plyr)
require(s)

#setwd("C:/Users/Zaiming/Dropbox/Nigeria 661 Baseline Data Cleaning (2)/")
setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")

#skilled <- read.csv("C:/Users/Zaiming/Dropbox/Nigeria 661 Baseline Data Cleaning (2)/external data/08_Skilled_Birth.csv")
skilled <- read.csv("external data/08_Skilled_Birth.csv") 

#ref <- read.csv("C:/Users/Zaiming/Dropbox/Nigeria 661 Baseline Data Cleaning (2)/external data/hnlss_LGA_Reference_Sheet.csv")
ref <- read.csv("external data/hnlss_LGA_Reference_Sheet.csv")

skilled_b <- merge(skilled, ref[,c("lga_id", "lg_hnlss", "state_hnlss")], by.x=c('state', 'lg'), 
                   by.y=c('state_hnlss', 'lg_hnlss'), all.x=T)


skilled_birth <- subset(skilled_b, select=c('lga_id', 'p_births'))
write.csv(skilled_birth, "external data/Skilled_Birth.csv")


hiv <- read.csv("external data/10_HIV_Tested.csv")

hiv_b <- merge(hiv, ref[,c("lga_id", "lga", "state")], by.x=c('state', 'lga'), 
               by.y=c('state', 'lga'))


hiv_tested <- subset(hiv_b, select=c('lga_id', 'p_tested'))
write.csv(hiv_tested, "external data/hiv_tested.csv")




