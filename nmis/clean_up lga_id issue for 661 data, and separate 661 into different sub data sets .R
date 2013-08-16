
setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
e <- read.csv("in_process_data/outlier_cleaned/Education_661_outliercleaned.csv")


master <- read.csv("lgas.csv")

tru_113 <- master[master$surveying_effort == 113, "lga_id"]
tru_661 <- master[master$surveying_effort %in% c("Others", "148"), "lga_id"]
tru_pilot <- master[master$surveying_effort == "Pilot", "lga_id"]


table(one13 %in% tru_113)
table(six61 %in% tru_113)
table(six61 %in% tru_pilot)
table(pilot %in% tru_pilot)




six61_113 <- e[which(e$lga_id %in% tru_113),]
six61_pilot <- e[which(e$lga_id %in% tru_pilot),]
six61_na <- e[which(is.na(e$lga_id)),]
six61_661 <- e[which(e$lga_id %in% tru_661),]
