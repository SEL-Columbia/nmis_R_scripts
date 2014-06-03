nm <- NULL
i <- 1 
for (name in percent_names){
    len_error <- length(which(nmis_lga[,name] < 0))
    if (len_error > 0){
        print(paste(name, len_error, sep=": "))
    nm[i] <- name   
    i <- i+1
}}

ext <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/external_data/output_data/external_data.rds")
lgas <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv")

nm[!(nm %in% names(ext))]

nmis_lga[which(nmis_lga$proportion_teachers_nce_juniorsec > 1), "lga_id"]
nmis_lga[which(nmis_lga$percentage_population_improved > 1),"lga_id"]
nmis_lga[which(nmis_lga$percentage_population_improved_functional > 1),"lga_id"]

nmis_lga[nmis_lga$lga_id == 110, "proportion_teachers_nce_juniorsec"]
lgas[lgas$lga_id == 110, "surveying_effort"]
