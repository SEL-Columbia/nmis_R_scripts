require(rjson)
require(plyr)
nmis_lga <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/All_774_LGA.rds")
edu_774 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Education_774_NMIS_Facility.rds")
health_774 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Health_774_NMIS_Facility.rds")
water_774 <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Water_774_NMIS_Facility.rds")




df_to_list <- function(df){
    df_list <- split(df, rownames(df), drop=FALSE)
    df_list <- lapply(df_list, as.list)
    df_list
}


    
lgas <- df_to_list(nmis_lga)

BASE_DIR <- "./json_ouput"
BASE_DIR <- normalizePath(BASE_DIR)
if (!file.exists(BASE_DIR)){
    dir.create(BASE_DIR)

}



for (lga in lgas){
    
    current_lga <- lga$unique_lga
    
    facility_df <- rbind.fill(subset(edu_774, unique_lga == current_lga), 
                                    subset(health_774, unique_lga == current_lga), 
                                    subset(water_774, unique_lga == current_lga))

    facility_list <- df_to_list(facility_df)
    names(facility_list) <- NULL
    
    lga[["facilities"]] <- facility_list
    
    output_json <- toJSON(lga)
    
    file_name <- paste(current_lga, "json", sep=".")
    output_dir <- paste(BASE_DIR, file_name, sep="/")
    
    write(output_json, output_dir)
    
    print(paste(current_lga, Sys.time(), sep = "    "))
}



