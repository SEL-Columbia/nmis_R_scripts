require(rjson)
require(plyr)




RJson_ouput <- function(BASE_DIR, nmis_lga, edu_774,
                        health_774, water_774){
    
    # utility function that turns Data.frame into named list
    df_to_list <- function(df){
        df_list <- split(df, rownames(df), drop=FALSE)
        df_list <- lapply(df_list, as.list)
        df_list
    }
    
    # Creating output folder
    BASE_DIR <- normalizePath(BASE_DIR)
    if (!file.exists(BASE_DIR)){
        dir.create(BASE_DIR)
        
    }
    
    # creating lgas level list
    lgas <- df_to_list(nmis_lga)
    
    # for each lga combine faciliti level indicators and append to
    # lga level indicators
    for (lga in lgas){
        
        current_lga <- lga$unique_lga
        
        facility_df <- rbind.fill(subset(edu_774, unique_lga == current_lga), 
                                  subset(health_774, unique_lga == current_lga), 
                                  subset(water_774, unique_lga == current_lga))
        
        facility_list <- df_to_list(facility_df)
        
        # remove names of the facility_list, 
        # so that the output will be a list of hash table instead of 
        # hash table with key=sequence
        names(facility_list) <- NULL
        
        lga[["facilities"]] <- facility_list
        
        output_json <- toJSON(lga)
        
        file_name <- paste(current_lga, "json", sep=".")
        output_dir <- paste(BASE_DIR, file_name, sep="/")
        
        write(output_json, output_dir)
    }
        
    
}

