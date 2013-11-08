# Update data with mop_up matching result
update_mopup <- function(df, edu_flag){
    if(edu_flag == T){
        df2 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/mop_up_matching_result/baseline_matching_education.csv", stringsAsFactors=F, na.strings = "")
    }else{
        df2 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/mop_up_matching_result/baseline_matching_health.csv", stringsAsFactors=F, na.strings = "")
    }
    df2 <- df2[!duplicated(df2$nmis_uuid),]
    
    update_df <- merge(df, df2, by.x = "uuid", by.y = "nmis_uuid", all.x=T)
    
    update_df$facility_name <- ifelse(!is.na(update_df$fcl_facility_name), 
                                      update_df$fcl_facility_name,
                                      update_df$facility_name)
    
    update_df$ward <- ifelse(!is.na(update_df$fcl_ward) & is.na(update_df$ward),
                             update_df$fcl_ward,
                             update_df$ward)
    
    update_df$community <- ifelse(!is.na(update_df$fcl_community) & is.na(update_df$community),
                                  update_df$fcl_community,
                                  update_df$community)
    update_df$fcl_ward <- NULL
    update_df$fcl_community <- NULL
    update_df$fcl_facility_name <- NULL
    
    return(update_df)
}



### Adding short_id
shortid_generate <- function(df, prefix) 
{ 
    l <- letters
    set.seed(1)
    x <- sample(0:26^4-1, dim(df)[1], replace=F)
    
    digits <- vector(mode="list", length=4)
    tmp <- x
    for (i in 4:1)
    {
        digits[[i]] <- (tmp %% 26) + 1
        tmp <- tmp %/% 26
    }
    df$short_id <- paste0(prefix,':', l[digits[[4]]],l[digits[[3]]],l[digits[[2]]],l[digits[[1]]])
    
    # test that these are unique by lga before returning
    numberofshortids <- length(unique(df$short_id))
    numberoffacilities <- length(df$short_id)
    stopifnot(numberofshortids == numberoffacilities)
    
    return(df) 
}
