require(plyr)

e_774 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Education_Pilot_ALL_FACILITY_INDICATORS.csv", stringsAsFactors=F)
h_774 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_pilot/Health_Pilot_ALL_FACILITY_INDICATORS.csv", stringsAsFactors=F)


facility_update <- function(df, edu_bool, facility_name_col, community_col="community", ward_col="ward")
{
    if (edu_bool == T){
        e_nmis <- read.csv("~/Desktop/matching/nmis_edu.csv", stringsAsFactors=F)
        e_lga <- read.csv("~/Desktop/matching/lga_edu.csv", stringsAsFactors=F)
        e_nmis <- subset(e_nmis, select=c(short_id, long_id, lga.long_id))
        e_lga <- subset(e_lga, select=c(long_id, facility_name, community, ward))
        update <- merge(e_nmis, e_lga, by.x="lga.long_id", by.y="long_id", all.x=T)
    }else
    {
        h_nmis <- read.csv("~/Desktop/matching/nmis_health.csv", stringsAsFactors=F)
        h_lga <- read.csv("~/Desktop/matching/lga_health.csv", stringsAsFactors=F)
        h_nmis <- subset(h_nmis, select=c(short_id, long_id, lga.long_id))
        h_lga <- subset(h_lga, select=c(long_id, facility_name, community, ward))
        update <- merge(h_nmis, h_lga, by.x="lga.long_id", by.y="long_id")
    }
    
    update$lga.long_id <- NULL
    names(update)[3:5] <- c("facility_name_update", "community_update", "ward_update")
    
    df <- merge(df, update, by.x='uuid', by.y='long_id', all.x=T)
    
    df[,facility_name_col] <- replace(df[,facility_name_col], which(!is.na(df$facility_name_update)), df$facility_name_update[which(!is.na(df$facility_name_update))])
    warning(paste(length(which(!is.na(df$facility_name_update))), 'facilities have their facility_name column updated' ))
    
    df[,community_col] <- replace(df[,community_col], which(!is.na(df$community_update)), df$community_update[which(!is.na(df$community_update))])
    warning(paste(length(which(!is.na(df$community_update))), 'facilities have their community column updated' ))
    
    df[,ward_col] <- replace(df[,ward_col], which(!is.na(df$ward_update)), df$ward_update[which(!is.na(df$ward_update))])
    warning(paste(length(which(!is.na(df$ward_update))), 'facilities have their ward column updated' ))
    
    df <- subset(df, select=-c(facility_name_update, community_update, ward_update))
    return(df)
}

h_774$
e_774$school_name
test <- facility_update(e_774, edu_bool=T, facility_name_col="school_name")
test2 <- facility_update(h_774, edu_bool=F, facility_name_col="facility_name")
