# Load nmis data
data_774 <- read.csv('~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_113/Education_113_ALL_FACILITY_INDICATORS.csv', stringsAsFactors=F)
health_661 <- read.csv('~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Health_661_ALL_FACILITY_INDICATORS.csv', stringsAsFactors=F)
water_661 <- read.csv('~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_661/Water_661_ALL_FACILITY_INDICATORS.csv', stringsAsFactors=F)

# head(data_774$gps)
# head(water_661$gps)
# 
# table(water_661$mylga_state)
# 
# df <- water_661
# state_col="mylga_state"
# gps_col="gps"


boundary_clean <- function(df, state_col="mylga_state", gps_col="gps")
{
    require(maptools)
    require(plyr)
    require(stringr)
    require(gdata)
    
    # detect #of akwa & cross in state column, and write out warnings
    n_akwa <- length(which(str_detect(df[, state_col], ignore.case("^akwa$"))))
    n_cross <- length(which(str_detect(df[, state_col], ignore.case("^cross$"))))
    if ( n_akwa > 0 )
    {
        warning(paste(n_akwa, 
                      "facility in 'Akwa_Ibom' have state spelled as 'akwa', and was replaced with correct one"))
    }
    if ( n_cross > 0 )
    {
        warning(paste(n_cross, 
                      "facility in 'Cross_River' have state spelled as 'cross', and was replaced with correct one"))
    }
    #standardize state spelling for cross_river & akwa_ibom
    df[, state_col] <- str_replace(df[, state_col], ignore.case("^akwa$"), "Akwa_Ibom")
    df[, state_col] <- str_replace(df[, state_col], ignore.case("^cross$"), "Cross_River")
    
    # Load shape file
    xx <- readShapeSpatial("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/nga_states/nga_states.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
    regions <- setNames(slot(xx, "polygons"), xx@data$Name)
    names(regions)[which(names(regions) == "Nassarawa")] <- 'Nasarawa'
    regions <- llply(regions, function(x) SpatialPolygons(list(x)))
    
    #create lat & long from gps
    gps <- strsplit(as.character(df[, gps_col]), split=' ')
    
    lat <- as.numeric(unlist(lapply(gps, function(x) x[1])))
    long <- as.numeric(unlist(lapply(gps, function(x) x[2])))
    #Create spatial_point with lat&long 
    hxy <- SpatialPoints(cbind(long, lat))
    
    # locate states based on x&y coordinate and shapefile
    output = NULL
    l_ply(names(regions), function(rid) {
        r = regions[[rid]]
        #print(over(pts, r))
        output <<- replace(output,
                           over(hxy, r) == 1,
                           rid)
    })
    df$state_valid <- output
    
    
    if (length(which(is.na(df$state_valid))) > 0 )
    {
        warning(paste(length(which(is.na(df$state_valid))), 
                      "facility have errors in XY coordinate, unable to locate state and were dropped from the data"))
    }
    df <- subset(df, !is.na(state_valid))
    
    table(df$state)
    table(df$state_valid)
    
    
    df$state_valid <- trim(toupper(str_replace_all(df$state_valid, ignore.case("(\\s|,|ABUJA)"), "")))
    df$state_orig <- trim(toupper(str_replace_all(df[,state_col], "_", "")))
    
    mis_orig <- unique(df$state_orig)[which(! unique(df$state_orig) %in% unique(df$state_valid))]
    mis_valid <- unique(df$state_valid)[which(! unique(df$state_valid) %in% unique(df$state_orig))]
    
    if( (length(mis_orig) > 0 | length(unique(df$state_orig)) > 37 ) ) 
    {
        warning("State spelling not consistant: ", mis_orig)
    }
    
    if( length(which(df$state_valid != df$state_orig)) > 0  ) 
    {
        warning(paste(length(which(df$state_valid != df$state_orig)), 
                      "facility have out-of-boundary issue, and were dropped from the data"))
    }
    
    
    df <- subset(df, df$state_valid == df$state_orig)
    return(df)
}



clean_df <- boundary_clean(data_774, "state")
clean_health <- boundary_clean(health_661, "mylga_state")
clean_water <- boudary_clean(water_661, "mylga_state")