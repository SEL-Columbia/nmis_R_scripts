#packages
required_library <- c("plyr", "doBy", "stringr", "digest", "gdata",
                      "maptools", "shapefiles", "sp", "spatstat", "geosphere")

# testing required packages and install if not installed  
for (re_lib in required_library)
{
    if (! re_lib %in% installed.packages())
    {
        install.packages(re_lib)    
    }
    require(re_lib,character.only = TRUE)
    print(re_lib)
}

rm(required_library, re_lib)

# takes a vector of data, and returns a version where all the na values are replaced with 0
zeroIfNA <- function(x) { replace(x, is.na(x), 0) }

# checks if value is between two thresholds (min, max); convenience function
# if inclusive = TRUE, then value can be equal to min or max
between <- function(value, min, max, inclusive=F) { 
  if(inclusive) { 
    value >= min & value <= max 
  } else { 
    value > min & value < max 
  }
}

#reuturn the column number of those columns has .x,.y issue
#aggressive decides if we would like to drop orginal columns as well
# e.g. agressive = T ---->> lga, lga.x, lga.y are all gone
x_y_index <- function(df, aggressive = F)
{
    #getting row number of all '.x' | '.y' columns
    xy_id <- grep('(\\.x$|\\.y$)', names(df))
    xy_names <- names(df)[xy_id]
    xy_names <- unique(xy_names)
    if (aggressive == T){
        xy_names_orginal <- unique(gsub('(\\.x$|\\.y$)', '',xy_names))
        xy_names <- unique(c(xy_names, xy_names_orginal)[order(c(xy_names, xy_names_orginal))])
    }
    new_ptrn <- paste('^','(',paste(xy_names, collapse='|'),')$',sep='')
    warning(paste("Following columns will be dropped from the data.frame: \n"), paste(xy_names, collapse=', '))
    xy_id_final <- grep(new_ptrn, names(df))
    return(xy_id_final)
}

x_y_merge_lga <- function(df, aggressive=F)
{
    # this function Drops .x,.y and common columns in lgas.csv
    lgas_ref <- read.csv('~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv',
                         stringsAsFactors=F)
    lga_cols <- names(lgas_ref)[(names(lgas_ref) != 'lga_id')]
    lga_cols <- lga_cols[lga_cols %in% names(df)]
    
    id2<- which(names(df) %in% lga_cols)
    warning(paste("Following columns will be dropped from the data.frame: \n"), paste(names(df)[id2], collapse=', '))
    id1 <- x_y_index(df, aggressive)
    drop_idx <- unique(c(id2, id1))
    df <- subset(df, select = -drop_idx)
    df <- merge_strict(df, lgas_ref, by='lga_id')
    return(df)
}

# Merge two dataframes, dropping redundant columns in dataframe2 if necessary
# note: by.x and by.y are not supported
merge_non_redundant <- function(df1, df2, by, by.x=NA, by.y=NA, printDropped=F, ...) {
  stopifnot(is.na(by.x) && is.na(by.y))
  df2uniquecols <- names(df2)[! names(df2) %in% names(df1)]
  df2unique <- df2[,c(df2uniquecols, by)]
  if (printDropped) {
    print(paste(c('Dropping columns during merge: ', names(df2)[names(df2) %in% names(df1)]), collapse=' '))
  }
  merge(df1, df2unique, by, ...)
}

# a version of merge that throws up an error if there are redundant columns
merge_strict <- function(df1, df2, ...) {
  merged <- merge(df1, df2, ...)
  stopifnot(all(names(merged) %in% c(names(df1), names(df2))))
  merged
}

bool_proportion <- function(numerator_TF, denominator_TF) {
    if(is.null(numerator_TF) | is.null(denominator_TF)) {
      print("bool_proportion called on empty column")
      NA
    } else {
      if (class(numerator_TF) == 'character') {
        if (length(c(which(str_detect(numerator_TF, ignore.case("yes|no|true|false"))), 
                     which(is.na(numerator_TF)))) / length(numerator_TF) > 0.4) {
            numerator_TF <- as.logical(recodeVar(tolower(numerator_TF), src=list(c("yes", "true"), c("no", "false")), 
                      tgt=list(TRUE, FALSE), default=NA, keep.na=T))
        }
        else {
            warning("Cannot recode Boolean value, check the data first!")
        }
      } else if (class(denominator_TF) == 'character') {
        if (length(c(which(str_detect(denominator_TF, ignore.case("yes|no|true|false"))), 
                     which(is.na(denominator_TF)))) / length(denominator_TF) > 0.4) {
            denominator_TF <- as.logical(recodeVar(tolower(denominator_TF), src=list(c("yes", "true"), c("no", "false")), 
                                                 tgt=list(TRUE, FALSE), default=NA, keep.na=T))
        } else {
            warning("Cannot recode Boolean value, check the data first!")
        }
      }
      df <- data.frame(cbind(num=numerator_TF, den=denominator_TF))
      df <- na.omit(df)
      icount(df$num & df$den) / icount(df$den)
    }
}

icount <- function(predicate) { 
    counts <- table(predicate)
    if('TRUE' %in% names(counts)) { counts['TRUE'] }
    else { 0 }
}

x_y_killa <- function(merged) {
  #getting rid of all .y's, and renaming .x's to no_x
  col_remove = grep("\\.y$", colnames(merged))
  print(paste(length(grep("\\.y$", colnames(merged))), " columns has .y's were changed"))
  print(paste(length(grep("\\.x$", colnames(merged))), " columns has .x's were changed"))
  if (length(col_remove) > 0) {
    merged <- merged[,-col_remove]    
  }
  colnames(merged) <- gsub("\\.x$", "", colnames(merged))
  print(length(which(duplicated(colnames(merged)))))
  merged <- merged[,!duplicated(colnames(merged))]
  return(merged)
}

ratio <- function(numerator_col, denominator_col, filter) {
    df <- data.frame(cbind(num=numerator_col, den=denominator_col))
    df <- na.omit(df[filter,])
    if (nrow(df) == 0 | sum(df$den) == 0){
        return(NA)
    }
    return(sum(df$num) / sum(df$den))
}

any_na.rm <- function(vec) {any(vec, na.rm=T)}

boundary_clean <- function(df, state_col="mylga_state", gps_col="gps")
{
  
      
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
    hxy <- cbind(long, lat)
    
    if (length(which(is.na(hxy[,1]))) > 0 )
    {
        warning(paste(length(which(is.na(hxy[,1]))), 
                      "facility don't have GPS value, unable to locate state and were dropped from the data"))
    }
    
    hxy[which(is.na(hxy[,1])),] <- c(180, 90)
    hxy <- SpatialPoints(hxy)
    
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


facility_update <- function(df, edu_bool, facility_name_col, community_col="community", ward_col="ward")
{
    if (edu_bool == T){
        e_nmis <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Matching result/nmis_edu.csv", stringsAsFactors=F)
        e_lga <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Matching result/lga_edu.csv", stringsAsFactors=F)
        e_nmis <- subset(e_nmis, select=c(short_id, long_id, lga.long_id))
        e_lga <- subset(e_lga, select=c(long_id, facility_name, community, ward))
        update <- merge(e_nmis, e_lga, by.x="lga.long_id", by.y="long_id", all.x=T)
    }else
    {
        h_nmis <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Matching result/nmis_health.csv", stringsAsFactors=F)
        h_lga <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Matching result/lga_health.csv", stringsAsFactors=F)
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


# Use with care, the distance is euclidean distance, need to project to Great Circle
# Unfortunatly I don't know the closest point on the polygon, so that I dont really know the angle
# So the distance is roughly "correct" and need to scale to 'km' or 'mile'.

lga_boudary_dist <- function(df, gps_col)
{

    ni_shp <- readShapePoly('~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/nga_lgas/nga_lgas.shp')
    regions <- setNames(slot(ni_shp, "polygons"), ni_shp@data$lga_id)

    regions <- lapply(regions, function(x) SpatialPolygons(list(x)))
    windows <- lapply(regions, as.owin)

    dist_funs <- lapply(windows, distfun)
    
    gps <- strsplit(as.character(df[, gps_col]), split=' ')
    lat <- as.numeric(unlist(lapply(gps, function(x) x[1])))
    long <- as.numeric(unlist(lapply(gps, function(x) x[2])))

    #Error handling kick out records with unvalid gps values like 'None'
    idx <- which(!(is.na(lat) | is.na(lat)))
    if (length(idx) != 0)
    {
        warning(paste(length(which(is.na(lat) | is.na(lat))),
                      'facility has invalid gps value, 
                      and records dropped from data', sep=''))
    }
    df <- df[idx,]
    lat <- lat[idx]
    long <- long[idx]
    rm(idx)
        
    #Create spatial_point with lat&long 
    hxy <- cbind(long, lat)

    # prints warning message and replace NAs with north pole
    if (length(which(is.na(hxy[,1]))) > 0 )
    {
        warning(paste(length(which(is.na(hxy[,1]))), 
                      "facility don't have GPS value, unable to locate state and were dropped from the data"))
    }
    df <- df[which(!(is.na(hxy[,1] | is.na(hxy[,2])))),]
#     hxy[which(is.na(hxy[,1])),] <- c(180, 90)
    hxy <- hxy[which(!(is.na(hxy[,1] | is.na(hxy[,2])))),]
    #xy_cp <- apply(hxy, MARGIN=1, FUN=list)
    xy_cp <- hxy
    hxy <- SpatialPoints(hxy)


    # locate states based on x&y coordinate and shapefile
    output <- NULL
    l_ply(names(regions), function(rid) {
        r = regions[[rid]]
        #print(over(pts, r))
        output <<- replace(output,
                           over(hxy, r) == 1,
                           rid)})

    # record true LGA location and labeled LGA location 
    df$lga_valid <- output
    df$lga_orig <- as.character(df$lga_id)

    # Testing out-of-countary points(out of shapefile)
    if (length(which(is.na(df$lga_valid))) > 0 )
    {
        warning(paste(length(which(is.na(df$lga_valid))), 
                      "facility have XY coordinate outside of Nigeria coordinate, unable to locate state and were dropped from the data"))
    }
    #df <- subset(df, !is.na(lga_valid))

    # Testing out-of-LGA points
    if( length(which(df$lga_valid != df$lga_orig)) > 0  ) 
    {
        warning(paste(length(which(df$lga_valid != df$lga_orig)), 
                      "facility have out-of-boundary issue"))
    }
    print(paste(nrow(df), "Total Facilities after those without GPS "))
    df2 <- subset(df, lga_valid == lga_orig)
    xy_cp <- xy_cp[(df$lga_valid != df$lga_orig) | is.na(df$lga_valid), ]
    df <- subset(df, (lga_valid != lga_orig) | is.na(lga_valid))
    
    if (nrow(df) != nrow(xy_cp))
    {
        warning("number of out-of-LGA facility doesnt match")
    }
    print(paste(nrow(df2), "facilities NOT have out-of-LGA issue"))
    print(paste(nrow(df), "facilities HAVE out-of-LGA issue"))
    
    ######################################################
    ##### Need to optimize this part in next iteration#### 
    ######################################################
    
    
    print("dist_function")
    dist_euc <- rep(NA, nrow(df))
    system.time(l_ply(names(dist_funs), function(rid) 
    {r = dist_funs[[rid]]
     idx <- which(df$lga_id == rid)
     dist_euc[idx] <<- r(xy_cp)[idx] }))
    
    
    df$dist_euc <- dist_euc
    org_xy <- xy_cp
    fake_xy <- org_xy + dist_euc/sqrt(2)
    df$dist_fake <- distVincentySphere(org_xy,fake_xy)/1000
    
    
    
    final <- rbind.fill(df, df2)
    hist(final$dist_fake, nclass=50)
    
    return(final)

}


#for correcting indicators that are false booleans

booleanification_loop <- function(df, bool_list) {
  for (nm_bool in bool_list)
  {
    df[,nm_bool] <- as.logical(df[,nm_bool])  
  }
  df 
}
  #columns that need cleaning (can be updated accordingly)
bools <- c('not_for_private_2.immunization.bcg_immunization', 
           'not_for_private_2.immunization.bcg_immunization',
           'not_for_private_2.immunization.opv_immuization',
           'not_for_private_2.immunization.measles_immun',
           'not_for_private_2.immunization.dpt_immunization',
           'not_for_private_2.immunization.yellow_fever_immun',
           'not_for_private_2.immunization.csm_immunization',
           'not_for_private_2.immunization.hepb_immunization',
           'not_for_private_2.immunization.tetanus_immun',
           'not_for_private_2.equipment.scale',
           'not_for_private_2.supplies.muac_tape',
           'malaria_treatment_artemisinin',
           'not_for_private_2.fees_adults.paid_services_anc_delivery')


