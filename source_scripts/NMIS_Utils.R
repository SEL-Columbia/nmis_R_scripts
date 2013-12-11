source('base_scripts/InstallFormhub.R')
load_packages_with_install(c('doBy', 'stringr', 'digest'))

lga_corrections <- read.csv('~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/source_data/nmis_lga_corrections.csv', stringsAsFactors=FALSE)
nmis_lga_mapping <- read.csv('~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/source_data/nmis_lga_mapping.csv', stringsAsFactors=FALSE)
add_lga_id = function(df, lgacolname='mylga', statecolname='mylga_state') {
  df$unique_lga <- ifelse(df[,lgacolname] %in% c('ERROR', NA),
                          NA,
                          str_c(df[,statecolname], df[,lgacolname], sep="_"))
  df$unique_lga <- tolower(str_replace_all(df$unique_lga, "[^a-zA-Z_]", "_"))
  df$unique_lga <- recodeVar(df$unique_lga, src=lga_corrections$orginal, tgt=lga_corrections$corrected)
  df$lga_id <- as.numeric(recodeVar(as.character(df$unique_lga), src=nmis_lga_mapping$unique_slug, tgt=nmis_lga_mapping$id))
  if(any(is.na(df$lga_id))) { 
    warning(str_c("\nCANNOT ADD LGA_ID for SLUG: ",levels(factor(df[is.na(df$lga_id), 'unique_lga']))))
  }
  df
}

add_formhub_photo_url = function(df) {
    small_prefix <- 'https://formhub.org/attachment/small?media_file=ossap/attachments/'
    original_prefix <- 'http://formhub.s3.amazonaws.com/ossap/attachments/'
    df$photo_url <- str_c(original_prefix, photo)
    df$photo_url_sml <- str_c(small_prefix, df$photo)
    df
}

add_nmisstatic_photo_url = function(df) {
    attachment_prefix <- 'http://nmisstatic.s3.amazonaws.com/facimg'
    # getting the first element of the md5sum
#     md5_folder <- substr(digest(sub('\\.jpg$', '', df$photo),
#                         algo='md5', serialize=FALSE), 1, 1)
    
    md5_folder <- sapply(sub('\\.jpg$', '', df$photo), function(x) 
                    substr(digest(x,  algo='md5', serialize=FALSE), 1, 1))
    df$photo_url <- paste(attachment_prefix, md5_folder, 0, df$photo, sep='/')
    df$photo_url_sml <- paste(attachment_prefix, md5_folder, 200, df$photo, sep='/')
    df
}

add_photo_url = function(df, type) {
    # since photo are from different sources based on different dataset, switch
    # to add_formhub_photo_url if it's 661 and add_nmisstatic_photo_url 
    # for the rest
    switch(type,
           formhub = add_formhub_photo_url(df),
           nmisstatic = add_nmisstatic_photo_url(df)
           )
}

source("source_scripts/Clean_LGA_State_errors.R")
row.names(nmis_lga_mapping) <- nmis_lga_mapping$id
replace_lga_ids = function(df, lga_idcol='lga_id', lgacol='mylga', statecol='mylga_state', uuidcol='uuid') {  
  errids <- newlgaids(df, lga_idcol)
  errids[,lgacol] <- nmis_lga_mapping[errids$new_col, "slug"]
  errids[,statecol] <- str_extract(nmis_lga_mapping[errids$new_col, "unique_slug"], "[a-z]+")
  
  df[,lga_idcol] <- replace(df[,lga_idcol], df[,uuidcol] %in% errids[,uuidcol], as.numeric(errids$new_col))
  
  df[,lgacol] <- as.character(df[,lgacol])
  df[,statecol] <- as.character(df[,statecol])
  df[,lgacol] <- replace(df[,lgacol], df[,uuidcol] %in% errids[,uuidcol], errids[,lgacol])
  df[,statecol] <- replace(df[,statecol], df[,uuidcol] %in% errids[,uuidcol], errids[,statecol])
  df[,lgacol] <- as.factor(df[,lgacol])
  df[,statecol] <- as.factor(df[,statecol])
  df
}
