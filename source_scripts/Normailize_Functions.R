require(plyr)
require(doBy)
require(digest)
require(stringr)

# search for slug_names that contain part of the input string 
slugsearch <- function(nm, df=edu_661){
    names(df)[grep(nm, names(df), ignore.case=T)]
}

# Basically the table function, but shows NA value
see <- function(nm, df=edu_113)
{
    table(df[,nm],exclude=NULL)
}

#returns number of NAs in specific column
na_num <- function(vec) length(which(is.na(vec)))

#returns proportion of NAs in specific column
na_prop <- function(vec) {
    print(class(vec)) 
    na_num(vec)/length(vec)
}

#Takes a list/vector of string(names of the Data.frame) 
# and returns a function that take a STRING(slug_name/variable_name)
# Which checks if the slug_name is contained in the list of the input data.frames.
common_slug <- function(df_names)
{
    function(slug)
    {
        dfs <- lapply(df_names, function(x) get(x))
        names(dfs) <- df_names
        
        flgs <- sapply(dfs, function(x) slug %in% names(x))
        
        if(all(flgs) == T){
            sprintf("%s is contained in all data sets",slug)
        }
        else{
            sprintf("%s does NOT have slug:   %s", paste(names(dfs)[!flgs], collapse=", "), slug)
        }
    }
    
}

#Takes a list/vector of string(names of the Data.frame) 
# and returns a function that take a STRING(slug_name/variable_name)
# Which checks if the CLASS of input slug is identical in input data.frames .
common_type <- function(df_names)
{
    #Define the compareson function campare all other elements with 1st element and see if they'r all equal
    my_compare <- function(vec){
        all(sapply(vec[-1], function(x) {x == vec[1]}))   
    }
    
    function(slug)
    {
        dfs <- lapply(df_names, function(x) get(x))
        names(dfs) <- df_names
        
        my_class <- function(df_name) {
            out <- tryCatch(
                out <- class(get(df_name)[,slug]),
                error=function(cond) {
                    message(paste(df_name, ":data frame doesnt have this slug"))
                    message(cond)
                    # Choose a return value in case of error
                    return(NA)
                }
            )    
            return(out)
        }
        
        flgs <- NULL
        j <- 1
        for (i in 1:length(df_names)){
            tmp_class <- my_class(df_names[i])
            if (is.na(tmp_class) == F){
                flgs[j] <- tmp_class
                names(flgs)[j] <- df_names[i]
                j <- j+1
                
            }
        }
        
        cat(paste("\n", length(flgs), "data.frames contained", slug))
        
        if(my_compare(flgs) == T){
            cat(paste('\n', slug, "has same type in all data.frame.\n The Type of", slug, "is: ", flgs[2]))
        }else{
            warning(paste('\n',slug, " is NOT same in all data.frame"))
            warning(paste(names(flgs), collapse=", "))
            warning(paste(flgs, collapse=", "))
        }
    }
    
}

numeric_batch <- function(df, list_of_column_names){
    
    list_of_column_names <- column_exists(df, list_of_column_names)
    
    l_ply(list_of_column_names, function(col) {
        if (!class(df[,col]) %in% c("numeric", "integer")){
            suppressWarnings(df[,col] <<- as.numeric(df[,col]))
        }
    })
    return(df)
}

yes_no_converter <- function(df, col_name)
{
    vec <- df[,col_name]
    if (class(vec) != "logical"){
        num_yn <- length(grep("(yes|no|true|false)", vec,ignore.case=T))
        num_itm <- length(which(!is.na(vec)))
        yn_prop <- num_yn/num_itm
        vec <- tolower(vec)
        
        if (yn_prop < 0.99){
            warning(sprintf("%s: yes and no values has only a proportion of %.3f", col_name, yn_prop))
        }
        suppressMessages(vec <- as.logical(revalue(vec, c('yes' = TRUE,'no' = FALSE,
                                      'true' = TRUE,'false' = FALSE)
                                  )))
    }        
    return(vec)
}

yes_no_batch <- function(df, list_of_column_names)
{
    list_of_column_names <- column_exists(df, list_of_column_names)
    
    l_ply(list_of_column_names, function(col) {
            df[,col] <<- yes_no_converter(df=df, col_name=col)    

    })
    return(df)
}

column_exists <- function(df, list_of_column_names){
    if (length(which(! list_of_column_names %in% names(df))) > 0){
        warning(paste("following cloumns are not in the data.frame: ", 
                      paste(list_of_column_names[which(! list_of_column_names %in% names(df))], 
                            collapse=", ")))
        return(list_of_column_names[which(list_of_column_names %in% names(df))])    
    }else{
        return(list_of_column_names)
    }
    
}

batch_type <- function(df, list_of_column_names)
{
    list_of_column_names <- column_exists(df, list_of_column_names)
    
    types <- unlist(lapply(list_of_column_names, function(x) class(df[,x])))
    names(types) <- list_of_column_names
    return(types)
}

smart_batch_type_convert <- function(df, column_list, 
                                     type_to_list=c("logical"), convert_func){
    
    check_type <- batch_type(df, column_list)
    column_list <- names(check_type)[! check_type %in% type_to_list]
    df <- convert_func(df, column_list)
    return(df)   
}
