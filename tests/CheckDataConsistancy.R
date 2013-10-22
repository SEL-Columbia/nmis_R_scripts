
check_connsistency <- function(df1, df2){
    
    df1_nm <- names(edu_norm)
    df2_nm <- names(edu_orig)
    
    # Asserting that "uuid" must be contained in both data
    stopifnot("uuid" %in% df1_nm & "uuid" %in% df2_nm)
    # print all columns that is not in both dfs to warning 
    dropped_nm <- c(df1_nm[!df1_nm %in% df2_nm],  df2_nm[!df2_nm %in% df1_nm])
    if (length(dropped_nm) > 0)
    {
        warning(paste("The columns are not in both data sets:\n", paste(dropped_nm, collapse=", "), sep=""))
    }
    
    # prepare common names for merging the comparing data
    common_nm <- df1_nm[df1_nm %in% df2_nm]
    df1 <- df1[,common_nm]
    df2 <- df2[,common_nm]
    
    merged <- merge(df1, df2, by = "uuid")
    merged_nm <- names(merged)
    
    # take the names end with .x as left
    left_name_list <- merged_nm[grep("\\.x$", merged_nm)]
    
    err_nm <- NULL
    err_list <- NULL
    for (left in left_name_list){
        # finding the right side by replace .x with .y for every element in left_name_list
        right <- paste(gsub("\\.x$", "",left), ".y", sep='')    
        #asserting (sort of) left & right has to be in the merged data set
        stopifnot(all(right %in% merged_nm))
        f_idx <- which(merged[,left] != merged[,right])
        
        if (length(f_idx) > 0){
            err_nm <- append(err_nm, gsub("\\.x$", "",left))
            err_list <- append(err_list, length(f_idx))
            cat(paste(length(f_idx), "data points are inconsistent with the old data\n"))
        }
        
    }
    names(err_list) <- err_nm
    return(err_list)
}

compare_value <- function(df1,df2,name_string)
{
#     name_string <- f_d_nm[2]
#     df1 <- edu_norm
#     df2 <- edu_orig
    
    df1_nm <- names(df1)
    df2_nm <- names(df2)
    
    # Asserting that "uuid" must be contained in both data
    stopifnot("uuid" %in% df1_nm & "uuid" %in% df2_nm)
    
    combined_df <- merge(df1, df2, by = "uuid")
    left <- paste(name_string, ".x",sep="")
    right <- paste(name_string, ".y",sep="")
    
    combined_df <- subset(combined_df, select=c(left,right, "uuid", "src"))
    
    left_type <- class(combined_df[,left])
    right_type <- class(combined_df[,right])
    if (left_type != right_type){
        warning(sprintf("Type of data is not consistent: %s, %s", left_type, right_type))
    }
    
    diff <- combined_df[which(combined_df[,left] != combined_df[,right]),]
    cat(paste(name_string, ":", sep=" "))
    print(table(diff$src))
    print(head(diff))
    
    if (left_type %in% c("numeric", "integer") & right_type %in% c("numeric", "integer")){
        print(mean(diff[,left] - diff[,right]))
        print(summary(diff[,left] - diff[,right]))
        plot(diff[,left]/diff[,right]-1)
    }
}
