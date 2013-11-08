
#functions for outlier removal
outlierOutputList = list() #for generating a running list of what is being knocked out
outlierreplace = function(df, c, rowpredicate, replaceVal=NA) {
  naCount1 <- length(which(is.na(df[,c])))
  df[,c] <- replace(df[,c], rowpredicate, replaceVal)
  naCount2 <- length(which(is.na(df[,c])))
  print(str_c(naCount2-naCount1, " outliers replaced for field: ", c)) #comment-out line if not desired
  outlierOutputList <<- c(outlierOutputList, 
                          str_c(naCount2-naCount1, " outliers replaced for field: ", c))
  df
}
