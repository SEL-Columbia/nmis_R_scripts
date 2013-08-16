
#packages
required_library <- c("data.table", "digest", "doBy")

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

#function for individual outlier removal
"cellst" = function(dt, cols, rows, value) {  
  if (any(rows)) {   
    set(dt, rows, cols, value)  
  }
}

rm(required_library, re_lib)

# cellst <- function(df, cols, rows, value)
# {
#     if (any(rows))
#     {
#         df[rows, cols] <- value    
#     }
#     return(df)
# }

