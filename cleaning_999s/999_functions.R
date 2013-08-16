
#packages
library(data.table)
library(digest)
library(doBy)

#function for individual outlier removal
"cellst" = function(dt, cols, rows, value) {  
  if (any(rows)) {   
    set(dt, rows, cols, value)  
  }
}