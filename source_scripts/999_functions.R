source('base_scripts/InstallFormhub.R')
#packages
load_packages_with_install(c("data.table", "digest", "doBy"))

#function for individual outlier removal
"cellst" = function(dt, cols, rows, value) {  
  if (any(rows)) {   
    set(dt, rows, cols, value)  
  }
}

#function for clarfying what is removed in above function
cells_help_clean <- function(dt, cols, rows, value) {  
  if (any(rows)) {   
    print(dt$uuid[rows])
    set(dt, rows, cols, value)  
  }
}

