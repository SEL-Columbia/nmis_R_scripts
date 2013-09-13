# REQUIRED PACKAGES
required_packages = data.frame(Package = c("plyr", "devtools"),
                               MinVersion = c("1.8", "1.1"), stringsAsFactors=F)
required_gh_packages = data.frame(Package = "formhub", MinVersion = "0.0.3.6.1",
                                      gh.uname = "modilabs", gh.repo = "formhub.R", stringsAsFactors=F)

row.names(required_packages) <- required_packages$Package
row.names(required_gh_packages) <- required_gh_packages$Package

# get all packages where Version of installed packages < MinVersion in required_packages
get_packages_to_install = function(required_packages) {
  installed_packages = as.data.frame(installed.packages(), stringsAsFactors=F)
  packages <- merge(installed_packages, required_packages, id="Package")
  packages$to_install <- packages$Version < packages$MinVersion
  unlist(subset(packages, to_install, select="Package"))
}
# install all packages from CRAN first (including devtools)
# required_packages must be a data frame with a Package and a corresponding MinVersion column
install_packages = function(required_packages) {
  pkglist = get_packages_to_install(required_packages)
  if(length(pkglist)) {
    print(paste("Installing", pkglist))
    install.packages(pkglist)
  }
}
# install gh packages
# required_packages must be a data frame with a Package and a corresponding MinVersion column
install_gh_packages = function(required_packages) {
  pkglist = get_packages_to_install(required_packages)
  if(length(pkglist)) {
    library(devtools)
    print(paste("Installing", pkglist))
    lapply(pkglist, function(package) {
      install_github(required_packages[package, 'gh.repo'], required_packages[package, 'gh.uname'])
    })
  }
}

# packages is just a list of package name strings
load_packages_with_install = function(packages) {
  for (re_lib in packages)
  {
    if (! re_lib %in% installed.packages())
    {
      install.packages(re_lib)    
    }
    suppressPackageStartupMessages(require(re_lib,character.only = TRUE))
  }
}

install_packages(required_packages)
install_gh_packages(required_gh_packages)

suppressPackageStartupMessages(library(formhub))
library(plyr)
