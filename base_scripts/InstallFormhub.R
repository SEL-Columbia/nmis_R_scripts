# REQUIRED PACKAGES
required_packages = data.frame(Package = c("plyr", "devtools"),
                               MinVersion = c("1.8", "1.1"), stringsAsFactors=F)
required_gh_packages = data.frame(Package = "formhub", MinVersion = "0.0.3.2",
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
install_packages = function(required_packages) {
  pkglist = get_packages_to_install(required_packages)
  if(length(pkglist)) {
    print("Installing", pkglist)
    install.packages(pkglist)
  } else {
    #print("all packages up to date; nothing to install from CRAN")
  }
}
# install gh packages
install_gh_packages = function(required_packages) {
  pkglist = get_packages_to_install(required_packages)
  if(length(pkglist)) {
    library(devtools)
    print(paste("Installing", pkglist))
    lapply(pkglist, function(package) {
      install_github(required_packages[package, 'gh.repo'], required_packages[package, 'gh.uname'])
    })
  } else {
    #print("all packages up to date; nothing to install from github")
  }
}

install_packages(required_packages)
install_gh_packages(required_gh_packages)

suppressPackageStartupMessages(library(formhub))
library(plyr)
source("~/Code/nmis_R_scripts/base_scripts/PipelineBase.R")
