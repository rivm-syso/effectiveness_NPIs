################################################################################
#
# Check package version
# compares locally installed versions to versions used in the repository
# - if not installed, ask to install required version
# - if older version is installed, ask to upgrade
# - if newer version is installed, ask to downgrade
# 
################################################################################

load_package_version <- function(package_version) {
  
  package <- unlist(strsplit(package_version, split = "_"))[1]
  required_version <- unlist(strsplit(package_version, split = "_"))[2]
  # define total sum to compare versions (higher number is newer version)
  required_version_total <- sum(c(10000,100,1)*as.integer(strsplit(required_version, "[-.]")[[1]]))
  
  if(package %in% installed.packages()) {
    
    installed_version <- packageVersion(package) 
    installed_version_total <- sum(c(10000,100,1)*unlist(installed_version))
    
    if(installed_version_total < required_version_total)
      
      if (menu(c("Yes", "No"),
               title= paste0("Installed version of ", package, " is ", installed_version,", older than version ", required_version, " used in repository. Do you want to upgrade?")) == "1") {
        require(remotes)
        install_version(package, version = required_version, repos = "http://cran.us.r-project.org")
      } else {print(paste0("Using older version of ", package))}
    
    if(installed_version_total > required_version_total)
      
      if (menu(c("Yes", "No"),
               title= paste("Installed version of ", package, " is ", installed_version,", newer than version ", required_version, " used in repository. Do you want to downgrade?")) == "1") {
        require(remotes)
        install_version(package, version = required_version, repos = "http://cran.us.r-project.org")
      } else {print(paste0("Using newer version of ", package))}
    
    library(package, character.only = TRUE)
    
  } else {
    
    if (menu(c("Yes", "No"),
             title= paste0("Package ", package, " is not installed. Do you want to install?")) == "1") {
      require(remotes)
      install_version(package, version = required_version, repos = "http://cran.us.r-project.org")
    } else {print(paste0("Cancelling installation of ", package, ". Some code in repository will not work."))}
    
  }
  
}
