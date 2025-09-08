################################################################################
#
# Check package version
# compares locally installed versions to versions used in the repository
# - if not installed, ask to install required version
# - if older version is installed, ask to upgrade
# - if newer version is installed, ask to downgrade
# This function requires packages to be consistent in their version formats
# 
################################################################################

load_package_version <- function(package_version) {
  
  package <- unlist(strsplit(package_version, split = "_"))[1]
  required_version <- unlist(strsplit(package_version, split = "_"))[2]
  # define total sum to compare versions (higher number is newer version)
  required_digits <- as.integer(strsplit(required_version, "[-.]")[[1]])
  # fix vector to length of 3
  required_digits <- c(required_digits, rep(0, 3-length(required_digits)))
  required_version_total <- sum(rev(100^(0:2))*required_digits)
  
  if(package %in% installed.packages()) {
    
    installed_version <- packageVersion(package) 
    installed_digits <- unlist(installed_version)
    # fix vector to length of 3
    installed_digits <- c(installed_digits, rep(0, 3-length(installed_digits)))
    installed_version_total <- sum(rev(100^(0:2))*installed_digits)
    
    if(installed_version_total < required_version_total)
      
      if (menu(c("Yes", "No"),
               title= paste0("Installed version of ", package, " is ", installed_version,", older than version ", required_version, " used in repository. Do you want to upgrade?")) == "1") {
        require(remotes)
        install_version(package, version = required_version, repos = "http://cran.us.r-project.org")
      } else {print(paste0("Using older version of ", package))}
    
    if(installed_version_total > required_version_total)
      
      if (menu(c("Yes", "No"),
               title= paste0("Installed version of ", package, " is ", installed_version,", newer than version ", required_version, " used in repository. Do you want to downgrade?")) == "1") {
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
