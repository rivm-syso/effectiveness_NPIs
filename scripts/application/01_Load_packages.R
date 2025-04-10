################################################################################
#
# Load packages and functions
# - define functions first (including load_package_version())
# - load required packages, checking whether
#   - package needs to be installed
#   - installed version differs from the version used in the repository
#     with optional up- or downgrade
# 
################################################################################

# Source functions
list.files(path = "./R/application", full.names = TRUE) |> 
  grep(pattern = ".r", value = TRUE) |> 
  sapply(source) |> 
  invisible()

# Load required packages with check on installed version 
c("remotes_2.5.0",
  "cbsodataR_1.0.1",               
  "ggnewscale_0.4.10",
  "ISOweek_0.6-2",
  "jsonlite_1.8.8",
  "lubridate_1.9.3",
  "nnet_7.3-20",
  "patchwork_1.2.0",
  "RColorBrewer_1.1-3",
  "scales_1.3.0",
  "tidyverse_2.0.0",
  "zoo_1.8-12") |> 
  sapply(load_package_version) |> 
  invisible()

