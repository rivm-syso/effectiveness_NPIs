################################################################################
#
# Load packages and functions
# 
################################################################################

# Load packages
library(tidyverse)
library(lubridate)
library(ggnewscale)
library(scales)
library(ISOweek)
library(RColorBrewer)
library(patchwork)
library(cbsodataR) # needed in 02_Population_data.R
library(jsonlite) # needed in 04_R_data.R
library(nnet) # needed in 06_Variant_data.R
library(readxl) # needed in 07_Vaccination_data.R
library(zoo) # needed in 04_R_data.R and 09_Immunity_over_time.R

# Source functions
list.files(path = "./R/application", full.names = TRUE) |> 
  grep(pattern = ".r", value = TRUE) |> 
  walk(.f = source)

