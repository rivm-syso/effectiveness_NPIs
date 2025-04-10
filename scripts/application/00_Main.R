################################################################################
#
# Main script for estimating effectiveness of NPIs
# - define global parameters that are used in multiple scripts
# - run through scripts in order
# 
################################################################################

parameters = list(age_groups = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"),
                  age_group_breaks = c(seq(0, 70, 10), Inf),
                  incubation_period = 5, # time between infection and symptom onset
                  seropositive_delay = 6, # time between symptom onset and seropositive
                  waning_scenarios = c("medium", "fast", "slow"),
                  startR0_date = as.Date("2020-03-01"), # reference date
                  # plotting parameters
                  output_format = "png",
                  end_date = as.Date("2021-10-31"),
                  waning_colors = c("slow" = "#9ccb86",
                                    "medium" = "#009392",
                                    "fast" = "#e88471"),
                  immunity_colors = c("Naive" = "#FDDBC7",
                                      "Non-naive susceptible" = "#F4A582",
                                      "Immune after vaccination" = "#4393C3",
                                      "Immune after infection" = "#2166AC")
                  
)

source("./scripts/application/01_Load_packages.R")
source("./scripts/application/02_Population_data.R")
source("./scripts/application/03_Serosurvey_data.R")
source("./scripts/application/04_R_data.R")
source("./scripts/application/05_Case_data.R")
source("./scripts/application/06_Variant_data.R")
source("./scripts/application/07_Vaccination_data.R")
source("./scripts/application/08_Stringency_data.R")
source("./scripts/application/09_Validation_data.R")
source("./scripts/application/10_Waning_profiles.R")
source("./scripts/application/11_Immunity_over_time.R")
source("./scripts/application/12_Basic_reproduction_number_over_time.R")
source("./scripts/application/13_Effectiveness_over_time.R")
source("./scripts/application/14_Plot_data_and_results.R")
source("./scripts/application/15_Sensitivity_analyses.R")


