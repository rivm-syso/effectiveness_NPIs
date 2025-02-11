################################################################################
#
# Load vaccination coverage data:
# - from data folder if available
# - data by week and 5-year age groups
# - split in ages by year
# - aggregate to larger age groups weighing coverage over population fraction
#   (using population of 1 Jan 2022)
# - linear interpolation to construct coverage by day 
#   (reported coverage = end coverage in that week)
# 
################################################################################

file_vaccination <- "./data/data_vaccination.csv"

if(file.exists(file_vaccination)) {
  
  vaccination_data <- read_csv(file = file_vaccination)
  
} else {
  vaccination_data_report <- read_xlsx("undisclosed_path/tabel_vaccinatiegraad_geboortejaar_cims_coronit_20220101_1031.xlsx") |> 
    filter(Leeftijdsgroep != "Leeftijd onbekend") |> 
    as_tibble() |> 
    transmute(date = ISOweek2date(paste0(DateUsedForStatistics, "-7")),
              age_group_vacc = as.factor(Leeftijdsgroep),
              coverage = `% volledig`) 

  age_group_vacc_data <- tibble(labels = levels(vaccination_data_report$age_group_vacc)) |> 
    mutate(age_group = if_else(labels == "91+", "91-105", labels),
           age_min = str_split_i(age_group, pattern = "-",  1) |> as.numeric()) |> 
    add_row(labels = "0-4", age_min = 0) |> 
    arrange(age_min)
  
  metadata <- cbs_get_meta("83482NED")
  
  # use population of 1 January 2022 for first vaccination series (download from CBS)
  population_Jan2022 <- cbs_get_data(
    "83482NED",
    Migratieachtergrond = "T001040",
    Generatie = "T001040",
    Perioden = "2022MM01",
    Geslacht = "T001038",
    Leeftijd = metadata$Leeftijd |> 
      filter(CategoryGroupID == 2) |> 
      pull(Key)
  ) |>
    cbs_add_label_columns() |>
    # recode age to integers
    mutate(
      age = gsub(Leeftijd_label, pattern = " of ouder", replacement = ""),
      age = as.integer(gsub(age, pattern = " jaar", replacement = "")),
      age_group = cut(age, breaks = c(seq(0, 70, 10), Inf), include.lowest = TRUE, right = FALSE,
                      labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")),
      population = BevolkingOpDeEersteVanDeMaand_1,
      frac_pop = population/sum(population)
    )
  
  population_vacc <- population_Jan2022 |> 
    # define age groups as used in vaccination_data_report
    mutate(age_group_vacc = cut(age, breaks = c(age_group_vacc_data$age_min, Inf), right = FALSE, include.lowest = TRUE,
                                labels = age_group_vacc_data$labels)) |> 
    group_by(age, age_group, age_group_vacc) |> 
    summarise(frac_pop = sum(frac_pop)) |> 
    ungroup()
  
  vaccination_data_by_week <- expand_grid(age = unique(population_vacc$age),
                                          date = unique(vaccination_data_report$date)) |> 
    left_join(population_vacc) |> 
    full_join(vaccination_data_report) |> 
    replace_na(replace = list(coverage = 0)) |> 
    # reweigh over new age groups
    group_by(date, age_group) |> 
    summarise(coverage = sum(coverage*frac_pop)/sum(frac_pop))
  
  
  # linear interpolation between week coverage
  vaccination_data <- expand_grid(age_group = levels(vaccination_data_by_week$age_group),
                                  date = seq(min(vaccination_data_by_week$date), max(vaccination_data_by_week$date), by = "day")) |> 
    full_join(vaccination_data_by_week) |> 
    group_by(age_group) |> 
    mutate(coverage = approxfun(x = date, y = coverage, method = "linear")(date),
           frac_vaccinated = coverage/100) |> 
    ungroup()

  write_csv(vaccination_data, file = file_vaccination)
  
  rm(metadata)
  rm(population_Jan2022)
  rm(age_group_vacc_data)
  rm(population_vacc)
  rm(vaccination_data_report)
  rm(vaccination_data_by_week)
  
}

rm(file_vaccination)


