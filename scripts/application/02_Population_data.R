################################################################################
#
# Get population data 
# - download from CBS for each 1st of month
# - interpolate population on intermediate dates by sex and age
# - aggregate into larger age groups over time
# 
################################################################################

metadata <- cbs_get_meta("83482NED")

population_data <- cbs_get_data(
  "83482NED",
  Migratieachtergrond = "T001040",
  Generatie = "T001040",
  Geslacht = metadata$Geslacht |> 
    filter(Title %in% c("Mannen", "Vrouwen")) |> 
    pull(Key),
  Leeftijd = metadata$Leeftijd |> 
    filter(CategoryGroupID == 2) |> 
    pull(Key)
) |>
  cbs_add_label_columns()

population_data <- population_data |>
  # recode age to integers
  mutate(
    age = gsub(Leeftijd_label, pattern = " of ouder", replacement = ""),
    age = as.integer(gsub(age, pattern = " jaar", replacement = "")),
    sex = if_else(Geslacht_label == "Mannen", "M", "F") |> 
      as.factor()
  ) |>
  # summarise by period, age and sex
  group_by(Perioden, age, sex) |>
  summarise(population = sum(BevolkingOpDeEersteVanDeMaand_1)) |>
  ungroup()


population_data <- 
  # define all intermediate dates of interest
  expand_grid(date = seq.Date(as.Date("2020-01-01"), as.Date("2023-06-01"), by = "day"),
            sex = levels(population_data$sex),
            age = unique(population_data$age)) |> 
  left_join(population_data |> 
              mutate(date = as.Date(paste0(Perioden, "-01"), format = "%YMM%m-%d"))) |> 
  group_by(age, sex) |>
  # linearly interpolate population for each age and sex
  mutate(population = approx(x = date, y = population, xout = date, method = "linear")$y) |> 
  group_by(date) |> 
  mutate(frac_pop = population/sum(population))

population_data <- population_data |> 
  # define age groups
  mutate(sex = factor(sex),
         age_group = cut(age, breaks = c(seq(0, 70, 10), Inf), include.lowest = TRUE, right = FALSE,
                         labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"))) |> 
  # aggregate by age group over time
  group_by(date, age_group) |> 
  summarise(population = sum(population),
            frac_pop = sum(frac_pop)) 

rm(metadata)


