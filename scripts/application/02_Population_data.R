################################################################################
#
# Get population data 
# - download from CBS for each 1st of month by age and sex
# - interpolate population on intermediate dates by sex and age
# - aggregate into larger age groups over time
# 
################################################################################

population_data <- get_cbs_population()

population_data <- 
  # define all intermediate dates of interest
  expand_grid(date = seq.Date(as.Date("2020-01-01"), as.Date("2023-06-01"), by = "day"),
            sex = levels(population_data$sex),
            age = unique(population_data$age)) |> 
  left_join(population_data |> 
              mutate(date = as.Date(paste0(period, "-01"), format = "%YMM%m-%d"))) |> 
  group_by(age, sex) |>
  # linearly interpolate population for each age and sex
  mutate(population = approx(x = date, y = population, xout = date, method = "linear")$y) |> 
  group_by(date) |> 
  mutate(frac_pop = population/sum(population))

population_data <- population_data |> 
  # define age groups
  mutate(# get first elements of age groups to define age breaks
         age_group = cut(age, breaks = parameters$age_group_breaks, include.lowest = TRUE, right = FALSE,
                         labels = parameters$age_groups)) |> 
  # aggregate by age group over time
  group_by(date, age_group) |> 
  summarise(population = sum(population),
            frac_pop = sum(frac_pop)) 

