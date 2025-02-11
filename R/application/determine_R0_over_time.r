
determine_R0_over_time <- function(ref_date,
                                   variant_frac,
                                   startR0,
                                   season_max_date = as.Date("2020-02-01"),
                                   season_ratio) {
  
  # transform season ratio (R in winter / R in summer) to season amplitude
  season_amplitude <- (season_ratio-1)/(season_ratio+1)
  
  # construct tibble from 60 days before ref_date to end of variant fractions
  tibble(date = seq(ref_date - 60, max(variant_frac$date), by = "day")) |> 
    # include weighted transmissibility: transmissibility of variants weighted by their fraction
    full_join(variant_frac |>
                # assumption: transmissibility advantage of variants up to Delta attributable to increase of R0
                #             transmissibility advantage of Omicron attributable to immune escape
                mutate(transmissibility = if_else(variant == "Omicron", NA_real_, transmissibility),
                       transmissibility = if_else(variant == "Omicron", max(transmissibility, na.rm = TRUE), transmissibility)) |> 
                group_by(date) |> 
                summarise(weighted_transmissibility = sum(fraction*transmissibility))) |>
    fill(weighted_transmissibility, .direction = "down") |> 
    # before genomic surveillance data, only WildType assumed to circulate with transmissibility 1
    replace_na(replace = list(weighted_transmissibility = 1)) |> 
    # include sinusoidal season with maximum at season_max_date
    # season_atrefdate will be used to tie startR0 at ref_date
    mutate(#season = season_amplitude*sin(as.integer(date - season_max_date + 365/4)*(2*pi)/365),
           season = season_amplitude*cos(2*pi*(as.integer(date - season_max_date)/365)),
           season_atrefdate = if_else(date == ref_date, season, NA)) |>
    fill(season_atrefdate, .direction = "downup") |> 
    mutate(R0 = startR0*weighted_transmissibility*(1+season)/(1+season_atrefdate),
           startR0 = startR0) 
  
}
