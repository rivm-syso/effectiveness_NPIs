################################################################################
#
# Determine variant transmissibility
# - from variant data containing counts of variants at sampling dates
# - assuming a gamma distributed generation interval with a shape and mean
# - fitting multinomial model
# - calculating relative transmissibility from variant growth rates
#   (assuming generation interval distribution and solving Euler-Lotka equation
#    https://royalsocietypublishing.org/doi/10.1098/rspb.2006.3754
#    https://academic.oup.com/ve/article/8/2/veac089/6752814 )
# - returning tibble with fraction and transmissibility of variant by day
#
################################################################################


determine_variant_transmissibility <- function(variant_dat, mean_gen_int, shape_gen_int ) {
  
  variant_dat <- variant_dat |>   
    mutate(days_since_start = as.integer(date - min(date)))

  fit <- multinom(variant ~ days_since_start, variant_dat, weights = count)
  
  variant_transmissibility <- tibble(variant = names(coef(fit)[, "days_since_start"]),
                            rate = coef(fit)[, "days_since_start"]) |> 
    # add reference wildtype
    add_row(variant = "WildType",
            rate = 0,
            .before = 1) |> 
    # transmissibility relative to WildType
    mutate(transmissibility = (1+rate*mean_gen_int/shape_gen_int)^shape_gen_int)
  
  # construct dataset with variant fractions at all intermediate dates
  new_data <- tibble(date = seq.Date(min(variant_dat$date), max(variant_dat$date), "day"),
                     days_since_start = min(variant_dat$days_since_start):max(variant_dat$days_since_start))
  
  variant_fractions <- new_data |> 
    bind_cols(predict(object = fit, new_data, type = 'probs')) |> 
    select(-days_since_start) |> 
    pivot_longer(cols = !date, names_to = "variant", values_to = "fraction") |> 
    mutate(variant = factor(variant, levels = c("WildType", "Alpha", "Beta", "Gamma", "Delta", "Omicron")))
  
  
 # return variant fractions over time with their relative transmissibility
 variant_fractions |> full_join(variant_transmissibility)
    
}
