plot_structure <- function(color_scheme) {
  
  c("Naive" = "#FDDBC7",
    "Non-naive susceptible" = "#F4A582",
    "Immune after vaccination" = "#4393C3",
    "Immune after infection" = "#2166AC")
  
  expand_grid(y = 1:3, x = 1:3) |> 
    mutate(n = 1:n(),
           class = case_when(n == 1 ~ names(color_scheme)[1],
                             n %in% c(2, 4, 5) ~ names(color_scheme)[2],
                             n %in% c(3, 6) ~ names(color_scheme)[3],
                             TRUE ~ names(color_scheme)[4]) |> 
             factor(levels = names(color_scheme)),
           label_letter = LETTERS[1:n()]) |> 
    ggplot(aes(x = x, y = y, fill = class)) +
    geom_tile(alpha = 0.8) +
    geom_text(aes(label = label_letter),
              size = 12,
              alpha = 0.3) +
    # geom_text(aes(label = label)) +
    coord_equal() +
    scale_x_continuous(breaks = 1:3,
                       labels = c(expression("1-v"), expression("v - "~phi[vac]), expression(phi[vac])),
                       position = "top") +
    scale_y_continuous(trans = "reverse",
                       breaks = 1:3,
                       labels = c(expression("1-q"), expression("q - "~phi[inf]), expression(phi[inf]))) +
    scale_fill_manual(values = color_scheme) +
    
    labs(x = "vaccination status",
         y = "infection status",
         fill = NULL) +
    theme_minimal() +
    theme(#legend.position = "bottom",
      panel.grid = element_blank(),
      axis.text = element_text(size = 12))
  
}
