# Load packages -----------------------------------------------------------
library(magrittr)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(brms)
library(tidybayes)
library(modelr)
library(extrafont)
library(flextable)
library(ggstance)


# Load Data and Model -----------------------------------------------------

## brms model
model_rejection_nl <-
  readRDS("Results/UPDATED_model_rejection_nl.RDS")

# Plot model --------------------------------------------------------------

## Add model-fitted posterior draws
plot_data <-
  model_rejection_nl$data %>%
  data_grid(
    intersquare_dist_mm = seq_range(intersquare_dist_mm, by = 0.1)
  ) %>%
  add_fitted_draws(model_rejection_nl, n = 10000, scale = "response", seed = 10) 

## Get median-fitted values for draws

median_fit <-
  plot_data %>% 
  group_by(intersquare_dist_mm) %>% 
  mutate(median_val = median(.value)) %>% 
  ungroup()


## Plot model

plot <-
ggplot(median_fit, aes(x = intersquare_dist_mm, y = reject_egg)) +
  ## model fits
  stat_smooth(aes(y = .value, group = .draw), geom = "line", size = 0.5, alpha = 0.01, color = "#22BDAD", se = FALSE) +
  ## global model fit
  geom_path(aes(y = median_val), size = 1.5, alpha = 1, color = "black") +
  ## 0.5 rejection probability threshold
  geom_segment(x = 1.175, xend = 1.175, y = 0, yend = 0.5,
               color = "red",
               alpha = 0.3,
               size = 1.5,
               linetype = "dotted") +
  geom_pointrangeh(aes(y = 0.5, x = 1.175,  xmin = 0.561, xmax = 1.755),
                   fill = "red",
                   color = "red",
                   stroke = 1,
                   size = 1.5,
                   alpha = 0.3) + 
  geom_count(data = model_rejection_nl$data,
             shape = 21,
             fill = "#22BDAD",
             color = "black") +
  scale_x_continuous(breaks = seq(0, 2.4, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  xlab("Inter-square distance (mm)") +
  ylab("Rejection Probability") +
  labs(size = "Number of Trials") + 
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    # text = element_text(family = "Courier New"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 

## Save plot
ggsave("Figures/rejection_probability_nl.png",
       plot = plot,
       width = 7.5,
       height = 4,
       units = "in",
       dpi = 600
)
