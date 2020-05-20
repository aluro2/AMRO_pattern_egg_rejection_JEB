# Load packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(brms)
library(tidybayes)
library(modelr)
library(extrafont)
library(flextable)
library(ggstance)


# Load Data and Model -----------------------------------------------------

## brms model
model_rejection_simple <-
  readRDS("Results/model_rejection_simple.RDS")


# Plot model --------------------------------------------------------------

## Add model-fitted posterior draws
plot_data <-
  model_rejection_simple$data %>%
    data_grid(
      intersquare_dist_mm = seq_range(intersquare_dist_mm, by = 0.1)
    ) %>%
    add_fitted_draws(model_rejection_simple, n = 1000, scale = "response", seed = 10) 

## Get median-fitted values for draws

median_fit <-
  plot_data %>% 
  group_by(intersquare_dist_mm) %>% 
  mutate(median_val = median(.value)) %>% 
  ungroup()


## Plot model
ggplot(median_fit, aes(x = intersquare_dist_mm, y = reject_egg)) +
  ## model fits
  stat_smooth(aes(y = .value, group = .draw), geom = "line", size = 0.5, alpha = 0.1, color = "#22BDAD", se = FALSE) +
  ## global model fit
  geom_path(aes(y = median_val), size = 1.5, alpha = 1, color = "black") +
  ## Median interval may be a bit off from median line --this is because "add_fitted_draws" takes random draws from model. Forgot to set seed for initial analysis
  geom_pointrangeh(aes(y = 0.5, x = 1.138,  xmin = 0.623, xmax = 1.675),
                   fill = "red",
                   color = "red",
                   stroke = 1,
                   size = 1.5,
                   alpha = 0.3) + 
  geom_count(data = model_rejection_simple$data, color = "#22BDAD") +
  scale_x_continuous(breaks = seq(0, 2.4, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  xlab("Inter-square distance (mm)") +
  ylab("Rejection Probability") +
  labs(size = "Number of Trials") + 
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    # text = element_text(family = "Courier New"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 

  ## Save plot
ggsave("Figures/rejection_probability.png",
       width = 7.5,
       height = 4,
       units = "in",
       dpi = 600
)
