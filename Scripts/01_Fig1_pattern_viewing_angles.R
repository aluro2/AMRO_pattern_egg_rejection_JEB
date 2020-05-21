
# Load packages -----------------------------------------------
library(colorspace)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Model of visual angles --------------------------------------------------


visual_angles_data <- function(intersq_dists_mm, stimulus_width_cm, viewing_distance_mm, acuity_CPD) {
  
  #intersq_dists <- round(stimulus_width_cm / n_squares, 2)* 10
  
  # min_resolvable_angle <- 1 / acuity_CPD
  
  model_data <-
    expand_grid(intersq_dists_mm, viewing_distance_mm) %>%
    mutate(viewing_angle = 2*atan((intersq_dists_mm) /(viewing_distance_mm*2))) %>% 
    arrange(intersq_dists_mm)
  
  return(model_data)
}

model_data <-
  visual_angles_data(
    intersq_dists_mm = seq(0.05, 2.6, 0.02),
    stimulus_width_cm = 6,
    viewing_distance_mm = seq(1, 300, 0.02)
  ) %>% 
  arrange(-intersq_dists_mm) 


# Acuity line -------------------------------------------------------------

# 14.54 CPD
acuity_line <-
  model_data %>%
  filter(viewing_angle > 0.068 & viewing_angle < 0.069) %>%
  lm(I(viewing_distance_mm - 10) ~ 0 + intersq_dists_mm, .  ) %>% 
  broom::tidy(.) %>% 
  mutate(eye = "American robin fovea") 

## 8.75 CPD
peripheral_acuity_line <-
  model_data %>% 
  filter(viewing_angle > 0.114 & viewing_angle < 0.118) %>%
  lm(I(viewing_distance_mm - 10) ~ 0 + intersq_dists_mm, .  ) %>% 
  broom::tidy(.)%>% 
  mutate(eye = "American robin peripheral")

# 60 CPD
acuity_line_human <-
  model_data %>%
  filter(viewing_angle > 0.015 & viewing_angle < 0.017) %>%
  lm(I(viewing_distance_mm - 10) ~ 0 + intersq_dists_mm, .  ) %>% 
  broom::tidy(.) %>% 
  mutate(eye = "Human fovea")

all_acuity_lines <-
  bind_rows(acuity_line, peripheral_acuity_line, acuity_line_human) %>% 
  select(term, estimate, eye) %>% 
  mutate(term = str_replace(term, "intersq_dists_mm", "Slope"),
         term = str_replace(term, "[()]", ""),
         term = str_replace(term, "[()]", ""),
         eye = factor(eye,
                      levels = c("Human fovea",
                                 "American robin fovea",
                                 "American robin peripheral"))) %>% 
  pivot_wider(., names_from = term, values_from = estimate) %>% 
  arrange(desc(Slope)) 


# Model Plot --------------------------------------------------------------

model_data %>% 
  #group_by(viewing_distance_mm) %>% 
  ggplot(., aes(x = intersq_dists_mm,
                ## convert viewing distance to cm for better interpretability
                y = viewing_distance_mm/10)) + 
  geom_raster(aes(fill = viewing_angle), interpolate = T) + 
  scale_fill_gradient2(
    low = "#0066ff",
    mid = "#ffff99",
    high = "#cc0000",
    limits = c(0.0005, 0.15),
    midpoint = 0.069,
    oob = scales::squish,
    na.value = "red",
    breaks = c(0.001, 0.016, 0.04, 0.069, 0.114, 0.15),
    labels = c("0.001°","0.016° Human fovea","0.04°", "0.069° American robin fovea", "0.114° American robin peripheral", "0.15°")
  ) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  scale_x_continuous(breaks = seq(0.1,2.5,0.4)) +
  xlab("Inter-square Distance (mm)") +
  ylab("Viewing Distance (cm)") +
  # Minimum resolvable angle lines
  geom_abline(
    data = all_acuity_lines,
    aes(
      # adjust intercepts and slopes to be in cm rather than mm
      intercept = 1,
      slope = Slope/10,
      linetype = eye),
    size = 1,
    alpha = 1,
    show.legend = T) + 
  scale_linetype_manual(values = c("twodash", "solid", "dotted")) +
  #scale_color_manual(values = c("seagreen4", "yellow4", "dodgerblue4")) +
  guides(fill = guide_colorbar(reverse = T, order = 1)) +
  labs(fill = "Visual Angle",
       linetype = "Minimum Resolvable Angle"
       ) +
  theme_bw() +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),
    # text = element_text(family = "Courier New"),
    panel.grid = element_blank()
  )

ggsave("Figures/visual_angles_plot.png", 
       width = 7.48,
       height = 3.5,
       units = "in",
       dpi = 800)

