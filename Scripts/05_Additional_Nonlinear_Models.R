

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
library(ggstance)

data <-
  read_csv("experimental_data/pixelated_egg_rejection.csv") %>%
  drop_na(.) %>%
  janitor::clean_names() %>%
  mutate_at(vars(reject_egg), ~ recode(., y = 1, n = 0)) %>%
  mutate(clutch_diff = clutch_size_on_decision_day - clutch_start) %>%
  mutate(date_from_may_01 = scale(date_from_may_01)
  ) 

# Full model ------------------------------------------------------------


priors_nl <- c(
  prior(normal(0, 6), nlpar = "eta"),
  prior(student_t(3, 0.5, 2.5), nlpar = "eta", class = "b", coef = "intersquare_dist_mm")
)

ggfortify::ggdistribution(LaplacesDemon::dst, seq(-5, 5, 0.01),  nu = 3, mu = 0, sigma = 1, fill = "blue")

## Include asymptotes
## lower = 20% (20% rejection rate for Cowbird-sized eggs painted a blue-green American robin mimetic color )
##  source DOI: 10.1002/ece3.3759
## upper = 80% (No eggs with "unnatural" colors were rejected at rates higher than 80%--e.g., blue, red and yellow painted Cowbird-sized eggs)
## source DOI: 10.1007/s00265-013-1649-8

#inv_logit <- function(x) 1 / (1 + exp(-x))

model_rejection_nl<- 
  brm(
    bf(reject_egg ~ 0.15 + 0.80 * inv_logit(eta),
       eta ~ intersquare_dist_mm, nl = TRUE),
    data = data,
    family = bernoulli("identity"), 
    prior = priors_nl
)

plot(conditional_effects(model_rejection_nl, probs = c(0.05, 0.95), spaghetti = F),  points = T)

pp_check(model_rejection_nl, type = "rootogram")

