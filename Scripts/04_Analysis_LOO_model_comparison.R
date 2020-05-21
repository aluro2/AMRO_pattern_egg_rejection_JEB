

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

data <-
  read_csv("experimental_data/pixelated_egg_rejection.csv") %>%
  drop_na(.) %>%
  janitor::clean_names() %>%
  mutate_at(vars(reject_egg), ~ recode(., y = 1, n = 0)) %>%
  mutate(clutch_diff = clutch_size_on_decision_day - clutch_start) %>%
  mutate(date_from_may_01 = scale(date_from_may_01)
  ) 

## Summary of experiments

data %>%
  group_by(intersquare_dist_mm) %>% 
  summarise(n_nests = n(),
            n_rejections = sum(reject_egg)) %>% 
  unite(`N-Rejections | N-Trials`, n_rejections:n_nests, sep = " | ") %>% 
  
  rename(`Inter-square Distance (mm)` =intersquare_dist_mm) %>% 
  qflextable() %>%
  theme_alafoli() %>%
  bold(., part = "header") %>%
  fontsize(
    part = "all",
    size = 10
  ) %>%
  color(
    part = "all",
    color = "black"
  ) %>% 
  align(
    align = "left",
    part = "all"
  ) %>%
  autofit() %>%
  print()
  #print(., preview = "pptx")


# Full model ------------------------------------------------------------


priors_logistic <- c(
  prior(student_t(3, 0, 5), class = b),
  prior(student_t(3, 0, 5), class = Intercept)
)

model_rejection <-
  brm(
    formula = reject_egg ~ intersquare_dist_mm + clutch_start + date_from_may_01,
    data = data,
    iter = 10000,
    chains = 4,
    cores = 6,
    prior = priors_logistic,
    family = bernoulli(link = "logit"),
    seed = 123,
    sample_prior = T,
    save_all_pars = T
  )

saveRDS(model_rejection, "Results/model_rejection.RDS")

# reduced model (+ clutch size, -date) ------------------------------------

priors_logistic <- c(
  prior(student_t(3, 0, 5), class = b),
  prior(student_t(3, 0, 5), class = Intercept)
)

model_rejection_clutch <-
  brm(
    formula = reject_egg ~ intersquare_dist_mm + clutch_start,
    data = data,
    iter = 10000,
    chains = 4,
    cores = 6,
    prior = priors_logistic,
    family = bernoulli(link = "logit"),
    seed = 123,
    sample_prior = T,
    save_all_pars = T
  )

saveRDS(model_rejection_clutch, "Results/model_rejection_clutch.RDS")

# reduced model (- clutch size, +date) ------------------------------------

priors_logistic <- c(
  prior(student_t(3, 0, 5), class = b),
  prior(student_t(3, 0, 5), class = Intercept)
)

model_rejection_date <-
  brm(
    formula = reject_egg ~ intersquare_dist_mm + date_from_may_01,
    data = data,
    iter = 10000,
    chains = 4,
    cores = 6,
    prior = priors_logistic,
    family = bernoulli(link = "logit"),
    seed = 123,
    sample_prior = T,
    save_all_pars = T
  )

saveRDS(model_rejection_date, "Results/model_rejection_date.RDS")


# Simple prediction mode (model egg intersq dists) ------------------------

priors_logistic <- c(
  prior(student_t(3, 0, 5), class = b),
  prior(student_t(3, 0, 5), class = Intercept)
)

model_rejection_simple <-
  brm(
    formula = reject_egg ~ intersquare_dist_mm,
    data = data,
    iter = 10000,
    chains = 4,
    cores = 6,
    prior = priors_logistic,
    family = bernoulli(link = "logit"),
    seed = 123,
    sample_prior = T,
    save_all_pars = T
  )

model_rejection_simple

saveRDS(model_rejection_simple, "Results/model_rejection_simple.RDS")

## Prior vs posterior
hyp_acuity <- hypothesis(model_rejection_simple, "intersquare_dist_mm > 0", alpha = 0.05)
hyp_acuity
plot(hyp_acuity)


# LOO model comparison ----------------------------------------------------

model_rejection <-
  readRDS("Results/model_rejection.RDS")

model_rejection_clutch <-
  readRDS("Results/model_rejection_clutch.RDS")

model_rejection_date <-
  readRDS("Results/model_rejection_date.RDS")

model_rejection_simple <-
  readRDS("Results/model_rejection_simple.RDS")

loo(model_rejection,
  model_rejection_clutch,
  model_rejection_date,
  model_rejection_simple,
  reloo = T
  ) %>%
  saveRDS(., "Results/loo_model_comparison.RDS")

loo_model_comparison <-
  readRDS("Results/loo_model_comparison.RDS")

## Make Table of Model Comparison

loo_model_comparison$diffs %>% 
  broom::tidy() %>% 
  mutate_if(is.numeric, round, 2) %>% 
  unite("ELPD LOO Difference ±SE", elpd_diff:se_diff, sep = " ±") %>% 
  rename("Model Predictors" = .rownames) %>% 
  mutate(`Model Predictors` = recode(`Model Predictors`,
                                     model_rejection_simple = "Inter-Square Difference (mm)",
                                     model_rejection_clutch = "Inter-Square Difference (mm) + Clutch Size",
                                     model_rejection_date = "Inter-Square Difference (mm) + Date",
                                     model_rejection = "Inter-Square Difference (mm) + Clutch Size + Date")) %>% 
  select(1:2) %>% 
  qflextable() %>%
  theme_alafoli() %>%
  bold(., part = "header") %>%
  fontsize(
    part = "header",
    size = 12
  ) %>%
  fontsize(
    part = "body",
    size = 12
  ) %>%
  align(
    align = "left",
    part = "all"
  ) %>%
  italic(., j = "Model Predictors") %>%
  autofit() %>%
  print()
  ## Load table in MS Powerpoint
  #print(., preview = "pptx")
  

# Model_rejection_simple fit ------------------------------------------
pp_check(model_rejection_simple,
         type = "dens_overlay",
         nsamples = 100)

bayes_R2(model_rejection_simple, robust = T)

## Highest Density interval of interquare_dist_mm

  ## brms
posterior_summary(model_rejection_simple, 
                  robust = T)

  ## tidybayes
model_rejection_simple %>% 
  spread_draws(b_intersquare_dist_mm) %>%
  median_hdci()
  

# get rejection threshold (inter-square distance where rejection prob = 0.5) --------------------


data %>%
  data_grid(
    intersquare_dist_mm = seq_range(intersquare_dist_mm, by = 0.001),
  ) %>%
  add_fitted_draws(model_rejection_simple, n = 1000, scale = "response", seed = 10) %>%
  mutate(.value = round(.value, 2)) %>% 
  ## Filter to narrow down to values where rejection probability = 0.5
  filter(.value == 0.5) %>% 
  select(intersquare_dist_mm) %>%
  as_tibble() %>% 
  as.mcmc() %>% 
  ## Get posterior median and 75% credible interval
  posterior_summary(robust = T, probs = c(0.15, 0.85))
  

