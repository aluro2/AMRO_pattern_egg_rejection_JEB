
# Load packages -----------------------------------------------------------
library(magrittr)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(brms)
library(bayesplot)
library(tidybayes)
library(modelr)
library(extrafont)
library(ggstance)
library(ggfortify)
library(LaplacesDemon)

# Load data
data <-
  read_csv("experimental_data/pixelated_egg_rejection.csv") %>%
  drop_na(.) %>%
  janitor::clean_names() %>%
  mutate_at(vars(reject_egg), ~ recode(., y = 1, n = 0)) %>%
  mutate(clutch_diff = clutch_size_on_decision_day - clutch_start) %>%
  mutate(date_from_may_01 = scale(date_from_may_01)[,1]
  ) 

# Full model ------------------------------------------------------------

priors_nl <- c(
  prior(normal(0, 1.5), nlpar = "eta"),
  prior(student_t(10, 1, 1.5), nlpar = "eta", class = "b", coef = "intersquare_dist_mm")
)

# Plot priors
ggfortify::ggdistribution(LaplacesDemon::dst, seq(-5, 5, 0.01),  nu = 10, mu = 1, sigma = 1.5, fill = "blue")
ggfortify::ggdistribution(dnorm, seq(-5, 5, 0.01),  mean = 0, sd  = 1.5 ,  fill = "blue")

## Include asymptotes
## lower = 15% (~15% rejection rate for Cowbird-sized eggs painted a blue-green American robin mimetic color )
##  source DOI: 10.1002/ece3.3759
## upper = 85% (No eggs with "unnatural" colors were rejected at rates higher than 80%--e.g., blue, red and yellow painted Cowbird-sized eggs)
##  source DOI: 10.1007/s00265-013-1649-8

#inv_logit <- function(x) 1 / (1 + exp(-x))

model_rejection_nl<- 
  brm(
    bf(reject_egg ~ 0.15 + 0.85 * inv_logit(eta),
       eta ~ intersquare_dist_mm,
       nl = TRUE),
    data = data,
    iter = 10000,
    chains = 4,
    cores = 6,
    family = bernoulli("identity"), 
    prior = priors_nl,
    seed = 868,
    sample_prior = T,
    save_all_pars = T
)

saveRDS(model_rejection_nl, "Results/UPDATED_model_rejection_nl.RDS")

model_rejection_nl <-
  readRDS("Results/UPDATED_model_rejection_nl.RDS")

## Prior-only
model_rejection_nl_prior<- 
  brm(
    bf(reject_egg ~ 0.15 + 0.85 * inv_logit(eta),
       eta ~ intersquare_dist_mm,
       nl = TRUE),
    data = data,
    iter = 10000,
    chains = 4,
    cores = 6,
    family = bernoulli("identity"), 
    prior = priors_nl,
    seed = 868,
    sample_prior = "only",
    save_all_pars = T
  )

saveRDS(model_rejection_nl_prior, "Results/UPDATED_model_rejection_nl_prior.RDS")

model_rejection_nl_prior <-
    readRDS("Results/UPDATED_model_rejection_nl_prior.RDS")

# Plot posterior predictive check for discrete response
cairo_pdf("Figures/supp_ppcheck_prior_rootogram.pdf", width = 4, height = 4)

bayesplot::color_scheme_set(scheme = "blue")

bayesplot::pp_check(model_rejection_nl_prior, type = "rootogram") +
  labs(title = "Prior Predictive Check") +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))

dev.off()

# Traceplot
cairo_pdf("Figures/supp_traceplot.pdf", width = 6, height = 4)

bayesplot::color_scheme_set(scheme = "red")
plot(model_rejection_nl)

dev.off()

# model_rejection_nl fit ------------------------------------------
cairo_pdf("Figures/supp_ppcheck_posterior_rootogram.pdf", width = 4, height = 4)

bayesplot::color_scheme_set(scheme = "red")
bayesplot::pp_check(model_rejection_nl,
         type = "rootogram",
         nsamples = 10000)  +
  labs(title = "Posterior Predictive Check") +
  #scale_fill_manual(values = "red") +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))

dev.off()

# R2[Error] = 0.11 [0.09]
bayes_R2(model_rejection_nl, robust = T)

## Highest Density interval of interquare_dist_mm

# Posterior Median +- MAD [95% Credible Interval] = 0.92 +- 0.56 [-0.18, 2.05]
## brms
posterior_summary(model_rejection_nl, 
                  robust = T)

## tidybayes
model_rejection_nl %>% 
  spread_draws(b_eta_intersquare_dist_mm) %>%
  median_hdci()


# Get rejection threshold (inter-square distance where rejection prob = 0.5) --------------------

# treshold +- error [75% Credible Interval] = 1.175mm +- 0.58mm [0.56, 1.76]
data %>%
  data_grid(
    intersquare_dist_mm = seq_range(intersquare_dist_mm, by = 0.001),
  ) %>%
  add_fitted_draws(model_rejection_nl, n = 1000, scale = "response", seed = 10) %>%
  mutate(.value = round(.value, 2)) %>% 
  ## Filter to narrow down to values where rejection probability = 0.5
  filter(.value == 0.5) %>% 
  select(intersquare_dist_mm) %>%
  as_tibble() %>% 
  as.mcmc() %>% 
  ## Get posterior median and 75% credible interval
  posterior_summary(robust = T, probs = c(0.15, 0.85))

# Viewing angle threshold

viewing_distance_mm <- c(10,100)

viewing_angle <- 2*atan((1.18) /(viewing_distance_mm*2))
