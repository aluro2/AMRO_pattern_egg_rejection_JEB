
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
  prior(normal(0, 3), nlpar = "eta"),
  prior(student_t(10, 1, 1.5), nlpar = "eta", class = "b", coef = "intersquare_dist_mm")
)

ggfortify::ggdistribution(LaplacesDemon::dst, seq(-5, 5, 0.01),  nu = 10, mu = 1, sigma = 1.5, fill = "blue")
ggfortify::ggdistribution(dnorm, seq(-5, 5, 0.01),  mean = 0, sd  = 4 ,  fill = "blue")

## Include asymptotes
## lower = 20% (20% rejection rate for Cowbird-sized eggs painted a blue-green American robin mimetic color )
##  source DOI: 10.1002/ece3.3759
## upper = 80% (No eggs with "unnatural" colors were rejected at rates higher than 80%--e.g., blue, red and yellow painted Cowbird-sized eggs)
## source DOI: 10.1007/s00265-013-1649-8

#inv_logit <- function(x) 1 / (1 + exp(-x))

model_rejection_nl_prior<- 
  brm(
    bf(reject_egg ~ 0.20 + 0.80 * inv_logit(eta),
       eta ~ intersquare_dist_mm,
       nl = TRUE),
    data = data,
    #iter = 10000,
    #chains = 4,
    #cores = 6,
    family = bernoulli("identity"), 
    prior = priors_nl,
    seed = 868,
    sample_prior = T,
    save_all_pars = T
)

saveRDS(model_rejection_nl_prior, "Results/UPDATED_model_rejection_nl_prior.RDS")

plot(conditional_effects(model_rejection_nl_prior, probs = c(0.05, 0.95), spaghetti = F), points = T)

# Plot posterior predictive check for discrete response
pdf("Figures/supp_ppcheck_rootogram.pdf")

pp_check(model_rejection_nl_prior, type = "rootogram")

dev.off()

# Traceplot
pdf("Figures/supp_traceplot.pdf")

plot(model_rejection_nl)

dev.off()

# Bayes R2
bayes_R2(model_rejection_nl_prior)

