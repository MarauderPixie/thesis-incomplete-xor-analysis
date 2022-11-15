h1_null    <- readRDS("models/h10.rds")
h1_blocked <- readRDS("models/h11.rds")
h1_rules   <- readRDS("models/h12.rds")
h1_both    <- readRDS("models/h131.rds")
h1_inter   <- readRDS("models/h132.rds")

rstan::check_divergences(h1_null$fit)
rstan::check_divergences(h1_blocked$fit)
rstan::check_divergences(h1_rules$fit)
rstan::check_divergences(h1_both$fit)
rstan::check_divergences(h1_inter$fit)

plot(h1_null)
plot(h1_blocked)
plot(h1_rules)
plot(h1_both)
plot(h1_inter)

#### Bayes Factors ----
bayestestR::bf_models(h1_null, h1_blocked, h1_rules, h1_both, h1_inter)
bayestestR::bf_models(h1_blocked, h1_rules, h1_both, h1_inter)
bayestestR::bf_models(h1_rules, h1_both, h1_inter)
bayestestR::bf_models(h1_both, h1_inter)
