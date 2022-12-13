source("init.R")

#### H1 ----
h1_null    <- readRDS("models/h1_transfer_re/h10.rds")
h1_blocked <- readRDS("models/h1_transfer_re/h11.rds")
h1_rules   <- readRDS("models/h1_transfer_re/h12.rds")
h1_both    <- readRDS("models/h1_transfer_re/h131.rds")
h1_inter   <- readRDS("models/h1_transfer_re/h132.rds")

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

## Bayes Factors 
bf10 <- bayestestR::bf_models(h1_null, h1_blocked, h1_rules, h1_both, h1_inter)
bayestestR::bf_models(h1_blocked, h1_rules, h1_both, h1_inter)
bayestestR::bf_models(h1_rules, h1_both, h1_inter)
bayestestR::bf_models(h1_both, h1_inter)

## ...oder von full nach reduced:
bayestestR::bf_models(h1_inter, h1_both, h1_blocked, h1_rules, h1_null)

bfs <- read_csv("BFs.csv")


#### C&K ----
ck5_null  <- readRDS("models/conkurz/ck_exab5_null.rds")
ck6_null  <- readRDS("models/conkurz/ck_exab6_null.rds")
ck5_inter <- readRDS("models/conkurz/ck_exab5_interaction.rds")
ck6_inter <- readRDS("models/conkurz/ck_exab6_interaction.rds")




#### H2 ----
# 2.1
h21_null  <- readRDS("models/h2_training/h21_null.rds")
h21_rules <- readRDS("models/h2_training/h21_rules.rds")

rstan::check_divergences(h21_null$fit)
rstan::check_divergences(h21_rules$fit)

plot(h21_null)
plot(h21_rules)

bayestestR::bayesfactor_models(h21_rules, h21_null)


# 2.2
h22_null <- readRDS("models/h2_training/h22_null.rds")
h22_rule <- readRDS("models/h2_training/h22_rule.rds")
saveRDS(h22_both,  "models/h2_training/h22_both.rds")
saveRDS(h22_inter, "models/h2_training/h22_interaction.rds")