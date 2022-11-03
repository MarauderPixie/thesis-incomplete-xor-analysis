source("init.R")

#### Read & prepare data ----
# demo <- readRDS("data-clean/demographics.rds")
training <- readRDS("data-clean/trials-training.rds")
transfer <- readRDS("data-clean/trials-transfer.rds")
# stimprob <- readRDS("data-clean/trials-probability.rds")

extra_all <- filter(transfer, item == "transfer")
rm(training, transfer, exctest)

#### Priors ----
# get_prior(correct ~ blocked * block + (1 + image | subj_id),
#           data = training)
# 
# priors5 <- c(
#   set_prior("normal(-1, 0.32)", class = "Intercept"), 
#   set_prior("normal(0, 0.5)", class = "b")
# )

prior_null <- c(
  set_prior("student_t(3, 0, 1)", class = "Intercept", lb = -10, ub = 10), 
  set_prior("student_t(3, 0, 1)", class = "sd")
)
prior_effect <- c(
  set_prior("student_t(3, 0, 1)", class = "Intercept", lb = -10, ub = 10), 
  set_prior("student_t(3, 0, 1)", class = "sd"), 
  set_prior("student_t(3, 0, 1)", class = "b", lb = -10, ub = 10)
)

#### Models on subsets ----
extra_rules   <- filter(extra_all, condition %in% c("control", "rules"))
extra_blocked <- filter(extra_all, condition %in% c("control", "blocked"))

## H1.1: effect of mentioning rules
# h1_rules_null1 <- brm(
#   data = extra_rules,
#   extrapolation ~ 1 + (rules | subj_id),
#   family = bernoulli(), prior = prior_effect,
#   cores = ncore, iter = 20000, warmup = 4000,
#   control = list(adapt_delta = 0.99),
#   save_pars = save_pars(all = TRUE)
# )
h1_rules_null2 <- brm(
  data = extra_rules,
  extrapolation ~ 1 + (rules || subj_id),
  family = bernoulli(), # prior = prior_effect, # would I actually apply a prior to the null model?
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)
h1_rules <- brm(
  data = extra_rules,
  extrapolation ~ rules + (rules || subj_id),
  family = bernoulli(), prior = prior_effect, 
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)

saveRDS(h1_rules_null2, "models/h1_rules_null_nocorr.rds")
saveRDS(h1_rules, "models/h1_rules_nocorr.rds")

## H1.2: Effect of blocked rule
# h1_blocked1 <- brm(
#   data = extra_blocked,
#   extrapolation ~ blocked + (blocked | subj_id),
#   family = bernoulli(), prior = prior_effect,
#   cores = ncore, iter = 20000, warmup = 4000,
#   control = list(adapt_delta = 0.99),
#   save_pars = save_pars(all = TRUE)
# )
h1_blocked_null2 <- brm(
  data = extra_blocked,
  extrapolation ~ 1 + (blocked || subj_id),
  family = bernoulli(), # prior = prior_effect, 
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)
h1_blocked2 <- brm(
  data = extra_blocked,
  extrapolation ~ blocked + (blocked || subj_id),
  family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)

saveRDS(h1_blocked_null2, "models/h1_blocked_null_nocorr.rds")
saveRDS(h1_blocked2, "models/h1_blocked_nocorr.rds")


## H1.3: Effect of both rules and blocked rule
h1_both_null1 <- brm(
  data = extra_all,
  extrapolation ~ 1 + (rules * blocked || subj_id),
  family = bernoulli(), # prior = prior_effect,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)
h1_both1 <- brm(
  data = extra_all,
  extrapolation ~ rules * blocked + (rules * blocked || subj_id),
  family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)

saveRDS(h1_both_null1, "models/h1_both_null_nocorr_full.rds")
saveRDS(h1_both1, "models/h1_both_nocorr_full.rds")

# alternatively:
extra_both <- filter(extra_all, condition %in% c("control", "both"))

h1_both_null2 <- brm(
  data = extra_both,
  extrapolation ~ 1 + (condition || subj_id),
  family = bernoulli(), # prior = prior_effect,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)
h1_both2 <- brm(
  data = extra_both,
  extrapolation ~ condition + (condition || subj_id),
  family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)

saveRDS(h1_both_null2, "models/h1_both_null_nocorr_subset.rds")
saveRDS(h1_both2, "models/h1_both_nocorr_subset.rds")

# this is a hard nut to crack: the comparison of "both" to "none" is easy,
# but comparison of say, both to rules is not: 
# that comparison uses different data in their respective models.
# Therefore it's maybe better to have both1 after all (using all data) 
# and then use brms to update() the fit; like, removing:
# - the interaction & blocked effect (m1)
# - the interaction & rules effect (m2)
# - the blocked & rules effect (m3)
# and then comparing: m3 to m1, m3 to m2

m1 <- update(h1_both1, formula. = ~ . -blocked -blocked:rules, cores = ncore)
m2 <- update(h1_both1, formula. = ~ . -rules -blocked:rules, cores = ncore)
m3 <- update(h1_both1, formula. = ~ . -rules -blocked, cores = ncore)
# m4 <- update(h1_both1, formula. = ~ . -rules -blocked -rulesno:blockedno -rulesyes:blockedno -rulesno:blockedyes, cores = ncore)

saveRDS(m1, "models/h13_m1.rds")
saveRDS(m2, "models/h13_m2.rds")
saveRDS(m3, "models/h13_m3.rds")



#### Models on full data ----
h1_null <- brm(
  data = extra_all,
  extrapolation ~ 1 + (rules * blocked || subj_id) + (rules * blocked || image),
  family = bernoulli(), prior = prior_null,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)
saveRDS(h1_null, "models/h1_null_fullish.rds")
# thing is, tho: rules and blocked conditions do not 
# "vary with the levels of the grouping factor given in group (that is: subj_id)" 
# -- Buerkner 2017, brms paper
#
# on the other hand tho:
# "In the present example, only one random term is specified in which (1 + age) are the random
#  effects and the grouping factor is patient. This implies that the intercept of the model as
#  well as the effect of age is supposed to vary between patients."
# -> the effect of rules and blocked may well be varying between subjects,
#    I'm not too sure about the images, tho...
#    sd-CIs are _really_ close to 0 too :x

# less REs
h1_null_min <- brm(
  data = extra_all,
  extrapolation ~ 1 + (1 | subj_id), # + (1 | image),
  family = bernoulli(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)
saveRDS(h1_null_min, "models/h1_null_min.rds")


## H1.1: Rules
# h1_rules <- brm(
#   data = extra_all,
#   extrapolation ~ rules + (rules * blocked || subj_id) + (rules * blocked || image),
#   family = bernoulli(), prior = prior_effect,
#   cores = ncore, iter = 20000, warmup = 4000,
#   control = list(adapt_delta = 0.9),
#   save_pars = save_pars(all = TRUE)
# )
# # -> 3 divergent transitions; probably not a problem
# saveRDS(h1_rules, "models/h1_rules_fullish.rds")

h1_rules_min <- brm(
  data = extra_all,
  extrapolation ~ rules + (1 | subj_id), # + (1 | image),
  family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup =2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)
saveRDS(h1_rules_min, "models/h1_rules_min.rds")


## H1.2: Blocked
# h1_blocked <- brm(
#   data = extra_all,
#   extrapolation ~ blocked + (rules * blocked || subj_id) + (rules * blocked || image),
#   family = bernoulli(), prior = prior_effect,
#   cores = ncore, iter = 20000, warmup = 4000,
#   control = list(adapt_delta = 0.9),
#   save_pars = save_pars(all = TRUE)
# )
# # -> 1 divergent transition; probably not a problem
# saveRDS(h1_blocked, "models/h1_blocked_fullish.rds")

h1_blocked_min <- brm(
  data = extra_all,
  extrapolation ~ blocked + (1 | subj_id), # + (1 | image),
  family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)
saveRDS(h1_blocked_min, "models/h1_blocked_min.rds")


## H1.3: Both & Interaction
# h1_both <- brm(
#   data = extra_all,
#   extrapolation ~ rules * blocked + (rules * blocked || subj_id) + (rules * blocked || image),
#   family = bernoulli(), prior = prior_effect,
#   cores = ncore, iter = 20000, warmup = 4000,
#   control = list(adapt_delta = 0.9),
#   save_pars = save_pars(all = TRUE)
# )
# # -> 220 divergent transitions with adapt_delta = 0.9 O_O
# #    looks definitely problematic when looking at the plots

h1_both_min <- brm(
  data = extra_all,
  extrapolation ~ rules * blocked + (1 | subj_id), # + (1 | image),
  family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)
saveRDS(h1_both_min, "models/h1_both_min.rds")


h1_both_noint <- brm(
  data = extra_all,
  extrapolation ~ rules + blocked + (1 | subj_id), # + (1 | image),
  family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)
saveRDS(h1_both_noint, "models/h1_both_noint.rds")



#### Model Diagnostics ----
h1_null    <- readRDS("models/h1_null_min.rds")
h1_rules   <- readRDS("models/h1_rules_min.rds")
h1_blocked <- readRDS("models/h1_blocked_min.rds")
h1_both    <- readRDS("models/h1_both_min.rds")
h1_noint   <- readRDS("models/h1_both_noint.rds")

rstan::check_divergences(h1_null$fit)
rstan::check_divergences(h1_rules$fit)
rstan::check_divergences(h1_blocked$fit)
rstan::check_divergences(h1_both$fit)
rstan::check_divergences(h1_noint$fit)

plot(h1_null)
plot(h1_rules)
plot(h1_blocked)
plot(h1_both)
plot(h1_noint)

## TO-DO: prior predictive thingy vs. posterior predictive? Oder so?

# bayes_factor(h1_null_min, h1_null)
# bayes_factor(h1_rules_min, h1_rules)
# bayes_factor(h1_blocked_min, h1_blocked)
# bayes_factor(h1_both_min, h1_both)
# # -> all BFs are at least >60 in favor of the simpler models

#### Bayes Factors ----
# H1.1: rules
bayes_factor(h1_rules, h1_null)

# H1.2: blocked
bayes_factor(h1_blocked, h1_null)

# H1.3: both
bayes_factor(h1_both, h1_blocked)
bayes_factor(h1_both, h1_rules)

bayes_factor(h1_both, h1_null)
bayes_factor(h1_both, h1_noint)



#### H2 ----
h2_data <- training %>% 
  filter(condition %in% c("control", "rules")) %>% 
  mutate(
    condition = droplevels(condition)
  ) 
  # group_by(subj_id, rules, block) %>% 
  # summarize(
  #   mean_correct = mean(correct)
  # ) %>%  ungroup()

h2_null <- brm(
  data = h2_data,
  correct ~ 1 + (1|subj_id + block),
  family = bernoulli(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)
saveRDS(h2_null, "models/h2_null.rds")

h2_rules <- brm(
  data = h2_data,
  correct ~ rules + (1|subj_id + block),
  family = bernoulli(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)
saveRDS(h2_rules, "models/h2_rules.rds")

h2_inter <- brm(
  data = h2_data,
  correct ~ rules * block + (1|subj_id + block),
  family = bernoulli(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)
saveRDS(h2_inter, "models/h2_inter.rds")


#### Diagnostics ----
h2_null  <- readRDS("models/h2_null.rds")
h2_rules <- readRDS("models/h2_rules.rds")
h2_inter <- readRDS("models/h2_inter.rds")

rstan::check_divergences(h2_null$fit)
rstan::check_divergences(h2_rules$fit)
rstan::check_divergences(h2_inter$fit)


#### Bayes Factors ----
bayes_factor(h2_rules, h2_null)

bayes_factor(h2_inter, h2_rules)
bayes_factor(h2_inter, h2_null)
