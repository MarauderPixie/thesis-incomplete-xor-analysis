source("init.R")

## Data ----
extra_binom <- readRDS("data-clean/trials-transfer.rds") %>% 
  filter(item == "transfer") %>% 
  group_by(subj_id, rules, blocked) %>% 
  summarize(
    k = sum(extrapolation),
    n = n(),
    p = k / n,
    .groups = "drop"
  ) %>% 
  mutate(
    combi = case_when(
      rules == "yes" & blocked == "yes" ~ "both",
      rules == "no"  & blocked == "yes" ~ "blocked",
      rules == "yes" & blocked == "no"  ~ "rules",
      TRUE ~ "neither"
    ),
    exab4 = ifelse(k > 3, 1, 0),
    exab5 = ifelse(k > 4, 1, 0),
    exab6 = ifelse(k > 5, 1, 0)
  )

beobachtet <- extra_binom %>% 
  group_by(rules, blocked) %>% 
  summarise(
    mean_p = mean(p),
    sd_p   = sd(p),
    mean_k = mean(k),
    sd_k   = sd(k),
    mean_ckab6 = mean(exab6),
    mean_ckab5 = mean(exab5),
    .groups = "drop"
  )


#### Priors ----
# Gelman-Default
prior_null   <- set_prior("student_t(3, 0, 1)", class = "Intercept")
prior_effect <- c(
  set_prior("student_t(3, 0, 1)", class = "Intercept"), 
  set_prior("student_t(3, 0, 1)", class = "b")
)

# bounded
prior_null_bounded <- set_prior(
  "student_t(3, 0, 1)",class = "Intercept", lb = -10, ub = 10
)
prior_effect_bounded <- c(
  set_prior("student_t(3, 0, 1)", class = "Intercept", lb = -10, ub = 10), 
  set_prior("student_t(3, 0, 1)", class = "b", lb = -10, ub = 10)
)

# based on sample descriptives: number of extrapolations
prior_null_data   <- set_prior(
  "student_t(3, -1.73, 1)", class = "Intercept" # qlogis(mean(extra_binom$p))
)
prior_effect_data <- c(
  set_prior("student_t(3, -2.88, 1)", class = "Intercept"), 
  set_prior("student_t(3, -1.49, 1)", class = "b") # qlogis(mean(c(.196, .129, .229)))
)


#### Prior Comparison ----
## intercept-only model ----
fit0_default <- brm(
  data = extra_binom,
  k|trials(n) ~ 1,
  family = binomial(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

fit0_bounded <- update(fit0_default, prior = prior_null_bounded, cores = ncore)
fit0_sample  <- update(fit0_default, prior = prior_null_data, cores = ncore)

## bridgesampling 
bs0_default <- bridge_sampler(fit0_default)
bs0_bounded <- bridge_sampler(fit0_bounded)
bs0_sample  <- bridge_sampler(fit0_sample)

## leave-one-out 
loo0_default <- add_criterion(fit0_default, "marglik") %>% loo(save_psis = TRUE)
loo0_bounded <- add_criterion(fit0_bounded, "marglik") %>% loo(save_psis = TRUE)
loo0_sample  <- add_criterion(fit0_sample, "marglik") %>% loo(save_psis = TRUE)

loo_compare(loo0_default, loo0_bounded, loo0_sample)


## full model ----
fit1_default <- brm(
  data = extra_binom,
  k|trials(n) ~ blocked * rules,
  family = binomial(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

fit1_bounded <- update(fit1_default, prior = prior_effect_bounded, cores = ncore)
fit1_sample  <- update(fit1_default, prior = prior_effect_data, cores = ncore)

## bridgesampling 
bs1_default <- bridge_sampler(fit1_default)
bs1_boudned <- bridge_sampler(fit1_bounded)
bs1_sample  <- bridge_sampler(fit1_sample)

bayestestR::bayesfactor_models(fit1_default, fit1_bounded, fit1_sample)

## leave-one-out 
loo1_default <- loo(fit1_default, save_psis = TRUE)
loo1_bounded <- loo(fit1_bounded, save_psis = TRUE)
loo1_sample  <- loo(fit1_sample, save_psis = TRUE)

loo_compare(loo1_default, loo1_bounded, loo1_sample)

# -> not sure if sinnvoll, aber BF(default|sample) = 12ish,
#    margliks und loo_compare sagen eh, es mache keinen
#    nennenswerten unterschied, daher bleib ich einfach bei Gelman...

