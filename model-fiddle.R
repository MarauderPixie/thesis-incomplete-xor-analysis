source("init.R")

#### Data Preparation ----
# With aggregation to binomial and different cutoffs
# as to when someone counts as "extrapolator"
extra_berni <- readRDS("data-clean/trials-transfer.rds") %>% 
  filter(item == "transfer")

extra_binom <- extra_berni %>% 
  group_by(subj_id, rules, blocked) %>% 
  summarize(
    k = sum(extrapolation),
    n = n(),
    p = k / n
  ) %>% 
  ungroup() %>% 
  mutate(
    exab4 = ifelse(k > 3, 1, 0),
    exab5 = ifelse(k > 4, 1, 0),
    exab6 = ifelse(k > 5, 1, 0)
  )


#### Priors ----
# some odds (ratios)
0.3 / 0.7 # <- baseline odds
0.4 / 0.6 # <- rather large effect odds
(.4/.6) / (.3/.7) # respective odds ratio?

prior_null <- c(
  # bc 30% in log odds = log(.3 / .7) = -0.8472979:
  set_prior("student_t(3, -0.85, 1)", 
            class = "Intercept", lb = -11, ub = 9),
  # ...oder doch eher: log((0.3 / 0.7) / (1 - (0.3 / 0.7)))?
  set_prior("student_t(3, 0, 1)", class = "sd", lb = 0)
)
prior_effect <- c(
  set_prior("student_t(3, -0.85, 1)", class = "Intercept", lb = -11, ub = 9), 
  set_prior("student_t(3, 0, 1)", class = "sd", lb = 0), 
  set_prior("student_t(3, 0, 1)", class = "b", lb = -10, ub = 10)
)

#### Binomial models (instead of bernoulli) ----
h1_null <- brm(
  data = extra_binom,
  k|trials(n) ~ 1,
  family = binomial(), prior = prior_null[1,],
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

h1_null_rnd <- brm(
  data = extra_binom,
  k|trials(n) ~ 1 + (1 | subj_id),
  family = binomial(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

h11 <- brm(
  data = extra_binom,
  k|trials(n) ~ blocked,
  family = binomial(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

h12 <- brm(
  data = extra_binom,
  k|trials(n) ~ rules,
  family = binomial(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

h13 <- brm(
  data = extra_binom,
  k|trials(n) ~ rules * blocked,
  family = binomial(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

#### Bayes Factors ----
bayesfactor_models(h1_binom_null, h1_binom_rules, 
                   h1_binom_blocked, h1_binom_both)



#### Extrapolators ----
expo_null <- brm(
  data = extra_binom,
  exab4 ~ 1 + (1 | subj_id),
  family = bernoulli(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)
)

expo_rules <- brm(
  data = extra_binom,
  exab4 ~ rules + (1 | subj_id),
  family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)
)

expo_blocked <- brm(
  data = extra_binom,
  exab4 ~ blocked + (1 | subj_id),
  family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)
)

expo_both <- brm(
  data = extra_binom,
  exab4 ~ rules * blocked + (1 | subj_id),
  family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)
)


#### Model Diagnostics ----
rstan::check_divergences(expo_null$fit)
rstan::check_divergences(expo_rules$fit)
rstan::check_divergences(expo_blocked$fit)
rstan::check_divergences(expo_both$fit)

#### BFs ----
#### Bayes Factors ----
bayesfactor_models(expo_null, expo_rules, 
                   expo_blocked, expo_both)


#### Going (A)NOVA! ----
library(afex)
av_h0  <- aov_4(p ~ 1 + (1|subj_id), data = extra_binom, factorize = FALSE) %>% print()
av_h11 <- aov_4(p ~ blocked, data = extra_binom) %>% print()
av_h12 <- aov_4(p ~ rules, data = extra_binom) %>% print()
# av_h12 <- aov_4(p ~ rules * blocked + (1|subj_id), data = extra_binom) %>% print()
aov_inter <- aov_4(p ~ rules * blocked + Error(1|subj_id), data = extra_binom) %>% print()
av_h132 <- aov_4(exab6 ~ rules * blocked, data = extra_binom) %>% print()
# none of the above work for som reason, but this does:
aov_ez("subj_id", "p", extra_binom, between = c("rules", "blocked"))


library(BayesFactor)
aov_fctr <- extra_binom %>% 
  mutate(
    rules   = as_factor(rules),
    blocked = as_factor(blocked)
  )
aov_bf <- BayesFactor::anovaBF(p ~ rules * blocked, aov_fctr, whichRandom = "subj_id",
                               iterations = 40000, whichModels = "all")



#### lme4, GLM, etc.
library(lme4)

m0 <- glmer(p ~ 1 + (1 | subj_id), data = extra_binom,  
            weights = n, family = binomial)
m1 <- glmer(p ~ blocked + (1 | subj_id), data = extra_binom,  
            weights = n, family = binomial)
m2 <- glmer(p ~ rules + (1 | subj_id), data = extra_binom,  
            weights = n, family = binomial)
m3 <- glmer(p ~ blocked + rules + (1 | subj_id), data = extra_binom,  
            weights = n, family = binomial)
m4 <- glmer(p ~ blocked * rules + (1 | subj_id), data = extra_binom,  
            weights = n, family = binomial)

m5 <- glmer(extrapolation ~ blocked * rules + (1 | subj_id), data = extra_berni,  
            weights = rep(9, nrow(extra_berni)), family = binomial)

anova(m0, m1, m2, m3, m4)


#### Response Times ----
rt_quant <- as.numeric(quantile(transfer$response_time, probs = c(.01, .99)))

rtdist <- age %>% 
  # filter(item == "transfer") %>% 
  mutate(
    dist_x = abs(4 - img_x),
    dist_y = abs(4 - img_y)
  ) %>% 
  filter(between(response_time, rt_quant[1], rt_quant[2]))

fit1 <- lm(log(response_time) ~ dist_x * dist_y * age, data = rtdist) # 4tehlolz

fit2 <- brm(response_time ~ age * dist_x * dist_y + (1|subj_id), 
            data = rtdist, prior = prior_effect,
            cores = ncore, iter = 12000, warmup = 2000,
            control = list(adapt_delta = 0.8),
            save_pars = save_pars(all = TRUE))



#### C&K
ckab6_null <- brm(
  data = extra_binom,
  exab6 ~ 1,
  family = bernoulli(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.8),
  save_pars = save_pars(all = TRUE)
)

ckab6_blocked <- brm(
  data = extra_binom,
  exab6~ blocked,
  family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.8),
  save_pars = save_pars(all = TRUE)
)





#### lme4 vs. brms: comparing percentages to successes
m1_lme4_p <- glmer(p ~ 1 + (1 | subj_id), data = extra_binom,  
                   weights = n, family = binomial)

m1_lme4_k <- glmer(extrapolation ~ 1 + (1 | subj_id), 
                   data = extra_berni, family = binomial)
# -> virtually identical results, hooray!

m2_brms_k_default <- brm(extrapolation ~ 1 + (1 | subj_id), 
                         data = extra_berni, family = bernoulli(), # !!
                         cores = ncore, iter = 12000, warmup = 2000,
                         control = list(adapt_delta = 0.9),
                         save_pars = save_pars(all = TRUE))

priors_gelman <- c(
  set_prior("student_t(3, 0, 1)", class = "Intercept", lb = -10, ub = 10),
  set_prior("student_t(3, 0, 1)", class = "sd", lb = 0)
)
priors_lme4 <- c(
  set_prior("student_t(3, -7.64, .74)", class = "Intercept", lb = -18, ub = 2),
  set_prior("student_t(3,  7.29, 1)", class = "sd", lb = 0)
)
priors_brms <- c(
  set_prior("student_t(3, -4.67, .53)", class = "Intercept", lb = -15, ub = 15),
  set_prior("student_t(3,  4.32, .5)", class = "sd", lb = 0)
)

m2_brms_k_gelman <- update(m2_brms_k_default, prior = priors_gelman, cores = ncore)
m2_brms_k_lme4   <- update(m2_brms_k_default, prior = priors_lme4, cores = ncore)
m2_brms_k_brms   <- update(m2_brms_k_default, prior = priors_brms, cores = ncore)

l1 <- loo(m2_brms_k_default, save_psis = TRUE)
l2 <- loo(m2_brms_k_gelman, save_psis = TRUE) # , criterion = c("loo", "waic", "marglik"), moment_match = TRUE)
l3 <- loo(m2_brms_k_lme4, save_psis = TRUE) # , criterion = c("loo", "waic", "marglik"), moment_match = TRUE)
l4 <- loo(m2_brms_k_brms, save_psis = TRUE) # , criterion = c("loo", "waic", "marglik"), moment_match = TRUE)


loo_compare(l1, l2, l3, l4)


b1 <- bridge_sampler(m2_brms_k_default)
b2 <- bridge_sampler(m2_brms_k_gelman)
b3 <- bridge_sampler(m2_brms_k_lme4)
b4 <- bridge_sampler(m2_brms_k_brms)

bayestestR::bayesfactor_models(m2_brms_k_default, m2_brms_k_gelman, 
                               m2_brms_k_lme4, m2_brms_k_brms)

# ok, so, m2_brms_k_brms seems best, but how about without prior-bounds?
m2_brms_k_brms_unbounded <- update(m2_brms_k_default, prior = c(
  set_prior("student_t(3, -4.67, .53)", class = "Intercept"),
  set_prior("student_t(3,  4.32, .5)", class = "sd", lb = 0)
), cores = ncore)

l5 <- loo(m2_brms_k_brms_unbounded, save_psis = TRUE)
loo_compare(l4, l5)
b5 <- bridge_sampler(m2_brms_k_brms_unbounded)
bayes_factor(m2_brms_k_brms_unbounded, m2_brms_k_brms)
