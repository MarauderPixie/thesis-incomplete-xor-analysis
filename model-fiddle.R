#### Data Preparation ----
# With aggregation to binomial and different cutoffs
# as to when someone counts as "extrapolator"
transfer <- readRDS("data-clean/trials-transfer.rds") %>% 
  filter(item == "transfer")

extra_binom <- transfer %>% 
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
  # set_prior("student_t(3, 0, 1)", class = "sd", lb = 0), 
  set_prior("student_t(3, 0, 1)", class = "b", lb = -10, ub = 10)
)

#### Binomial models (instead of bernoulli) ----
h1_null <- brm(
  data = extra_binom,
  k|trials(n) ~ 1,
  family = binomial(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

h1_null_rnd <- brm(
  data = extra_binom,
  k|trials(n) ~ 1 + (1 | subj_id),
  family = binomial(), prior = prior_null_rnd,
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
av_h0  <- aov_4(k ~ 1 + (1|subj_id), data = extra_binom, factorize = FALSE) %>% print()
av_h11 <- aov_4(k ~ blocked, data = extra_binom) %>% print()
av_h12 <- aov_4(k ~ rules, data = extra_binom) %>% print()
# av_h12 <- aov_4(p ~ rules * blocked + (1|subj_id), data = extra_binom) %>% print()
av_h131 <- aov_4(p ~ rules + blocked, data = extra_binom) %>% print()
av_h132 <- aov_4(k ~ rules * blocked, data = extra_binom) %>% print()
# none of the above work for som reason, but this does:
aov_ez("subj_id", "k", extra_binom, between = c("rules", "blocked"))

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
