#### Data Preparation ----
# With aggregation to binomial and different cutoffs
# as to when someone counts as "extrapolator"
extra_binom <- extra_all %>% 
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
prior_null <- c(
  set_prior("student_t(3, 0, 2.5)", class = "Intercept"), 
  set_prior("student_t(3, 0, 2.5)", class = "sd")
)
prior_effect <- c(
  set_prior("student_t(3, 0, 2.5)", class = "Intercept"), 
  set_prior("student_t(3, 0, 2.5)", class = "sd"), 
  set_prior("student_t(3, 0, 2.5)", class = "b")
)

#### Binomial models (instead of bernoulli) ----
h1_binom_null <- brm(
  data = extra_binom,
  k|trials(n) ~ 1 + (1 | subj_id) ,
  family = binomial(), prior = prior_null,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

h1_binom_rules <- brm(
  data = extra_binom,
  k|trials(n) ~ rules + (1 | subj_id) ,
  family = binomial(), prior = prior_effect,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

h1_binom_blocked <- brm(
  data = extra_binom,
  k|trials(n) ~ blocked + (1 | subj_id) ,
  family = binomial(), prior = prior_effect,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

h1_binom_both <- brm(
  data = extra_binom,
  k|trials(n) ~ rules * blocked + (1 | subj_id) ,
  family = binomial(), prior = prior_effect,
  cores = ncore, iter = 20000, warmup = 4000,
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
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)
)

expo_rules <- brm(
  data = extra_binom,
  exab4 ~ rules + (1 | subj_id),
  family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)
)

expo_blocked <- brm(
  data = extra_binom,
  exab4 ~ blocked + (1 | subj_id),
  family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)
)

expo_both <- brm(
  data = extra_binom,
  exab4 ~ rules * blocked + (1 | subj_id),
  family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 20000, warmup = 4000,
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
fit1 <- aov_4(k ~ rules * blocked + (1|subj_id), data = extra_binom) %>% print()
fit2 <- aov_4(p ~ rules * blocked + (1|subj_id), data = extra_binom) %>% print()


#### Response Times ----
lort <- as.numeric(quantile(transfer$response_time, probs = c(.01, .99)))[1]
hirt <- as.numeric(quantile(transfer$response_time, probs = c(.01, .99)))[2]

rtdist <- transfer %>% 
  # filter(item == "transfer") %>% 
  mutate(
    dist_x = abs(4 - img_x),
    dist_y = abs(4 - img_y)
  ) %>% 
  filter(between(response_time, lort, hirt))

fit1 <- lm(response_time ~ dist_x * dist_y, data = rtdist)

fit2 <- brm(response_time ~ rules * blocked * dist_x * dist_y + (1|subj_id), 
            data = rtdist, prior = prior_effect,
            cores = ncore, iter = 12000, warmup = 2000,
            control = list(adapt_delta = 0.8),
            save_pars = save_pars(all = TRUE))
