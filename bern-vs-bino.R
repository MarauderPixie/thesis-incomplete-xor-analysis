source("init.R")

## Data ----
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


## Priors ----
prior_null <- c(
  set_prior("student_t(5, 0, 1)", class = "Intercept"),
  set_prior("student_t(5, 0, 1)", class = "sd", lb = 0)
)
prior_effect <- c(
  set_prior("student_t(5, 0, 1)", class = "Intercept"),
  set_prior("student_t(5, 0, 1)", class = "sd", lb = 0), 
  set_prior("student_t(5, 0, 1)", class = "b")
)

#### Data Comparison ----
## Bernoulli models ----
bern_fit0 <- brm(
  data = extra_berni,
  extrapolation ~ 1 + (1 | subj_id),
  family = bernoulli(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

bern_fit1 <- update(bern_fit0, extrapolation ~ blocked + (1 | subj_id), 
                    newdata = extra_berni, prior = prior_effect, cores = ncore)
bern_fit2 <- update(bern_fit0, extrapolation ~ rules + (1 | subj_id), 
                    newdata = extra_berni, prior = prior_effect, cores = ncore)
bern_fit3 <- update(bern_fit0, extrapolation ~ rules + blocked + (1 | subj_id), 
                    newdata = extra_berni, prior = prior_effect, cores = ncore)
bern_fit4 <- update(bern_fit0, extrapolation ~ rules * blocked + (1 | subj_id), 
                    newdata = extra_berni, prior = prior_effect, cores = ncore)

## Binomial models ----
binom_fit0 <- brm(
  data = extra_binom,
  k|trials(n) ~ 1 + (1 | subj_id),
  family = binomial(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

binom_fit1 <- update(binom_fit0, 
                     k|trials(n) ~ blocked + (1 | subj_id), 
                     newdata = extra_binom, prior = prior_effect, cores = ncore)
binom_fit2 <- update(binom_fit0, 
                     k|trials(n) ~ rules + (1 | subj_id), 
                     newdata = extra_binom, prior = prior_effect, cores = ncore)
binom_fit3 <- update(binom_fit0, 
                     k|trials(n) ~ rules + blocked + (1 | subj_id), 
                     newdata = extra_binom, prior = prior_effect, cores = ncore)
binom_fit4 <- update(binom_fit0, 
                     k|trials(n) ~ rules * blocked + (1 | subj_id), 
                     newdata = extra_binom, prior = prior_effect, cores = ncore)

## Comparison & Conclusion ----
bern_fit4
binom_fit4
# outputs look virtually identical for all models, 
# therefore the (leaner) binomial models are preferred
rm(bern_fit0, bern_fit1, bern_fit2, bern_fit3, bern_fit4)
