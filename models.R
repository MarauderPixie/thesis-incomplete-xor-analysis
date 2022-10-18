source("init.R")

#### Read & prepare data ----
# demo <- read_csv("data-clean/demographics.csv")
# training <- read_csv("data-clean/trials-training.csv")
# transfer <- read_csv("data-clean/trials-transfer.csv")
demo <- readRDS("data-clean/demographics.rds")
training <- readRDS("data-clean/trials-training.rds")
transfer <- readRDS("data-clean/trials-transfer.rds")
stimprob <- readRDS("data-clean/trials-probability.rds")

exctest <- training %>% 
  filter(block > 9) %>% 
  group_by(subj_id) %>% 
  summarise(
    n = n(),
    k = sum(correct),
    p = k / n
  ) %>% 
  filter(p < .7)

demo <- demo %>% 
  filter(!(subj_id %in% exctest$subj_id))
training <- training %>% 
  filter(!(subj_id %in% exctest$subj_id))
transfer <- transfer %>% 
  filter(!(subj_id %in% exctest$subj_id))
stimprob <- stimprob %>% 
  filter(!(subj_id %in% exctest$subj_id))


#### Priors ----
get_prior(correct ~ blocked * block + (1 + image | subj_id),
          data = training)

priors5 <- c(
  set_prior("normal(-1, 0.32)", class = "Intercept"), 
  set_prior("normal(0, 0.5)", class = "b")
)

prereg_prior <- c(
  set_prior("student_t(5, 0, 2.5)", class = "Intercept"), 
  set_prior("student_t(5, 0, 2.5)", class = "sd"), 
  set_prior("student_t(5, 0, 2.5)", class = "b")
)

#### Models ----
extra_all     <- filter(transfer, item == "transfer")
extra_rules   <- filter(extra_all, condition %in% c("control", "rules"))
extra_blocked <- filter(extra_all, condition %in% c("control", "blocked"))

## H1.1: effect of mentioning rules
# h1_rules_null1 <- brm(
#   data = extra_rules,
#   extrapolation ~ 1 + (rules | subj_id),
#   family = bernoulli(), prior = prereg_prior,
#   cores = ncore, iter = 20000, warmup = 4000,
#   control = list(adapt_delta = 0.99),
#   save_pars = save_pars(all = TRUE)
# )
h1_rules_null2 <- brm(
  data = extra_rules,
  extrapolation ~ 1 + (rules || subj_id),
  family = bernoulli(), prior = prereg_prior,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)

saveRDS(h1_rules_null2, "models/h1_null_nocorr.rds")

## H1.2: Effect of blocked rule
# h1_blocked1 <- brm(
#   data = extra_blocked,
#   extrapolation ~ blocked + (blocked | subj_id),
#   family = bernoulli(), prior = prereg_prior,
#   cores = ncore, iter = 20000, warmup = 4000,
#   control = list(adapt_delta = 0.99),
#   save_pars = save_pars(all = TRUE)
# )
h1_blocked2 <- brm(
  data = extra_blocked,
  extrapolation ~ blocked + (blocked || subj_id),
  family = bernoulli(), prior = prereg_prior,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)

saveRDS(h1_blocked2, "models/h1_blocked_nocorr.rds")




# h1_both <- brm(
#   data = extra_all,
#   extrapolation ~ rules * blocked + (rules * blocked | subj_id),
#   family = bernoulli(), prior = pr_prior,
#   cores = ncore, iter = 20000, warmup = 4000,
#   control = list(adapt_delta = 0.99),
#   save_pars = save_pars(all = TRUE)
# )
# saveRDS(h1_both, "models/h1_both_full.rds")

#### Bayes Factors
bayes_factor(h1_rules2, h1_rules_null2)
