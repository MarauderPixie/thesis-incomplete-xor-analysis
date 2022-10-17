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
incomplete_items <- filter(transfer, item == "transfer")

null_model <- brm(
  data = incomplete_items,
  extrapolation ~ 1 + (rules * condition | subj_id),
  family = bernoulli(), 
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)

saveRDS(null_model, "models/h1_null_full.rds")

## full-ish model
h1_rules <- brm(
  data = incomplete_items,
  extrapolation ~ rules + (rules * condition | subj_id),
  family = bernoulli(), prior = prereg_prior,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)

saveRDS(h1_rules, "models/h1_rules_full.rds")


h1_blocked <- brm(
  data = incomplete_items,
  extrapolation ~ blocked + (blocked * condition | subj_id),
  family = bernoulli(), prior = prereg_prior,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)

saveRDS(h1_blocked, "models/h1_blocked_full.rds")

h1_both <- brm(
  data = incomplete_items,
  extrapolation ~ rules * blocked + (rules * blocked | subj_id),
  family = bernoulli(), prior = pr_prior,
  cores = ncore, iter = 20000, warmup = 4000,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE)
)

saveRDS(h1_both, "models/h1_both_full.rds")
