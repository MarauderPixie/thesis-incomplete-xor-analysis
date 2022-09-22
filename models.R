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

#### Models ----
h21 <- brm(
  data = training,
  correct ~ blocked * block + (1 + image | subj_id),
  family = bernoulli(), prior = priors5,
  cores = ncore, iter = 4000, warmup = 1000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

h22 <- brm(
  data = training,
  correct ~ rules * num_block + (1 + image | subj) + (1 + subj | image),
  family = bernoulli(), # prior = priors5,
  cores = ncore, iter = 10000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)