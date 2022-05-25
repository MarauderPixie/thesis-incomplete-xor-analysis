library(tidyverse)
library(brms)

theme_set(hrbrthemes::theme_ipsum_ps())

#### condition-assignment: ----
# A: no rll, no ordering
# B: no rr, simple rule first
# C: rll, no ordering
# D: rll, srf

### notes on hypotheses:
# We're interested in comparisons of
# A <-> B
# A <-> C
# D <-> A, B, C
# amirite?


#### test phase data ----
# the data used here was collected in several developmental stages of the experiment;
# final results will look very similar in structure such that this example should
# completely suffice to serve as an example
dummy <- read_csv("data-raw/messy-dummy-data.csv") %>% 
  mutate(
    assignment = case_when(assignment == "1" ~ "correct1",
                           assignment == "2" ~ "correct2",
                           TRUE ~assignment)
  )

# incomplete category images:
utt <- paste0(
  "images/", c("e45_4000-366.7.jpg", "e44_4000-233.3.jpg", "e43_4000-100.jpg",
               "e38_3583-366.7.jpg", "e37_3583-233.3.jpg", "e36_3583-100.jpg",
               "e31_3167-366.7.jpg", "e30_3167-233.3.jpg", "e29_3167-100.jpg")
)

# data prep
train <- dummy %>% 
  filter(!is.na(correct1)) %>% 
  mutate(
    subj = as_factor(submission_id),
    condition = as_factor(condition),
    block = rep(rep(1:12, each = 8), 4) %>% as_factor(),
    image = as_factor(image),
    correct = ifelse(assignment == "correct1", correct1, correct2),
    score = ifelse(correct == response, 1, 0)
  ) %>% 
  select(-correct1, -correct2, -education, -gender, -submission_id)

aggr <- train %>% 
  group_by(subj, condition, block, image) %>% 
  summarise(
    acc = mean(response == correct)
  )

aggr_binom <- train %>% 
  group_by(subj, condition, block) %>% 
  summarise(
    k = sum(response == correct),
    n = n()
  )


general <- dummy %>% 
  filter(image %in% utt) %>% 
  mutate(
    subj = as_factor(submission_id),
    condition = as_factor(condition),
    exin = case_when(response == "A" & assignment == "correct1" ~ 0,
                     response == "B" & assignment == "correct2" ~ 0,
                     TRUE ~ 1) # needs to be validated very very much!!
  ) %>% 
  select(subj, condition, exin)

general_binom <- general %>% 
  group_by(subj, condition) %>% 
  summarise(
    k = sum(exin),
    n = n()
  )




# final models, approximately:
model_training <- brm(data = aggr, acc ~ condition * block + (1|subj + block))
model_general  <- brm(data = general, exin ~ condition + (1|subj), family = bernoulli)

alt_model_training <- brm(data = aggr_binom, 
                          k|trials(n) ~ condition * block + (1|subj + block), 
                          family = binomial)
alt_model_general  <- brm(data = general_binom, 
                          k|trials(n) ~ condition + (1|subj), 
                          family = binomial)



## model playground
alt_model_training <- brm(data = train, 
                          score ~ condition * block + (1|subj + image), 
                          family = bernoulli)

#### "simulate" some data ----
subj  <- paste0("S0", str_pad(1:100, 2, pad = "0")) %>% as_factor()
cond  <- rep(LETTERS[1:4], each = 25) %>% as_factor()
block <- as_factor(1:12)
imgs1 <- rep(paste0("img", 1:4), 2)
imgs2 <- c(paste0("img", 1:6), imgs1[1:2])
# gimgs <- paste0("img", str_pad(1:49, 2, pad = "0"))
gen_state <- c(rep("critical", 9), rep("midlane", 13), rep("regular", 27))

simdat <- tibble(
  subj = rep(subj, each = 96),
  condition = rep(cond, each = 96),
  block = rep(block, each = 8, times = 100),
  image = c(
    replicate(25*12, sample(imgs2)) %>% as.vector(),
    replicate(25, c(replicate(2, sample(imgs1)), 
                    replicate(10, sample(imgs2)))) %>% as.vector(),
    replicate(25*12, sample(imgs2)) %>% as.vector(),
    replicate(25, c(replicate(2, sample(imgs1)),
                    replicate(10, sample(imgs2)))) %>% as.vector()
    ) %>% factor(),
  correct = c(
    map(rep(pmin(1:12/12, .6), 25), ~rbernoulli(n = 8, p = .x)) %>% flatten_int(),
    map(rep(pmin(1:12/12, .725), 25), ~rbernoulli(n = 8, p = .x)) %>% flatten_int(),
    map(rep(pmin(1:12/12, .85), 25), ~rbernoulli(n = 8, p = .x)) %>% flatten_int(),
    map(rep(pmin(1:12/12, .975), 25), ~rbernoulli(n = 8, p = .x)) %>% flatten_int()
  )
)

gensim <- tibble(
  subj = rep(subj, each = 49),
  condition = rep(cond, each = 49),
  image = replicate(100, sample(gen_state, 49)) %>% as.vector(),
  response = c( # taking it easy...
    replicate(25, rbernoulli(49, .3)) %>% as.integer(),
    replicate(25, rbernoulli(49, .5)) %>% as.integer(),
    replicate(25, rbernoulli(49, .7)) %>% as.integer(),
    replicate(25, rbernoulli(49, .9)) %>% as.integer()
  )
)
rm(subj, cond, block, imgs1, imgs2, gen_state)

saveRDS(simdat, "data-raw/simulated_training.rds")
saveRDS(gensim, "data-raw/simulated_generalization.rds")


#### model the simulated data ----
# What are my random effects?
# images occur repeatedly, as well as subjects
# every subject goes through every block once tho
# same for every condition
#
# I'm interested in the differences between blocks and conditions
# that is: not only are blocks 1 & 2 'training blocks' in any case
# they also differ in composition
# now...
# SM suggests comparing two models, one of conditions 1&3 and one of 2&4, amirite?
full_dat <- simdat %>% filter(as.numeric(block) > 2)
rll_dat  <- full_dat %>% filter(condition %in% c("A", "C"))
srf_dat  <- full_dat %>% filter(condition %in% c("A", "B"))


full_mod <- brm(data = full_dat,  
                correct ~ condition * block + (1|subj + image), 
                family = bernoulli, iter = 4000, # 2000 -> low ESS, 4k seem fine
                save_pars = save_pars(all = TRUE))

### old fiddling around with RE structure...
# sim_mod2 <- brm(data = simdat, 
#                 correct ~ condition + (1|subj + image + block), 
#                 family = bernoulli, iter = 4000,
#                 save_pars = save_pars(all = TRUE))
# 
# saveRDS(sim_mod2, "models/simulation02.rds")

rll_mod <- brm(data = rll_dat,  
               correct ~ condition * block + (1|subj + image), 
               family = bernoulli, 
               save_pars = save_pars(all = TRUE))

srf_mod <- brm(data = srf_dat,  
               correct ~ condition * block + (1|subj + image), 
               family = bernoulli, control = list( adapt_delta = .9),
               save_pars = save_pars(all = TRUE))

saveRDS(full_mod, "models/simulation_full.rds")
saveRDS(rll_mod, "models/simulation_rll.rds")
saveRDS(srf_mod, "models/simulation_srf.rds")


## further analysis
full_mod <- readRDS("models/simulation_full.rds")
rll_mod  <- readRDS("models/simulation_rll.rds")
srf_mod  <- readRDS("models/simulation_srf.rds")

loo1 <- loo::loo(rll_mod, save_psis = TRUE)
loo2 <- loo::loo(srf_mod, save_psis = TRUE)


yrep1 <- posterior_predict(rll_mod)
yrep2 <- posterior_predict(srf_mod)

bayesplot::ppc_loo_pit_overlay(rll_dat$correct, yrep1, lw = weights(loo1$psis_object))
bayesplot::ppc_loo_pit_overlay(srf_dat$correct, yrep2, lw = weights(loo2$psis_object))


loo::loo_compare(loo1, loo2)

bayes_factor(rll_mod, srf_mod)


# altmods & playing around with... ----

### different RE structures
# only allow for variation in subjects
rll_mod2 <- brm(data = rll_dat,  
                correct ~ condition * block + (1 | subj), 
                family = bernoulli, 
                save_pars = save_pars(all = TRUE))

# assume diff subjects react moreor less to condition; || means 'no correlation bw REs'
# that is:
# intercept and slope for `condition` varies with values for `subj`
rll_mod3 <- brm(data = rll_dat,  
                correct ~ condition * block + (1 + condition || subj), 
                family = bernoulli, 
                save_pars = save_pars(all = TRUE))

# maybeb slope for 'images' varies with 'subj'
rll_mod4 <- brm(data = rll_dat,  
                correct ~ condition * block + (1 + image || subj), 
                family = bernoulli, 
                save_pars = save_pars(all = TRUE))

# yet another assumption might be that the trend over blocks is allowed to vary by subject
# and/or by block AND condition, and/or by block AND condition AND image...
rll_mod5 <- brm(data = rll_dat,  
                correct ~ condition * block + (1 + block || subj), 
                family = bernoulli, 
                save_pars = save_pars(all = TRUE))

saveRDS(rll_mod5, "models/rll_mod5.rds")

# TODO: it's probably a good idea to compare bern to binom models, 
# or models where 'block' is a numeric predictor...?
#
# also: so far, only model 5 seems to be any (reasonably) better
rll_mod6 <- brm(data = rll_dat,  
                correct ~ condition * block + (1 + block + image || subj), 
                family = bernoulli, 
                save_pars = save_pars(all = TRUE))

rll_mod7 <- brm(data = mutate(rll_dat, block = as.numeric(block)),
                correct ~ condition * block + (1 + block || subj), 
                family = bernoulli, cores = 2,
                save_pars = save_pars(all = TRUE))

# all_mods <- loo(rll_mod, rll_mod2, rll_mod3, rll_mod4, rll_mod5)


#### let's try binomial models? ----
aggr <- full_dat %>% 
  group_by(condition, subj, block) %>% 
  summarise(
    correct = sum(correct),
    total   = n(),
    perc    = correct / total
  ) %>% 
  ungroup()

rll_aggr <- aggr %>% filter(condition %in% c("A", "C"))
rll_mod9 <- brm(data = rll_aggr,  
                correct|trials(total) ~ condition * block + (1 + block || subj), 
                family = binomial, cores = 4,
                save_pars = save_pars(all = TRUE))

srf_aggr <- aggr %>% filter(condition %in% c("A", "B"))
srf_mod9 <- brm(data = srf_aggr,  
                correct|trials(total) ~ condition * block + (1 + block || subj), 
                family = binomial, cores = 4,
                save_pars = save_pars(all = TRUE))


#### models for data of generalization phase ----
crits <- gensim %>% filter(image == "critical")

gen_mod1 <- brm(data = crits,  
                response ~ condition + (1 | subj), 
                family = bernoulli, cores = 4,
                control = list( adapt_delta = .85),
                save_pars = save_pars(all = TRUE))

gen_mod2 <- brm(data = filter(crits, condition %in% c("A", "C")),
                response ~ condition + (1 | subj), 
                family = bernoulli, cores = 4,
                # control = list( adapt_delta = .85),
                save_pars = save_pars(all = TRUE))

gen_mod3 <- brm(data = filter(crits, condition %in% c("A", "B")),
                response ~ condition + (1 | subj), 
                family = bernoulli, cores = 4,
                # control = list( adapt_delta = .85),
                save_pars = save_pars(all = TRUE))

craggr <- crits %>% 
  group_by(condition, subj) %>% 
  summarise(
    extra = sum(response),
    total = n(),
    p     = extra / total
  ) %>% ungroup()

gen_mod4 <- brm(data = craggr,  
                extra|trials(total) ~ condition + (1 | subj), 
                family = binomial, cores = 4,
                control = list( adapt_delta = .85),
                save_pars = save_pars(all = TRUE))

gen_mod5 <- brm(data = filter(craggr, condition %in% c("A", "C")),
                extra|trials(total) ~ condition + (1 | subj), 
                family = binomial, cores = 4,
                # control = list( adapt_delta = .85),
                save_pars = save_pars(all = TRUE))

gen_mod6 <- brm(data = filter(craggr, condition %in% c("A", "B")),
                extra|trials(total) ~ condition + (1 | subj), 
                family = binomial, cores = 4,
                # control = list( adapt_delta = .85),
                save_pars = save_pars(all = TRUE))


loo1 <- loo(gen_mod1, save_psis = TRUE)
loo4 <- loo(gen_mod4, save_psis = TRUE)

yrep1 <- posterior_predict(gen_mod1)

library(bayesplot)
ppc_loo_pit_overlay(y = crits$response, 
                    yrep = yrep1,
                    lw = weights(loo1$psis_object))






## as.factor(block) vs. as.numeric(block)
full_dat <- simdat %>% filter(as.numeric(block) > 2) %>% 
  group_by(subj, condition, block) %>% 
  summarise(
    k = sum(correct),
    n = n(),
    p = k / n
  ) %>% 
  ungroup()


mod_num <- brm(data = mutate(full_dat, block = as.numeric(block)),
               k|trials(n) ~ condition + block + (block | subj), 
               family = binomial, cores = 4,
               # control = list( adapt_delta = .85),
               save_pars = save_pars(all = TRUE))

mod_fct <- brm(data = full_dat,
               k|trials(n) ~ condition + block + (block | subj), 
               family = binomial, cores = 4,
               # control = list( adapt_delta = .85),
               save_pars = save_pars(all = TRUE))
