library(tidyverse)
library(brms)

theme_set(hrbrthemes::theme_ipsum_tw())

### condition-assignment:
# 1: no rll, no ordering
# 2: no rr, simple rule first
# 3: rll, no ordering
# 4: rll, srf


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

# "simulate" some data
subj  <- paste0("S0", str_pad(1:100, 2, pad = "0")) %>% as_factor()
cond  <- rep(LETTERS[1:4], each = 25) %>% as_factor()
block <- as_factor(1:12)
imgs  <- paste0("img", 1:8)

simdat <- tibble(
  subj = rep(subj, each = 96),
  condition = rep(cond, each = 96),
  block = rep(rep(block, each = 8), times = 100),
  image = rep(map(1:12, function(i){sample(imgs, 8)}) %>% flatten_chr() %>% factor(levels = imgs), 100),
  correct = c(
    lapply(1:25, function(i){map(seq(12), ~rbernoulli(n = 8, p = min(.x/12, .6))) %>% flatten_int()}) %>% flatten_int(),
    lapply(1:25, function(i){map(seq(12), ~rbernoulli(n = 8, p = min(.x/12, .725))) %>% flatten_int()}) %>% flatten_int(),
    lapply(1:25, function(i){map(seq(12), ~rbernoulli(n = 8, p = min(.x/12, .85))) %>% flatten_int()}) %>% flatten_int(),
    lapply(1:25, function(i){map(seq(12), ~rbernoulli(n = 8, p = min(.x/12, .975))) %>% flatten_int()}) %>% flatten_int()
  ),
  ylog = log(correct)
)
rm(subj, cond, block, imgs)

sim_mod1 <- brm(data = simdat, 
                correct ~ condition * block + (1|subj + image), 
                family = bernoulli, iter = 4000, # 2000 -> low ESS, 4k seem fine
                save_pars = save_pars(all = TRUE))

saveRDS(sim_mod1, "models/simulation01.rds") 

sim_mod2 <- brm(data = simdat, 
                correct ~ condition + (1|subj + image + block), 
                family = bernoulli, iter = 4000,
                save_pars = save_pars(all = TRUE))

saveRDS(sim_mod2, "models/simulation02.rds")

## further analysis
sim_mod1 <- readRDS("models/simulation01.rds")
sim_mod2 <- readRDS("models/simulation02.rds")

loo1 <- loo::loo(sim_mod1, save_psis = TRUE)
loo2 <- loo::loo(sim_mod2, save_psis = TRUE)


yrep1 <- posterior_predict(sim_mod1)
yrep2 <- posterior_predict(sim_mod2)

ppc_loo_pit_overlay(simdat$correct, yrep1, lw = weights(loo1$psis_object))
ppc_loo_pit_overlay(simdat$correct, yrep2, lw = weights(loo2$psis_object))


loo::loo_compare(loo1, loo2)


# altmods & playing around
aggr <- simdat %>% 
  group_by(condition, subj, block) %>% 
  summarise(
    correct = sum(correct),
    total   = n(),
    p_corr  = correct / total
  ) %>% 
  ungroup()

sim_mod_alt1 <- brm(data = aggr, 
                    correct|trials(total) ~ condition * block + (1|subj), 
                    family = binomial, iter = 4000, # 2000 -> low ESS, 4k seem fine
                    save_pars = save_pars(all = TRUE))

saveRDS(sim_mod_alt1, "models/simulation03.rds") 


## trial responses for 1 subject
# 12x8 trials
# p -> steigt bis min(.x/12, 1) an und simuliert steigende acc pro block
# wenn y in min(.x/12, y) <1 gesetzt wird, sinkt sowohl max_acc als auch die rate, in der acc ansteigt
map(seq(12), ~rbernoulli(n = 8, p = min(.x/12, 1))) %>% flatten_dbl()
