source("init.R")

#### Data ----
## transfer
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

## training
training <- readRDS("data-clean/trials-training.rds")

subj_rules <- training %>% 
  filter(condition %in% c("control", "rules")) %>% 
  group_by(subj_id, rules) %>% 
  summarise(
    k = sum(correct),
    n = n(),
    p = mean(correct),
    .groups = "drop"
  ) %>% 
  mutate(
    rules = as_factor(rules)
  )

blocks_rules <- training %>% 
  filter(condition %in% c("control", "rules")) %>% 
  group_by(subj_id, rules, block) %>% 
  summarise(
    k = sum(correct),
    n = n(),
    p = mean(correct),
    .groups = "drop"
  ) %>% 
  mutate(
    rules = as_factor(rules)
  )


#### Priors ----
prior_null   <- set_prior(
  "student_t(3, -1, 1)", class = "Intercept", lb = -4.6
)
prior_effect <- c(
  set_prior("student_t(3, -1, 1)", class = "Intercept", lb = -4.6), 
  set_prior("student_t(3, .5, 1)", class = "b")
)

#### H1 Models ----
h1_null <- brm(
  data = extra_binom,
  k|trials(n) ~ 1,
  family = binomial(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

h1_blocked <- update(h1_null, k|trials(n) ~ blocked, 
                     newdata = extra_binom, prior = prior_effect, 
                     cores = ncore)
h1_rules   <- update(h1_null, k|trials(n) ~ rules, 
                     newdata = extra_binom, prior = prior_effect, 
                     cores = ncore)
h1_both    <- update(h1_null, k|trials(n) ~ blocked + rules, 
                     newdata = extra_binom, prior = prior_effect, 
                     cores = ncore)
h1_inter   <- update(h1_null, k|trials(n) ~ blocked * rules, 
                     newdata = extra_binom, prior = prior_effect, 
                     cores = ncore)

## store away
saveRDS(h1_null,    "models/h1_transfer_fe/h10.rds")
saveRDS(h1_blocked, "models/h1_transfer_fe/h11.rds")
saveRDS(h1_rules,   "models/h1_transfer_fe/h12.rds")
saveRDS(h1_both,    "models/h1_transfer_fe/h131.rds")
saveRDS(h1_inter,   "models/h1_transfer_fe/h132.rds")


#### C&K Models ----
pck_null   <- c(
  set_prior("student_t(3, -.85, 1)", class = "Intercept", lb = -4.6),
  set_prior("student_t(3,   .3, 1)", class = "sd", lb = 0, ub = 5)
)
pck_effect <- c(
  set_prior("student_t(3, -.85, 1)", class = "Intercept", lb = -4.6), 
  set_prior("student_t(3,   .3, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(3,   .5, 1)", class = "b")
)
## "Extrapolator" ab 5 trials
ck_ab5_null <- brm(
  data = extra_binom,
  exab5 ~ 1,
  family = bernoulli(), prior = pck_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

ck_ab5_blocked <- update(ck_ab5_null, exab5 ~ blocked, 
                         newdata = extra_binom, prior = prior_effect, 
                         cores = ncore)
ck_ab5_rules   <- update(ck_ab5_null, exab5 ~ rules, 
                         newdata = extra_binom, prior = prior_effect, 
                         cores = ncore)
ck_ab5_both    <- update(ck_ab5_null, exab5 ~ blocked + rules, 
                         newdata = extra_binom, prior = pck_effect, 
                         cores = ncore)
ck_ab5_inter   <- update(ck_ab5_null, exab5 ~ blocked * rules, 
                         newdata = extra_binom, prior = prior_effect, 
                         cores = ncore)

saveRDS(ck_ab5_null,    "models/conkurz/ck_exab5_null.rds")
saveRDS(ck_ab5_blocked, "models/conkurz/ck_exab5_blocked.rds")
saveRDS(ck_ab5_rules,   "models/conkurz/ck_exab5_rules.rds")
saveRDS(ck_ab5_both,    "models/conkurz/ck_exab5_both.rds")
saveRDS(ck_ab5_inter,   "models/conkurz/ck_exab5_interaction.rds")

## "Extrapolator" ab 6 trials
ck_ab6_null <- brm(
  data = extra_binom,
  exab6 ~ 1 + (1 | subj_id),
  family = bernoulli(), prior = pck_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

ck_ab6_blocked <- update(ck_ab6_null, exab6 ~ blocked, 
                         newdata = extra_binom, prior = prior_effect, 
                         cores = ncore)
ck_ab6_rules   <- update(ck_ab6_null, exab6 ~ rules, 
                         newdata = extra_binom, prior = prior_effect, 
                         cores = ncore)
ck_ab6_both    <- update(ck_ab6_null, exab6 ~ blocked + rules + (1 | subj_id), 
                         newdata = extra_binom, prior = pck_effect, 
                         cores = ncore)
ck_ab6_inter   <- update(ck_ab6_null, exab6 ~ blocked * rules, 
                         newdata = extra_binom, prior = prior_effect, 
                         cores = ncore)

saveRDS(ck_ab6_null,    "models/conkurz/ck_exab6_null_re.rds")
saveRDS(ck_ab6_blocked, "models/conkurz/ck_exab6_blocked.rds")
saveRDS(ck_ab6_rules,   "models/conkurz/ck_exab6_rules.rds")
saveRDS(ck_ab6_both,    "models/conkurz/ck_exab6_both_re.rds")
saveRDS(ck_ab6_inter,   "models/conkurz/ck_exab6_interaction.rds")


#### H2 Models ----
# I could go ahead and overengineer it with brms,
# but certainly a simple t-test will do for 2.1 
h21_ttestBF <- BayesFactor::ttestBF(formula = p ~ rules, data = subj_rules)

## nonetheless...
h21_null <- brm(
  k|trials(n) ~ 1,
  data = subj_rules,
  family = binomial(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  save_pars = save_pars(all = TRUE)
)

h21_rules <- brm(
  k|trials(n) ~ rules,
  data = subj_rules,
  family = binomial(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  save_pars = save_pars(all = TRUE)
)

loo(h21_null, h21_rules)
bayes_factor(h21_rules, h21_null)


## 2.2 Interaction of Rule & Training Block
h22_null <- brm(
  k|trials(n) ~ 1 + (block|subj_id),
  data = blocks_rules, 
  family = binomial(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  save_pars = save_pars(all = TRUE)
)

h22_rule  <- update(h22_null, k|trials(n) ~ rules + (block|subj_id),
                    data = blocks_rules, prior = prior_effect, cores = ncore)
h22_both  <- update(h22_null, k|trials(n) ~ rules + block + (block|subj_id),
                    data = blocks_rules, prior = prior_effect, cores = ncore)
h22_inter <- update(h22_null, k|trials(n) ~ rules * block + (block|subj_id),
                    data = blocks_rules, prior = prior_effect, cores = ncore)

saveRDS(h22_null,  "models/h2_training/h22_null.rds")
saveRDS(h22_rule,  "models/h2_training/h22_rule.rds")
saveRDS(h22_both,  "models/h2_training/h22_both.rds")
saveRDS(h22_inter, "models/h2_training/h22_interaction.rds")






rm(beobachtet, extra_binom, prior_effect, prior_null)