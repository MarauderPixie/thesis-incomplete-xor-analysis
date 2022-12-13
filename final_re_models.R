source("init.R")

#### Data ----
## transfer
extra_berni <- readRDS("data-clean/trials-transfer.rds") %>% 
  filter(item == "transfer")

## training
train_rules <- readRDS("data-clean/trials-training.rds") %>%  
  filter(condition %in% c("control", "rules")) %>% 
  group_by(rules, subj_id, block) %>% 
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
# 30% of subjs having at least 6/9 extrials
prior_null   <- c(
  set_prior("student_t(3, -1.4, 1)", class = "Intercept"),
  set_prior("student_t(3,  2.2, 1)", class = "sd", lb = 0, ub = 5)
)
prior_effect <- c(
  set_prior("student_t(3, -1.4, 1)", class = "Intercept"), 
  set_prior("student_t(3,  2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(3,   .5, 1)", class = "b")
)

#### H1 Models ----
h1_re_null <- brm(
  extrapolation ~ 1 + (1 | subj_id),
  data = extra_berni, family = bernoulli(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.92),
  save_pars = save_pars(all = TRUE)
)
saveRDS(h1_re_null,    "models/h1_transfer_re/h10.rds")

h1_re_blocked <- update(h1_re_null, extrapolation ~ blocked + (1 | subj_id), 
                        newdata = extra_berni, prior = prior_effect, 
                        cores = ncore)
saveRDS(h1_re_blocked, "models/h1_transfer_re/h11.rds")

h1_re_rules   <- update(h1_re_null, extrapolation ~ rules + (1 | subj_id), 
                        newdata = extra_berni, prior = prior_effect, 
                        cores = ncore)
saveRDS(h1_re_rules,   "models/h1_transfer_re/h12.rds")

h1_re_both    <- update(h1_re_null, extrapolation ~ blocked + rules + (1 | subj_id), 
                        newdata = extra_berni, prior = prior_effect, 
                        cores = ncore)
saveRDS(h1_re_both,    "models/h1_transfer_re/h131.rds")

h1_re_inter   <- update(h1_re_null, extrapolation ~ blocked * rules + (1 | subj_id), 
                        newdata = extra_berni, prior = prior_effect, 
                        cores = ncore)
saveRDS(h1_re_inter,   "models/h1_transfer_re/h132.rds")


#### H2 Models ----
# I could go ahead and overengineer it with brms,
# but certainly a simple t-test will do for 2.1 
# h21_ttestBF <- BayesFactor::ttestBF(formula = p ~ rules, data = subj_rules)

## nonetheless...
p2_null   <- c(
  set_prior("student_t(3, 2, 1)", class = "Intercept"),
  set_prior("student_t(3, 2.2, 1)", class = "sd", lb = 0, ub = 5)
)
p2_effect <- c(
  set_prior("student_t(3, 2, 1)", class = "Intercept"), 
  set_prior("student_t(3, 2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(3, 0, 1)", class = "b")
)


h21_null <- brm(
  k|trials(n) ~ 1 + (block || subj_id),
  data = train_rules,
  family = binomial(), prior = p2_null,
  cores = ncore, iter = 12000, warmup = 2000,
  save_pars = save_pars(all = TRUE)
)
saveRDS(h21_null, "models/h2_training/h21_null.rds")

h21_rules <- brm(
  k|trials(n) ~ rules + (block || subj_id),
  data = train_rules,
  family = binomial(), prior = p2_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  save_pars = save_pars(all = TRUE)
)
saveRDS(h21_rules, "models/h2_training/h21_rules.rds")


## 2.2 Interaction of Rule & Training Block
# h22_null <- brm(
#   k|trials(n) ~ 1 + (block|subj_id),
#   data = blocks_rules, 
#   family = binomial(), prior = prior_null,
#   cores = ncore, iter = 12000, warmup = 2000,
#   save_pars = save_pars(all = TRUE)
# )
# 
# h22_rule  <- update(h22_null, k|trials(n) ~ rules + (block|subj_id),
#                     data = blocks_rules, prior = prior_effect, cores = ncore)
# h22_both  <- update(h22_null, k|trials(n) ~ rules + block + (block|subj_id),
#                     data = blocks_rules, prior = prior_effect, cores = ncore)
# h22_inter <- update(h22_null, k|trials(n) ~ rules * block + (block|subj_id),
#                     data = blocks_rules, prior = prior_effect, cores = ncore)
# 
# saveRDS(h22_null,  "models/h2_training/h22_null.rds")
# saveRDS(h22_rule,  "models/h2_training/h22_rule.rds")
# saveRDS(h22_both,  "models/h2_training/h22_both.rds")
# saveRDS(h22_inter, "models/h2_training/h22_interaction.rds")

rm(extra_berni, h1_re_blocked, h1_re_both, h1_re_inter, h1_re_null, prior_effect,
   h1_re_rules, h21_null, h21_rules, p2_null, p2_effect, prior_null, train_rules)
