library(lme4)
library(afex)


aov_4(p ~ rules + (1|subj_id), data = subj_rules)
glm(p ~ rules, data = subj_rules) %>% summary()
t.test(p ~ rules, subj_rules) %>% effectsize::effectsize()

BayesFactor::anovaBF(p ~ rules, subj_rules, whichRandom = "subj_id", iterations = 40000)
BayesFactor::ttestBF(formula = p ~ rules, data = subj_rules) %>% effectsize::effectsize()

brm_rules0 <- brm(
  k|trials(n) ~ 1, data = subj_rules,
  family = binomial(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  save_pars = save_pars(all = TRUE)
)
brm_rules1 <- brm(
  k|trials(n) ~ rules, data = subj_rules,
  family = binomial(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  save_pars = save_pars(all = TRUE)
)

bayes_factor(brm_rules1, brm_rules0)



aov_4(p ~ blocked + (1|subj_id), data = subj_blocked)
glm(p ~ blocked, data = subj_blocked) %>% summary()
t.test(p ~ blocked, subj_blocked) %>% effectsize::effectsize()

BayesFactor::anovaBF(p ~ blocked, subj_blocked, whichRandom = "subj_id", iterations = 40000)
BayesFactor::ttestBF(formula = p ~ blocked, data = subj_blocked) %>% effectsize::effectsize()

brm_blocked0 <- brm(
  k|trials(n) ~ 1, data = subj_blocked,
  family = binomial(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  save_pars = save_pars(all = TRUE)
)
brm_blocked1 <- brm(
  k|trials(n) ~ blocked, data = subj_blocked,
  family = binomial(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  save_pars = save_pars(all = TRUE)
)

bayes_factor(brm_blocked1, brm_blocked0)




aov_ez(c("subj_id", "block"), "p", blocks_rules, between = "rules")
aov_ez(c("subj_id", "block"), "p", blocks_blocked, between = "blocked")





BayesFactor::anovaBF(p ~ rules * block, blocks_rules, 
                     iterations = 40000, whichRandom = "subj_id")

# aov_4(p ~ rules * block + (1|subj_id + block), data = blocks_rules, observed = NULL)
aov_ez("subj_id", "p", blocks_rules, between = "rules", within = "block")
glm(p ~ rules, data = subj_rules) %>% summary()
t.test(p ~ rules, subj_rules)

glmer(p ~ rules * block + (block|subj_id), data = blocks_rules, family = binomial) %>% summary()



aov_ez("subj_id", "p", blocks_blocked, between = "blocked", within = "block")
glmer(p ~ blocked * block + (1|subj_id), data = blocks_blocked, family = binomial) %>% summary()


#### Interactions? Random Effects? ----
subj_blocked %>% 
  group_by(blocked) %>% 
  summarise(mean_p = mean(p), sd_p = sd(p),
            mean_k = mean(k), sd_k = sd(k))

blocks_blocked %>% 
  group_by(block, blocked) %>% 
  summarise(mean_p = mean(p), sd_p = sd(p),
            mean_k = mean(k), sd_k = sd(k))

# 1st: anova with afex, since that's easy and robust
aov_ez("subj_id", "p", blocks_rules, between = "rules", within = "block")

# 2nd: anovaBF with BayesFactor, since that's just a little step away:
BayesFactor::anovaBF(p ~ rules * block, blocks_rules, whichModels = "withmain",
                     iterations = 40000, whichRandom = c("subj_id"))

BayesFactor::anovaBF(p ~ block, blocks_rules, whichModels = "all",
                     iterations = 40000, whichRandom = c("subj_id", "block"))

BayesFactor::anovaBF(p ~ rules, blocks_rules, whichModels = "all",
                     iterations = 40000, whichRandom = c("subj_id", "block"))


# 3rd: how would you go about doing that in lme4, and more importantly, brms?
glmer(p ~ rules * block + (block|subj_id),
      data = blocks_rules, 
      family = binomial()) %>% summary()



blockrule_null <- brm(
  k|trials(n) ~ 1 + (block|subj_id),
  data = blocks_rules, 
  family = binomial(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  # control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

blockrule_rule <- brm(
  k|trials(n) ~ rules + (block|subj_id),
  data = blocks_rules, 
  family = binomial(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  # control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

blockrule_both <- brm(
  k|trials(n) ~ rules + block + (block|subj_id),
  data = blocks_rules, 
  family = binomial(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  # control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

blockrule_int <- brm(
  k|trials(n) ~ rules * block + (block|subj_id),
  data = blocks_rules, 
  family = binomial(), prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  # control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

# seem fine, better save them
saveRDS(blockrule_null, "models/h2_training/h22_null.rds")
saveRDS(blockrule_rule, "models/h2_training/h22_rule.rds")
saveRDS(blockrule_both, "models/h2_training/h22_both.rds")
saveRDS(blockrule_int,  "models/h2_training/h22_interaction.rds")





#### t-test with brms and such ----
subj_rules %>% 
  group_by(rules) %>% 
  summarise(mean = mean(p), 
            sd = sd(p))

h21_null <- brm(
  p ~ 1,
  data = subj_rules, 
  family = student, prior = prior_p,
  cores = ncore, iter = 12000, warmup = 2000,
  save_pars = save_pars(all = TRUE)
)

h21_rules <- brm(
  p ~ rules,
  data = subj_rules, 
  family = student, prior = prior_effect,
  cores = ncore, iter = 12000, warmup = 2000,
  save_pars = save_pars(all = TRUE)
)

welch <- brm(bf(p ~ rules, sigma ~ rules), 
             data = subj_rules,
             family = student, prior = prior_effect,
             cores = ncore, iter = 12000, warmup = 2000,
             save_pars = save_pars(all = TRUE))

## beta prior bc percentages:
prior_p <- set_prior("beta(11, 2)", class = "Intercept")
prior_p_r <- c(
  set_prior("beta(11, 2)", class = "Intercept"),
  set_prior("student_t(7, 0, 1)", class = "b")
)

h21_beta <- brm(
  p ~ 1,
  data = subj_rules, 
  family = student, prior = prior_p,
  cores = ncore, iter = 12000, warmup = 2000,
  save_pars = save_pars(all = TRUE)
)

h21_beta_eff <- brm(
  p ~ rules,
  data = subj_rules, 
  family = student, prior = prior_p_r,
  cores = ncore, iter = 12000, warmup = 2000,
  save_pars = save_pars(all = TRUE)
)

