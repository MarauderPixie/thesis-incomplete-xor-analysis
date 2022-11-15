source("init.R")

## Data ----
extra_binom <- readRDS("data-clean/trials-transfer.rds") %>% 
  filter(item == "transfer") %>% 
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

beobachtet <- extra_binom %>% 
  group_by(rules, blocked) %>% 
  summarise(
    mean_p = mean(p),
    sd_p   = sd(p),
    mean_k = mean(k),
    sd_k   = sd(k),
    mean_ckab6 = mean(exab6)
  )

## Priors ----
prior_null <- c(
  set_prior("student_t(3, 0, 1)", class = "Intercept"), 
  set_prior("student_t(3, 0, 1)", class = "sd", lb = 0)
)
prior_effect <- c(
  set_prior("student_t(3, 0, 1)", class = "Intercept"), 
  set_prior("student_t(3, 0, 1)", class = "sd", lb = 0), 
  set_prior("student_t(3, 0, 1)", class = "b")
)

## H1 Models ----
h1_null <- brm(
  data = extra_binom,
  k|trials(n) ~ 1 + (1 | subj_id),
  family = binomial(), prior = prior_null,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

h1_blocked <- update(h1_null, k|trials(n) ~ blocked + (1 | subj_id), 
                     newdata = extra_binom, prior = prior_effect, cores = ncore)
h1_rules   <- update(h1_null, k|trials(n) ~ rules + (1 | subj_id), 
                     newdata = extra_binom, prior = prior_effect, cores = ncore)
h1_both    <- update(h1_null, k|trials(n) ~ rules + blocked + (1 | subj_id), 
                     newdata = extra_binom, prior = prior_effect, cores = ncore)
h1_inter   <- update(h1_null, k|trials(n) ~ rules * blocked + (1 | subj_id), 
                     newdata = extra_binom, prior = prior_effect, cores = ncore)

saveRDS(h1_null,    "models/h10.rds")
saveRDS(h1_blocked, "models/h11.rds")
saveRDS(h1_rules,   "models/h12.rds")
saveRDS(h1_both,    "models/h131.rds")
saveRDS(h1_inter,   "models/h132.rds")
rm(beobachtet, extra_binom, prior_effect, prior_null)
