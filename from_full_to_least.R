source("init.R")

#### Data ----
## transfer
extra_berni <- readRDS("data-clean/trials-transfer.rds") %>% 
  filter(item == "transfer")

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

## Priors ----
# 30% having at least 6/9 extrials
prior_null   <- set_prior(
  "student_t(3, -1.4, 1)", class = "Intercept", lb = -4.6
)
prior_effect <- c(
  set_prior("student_t(3, -1.4, 1)", class = "Intercept", lb = -4.6), 
  set_prior("student_t(3, .5, 1)", class = "b")
)

## (somewhat sensible) RE models
h1_re_null <- brm(
  extrapolation ~ 1 + (1 | subj_id),
  data = extra_berni, family = bernoulli(), prior = prior_null,
  cores = ncore, iter = 16000, warmup = 3000,
  control = list(adapt_delta = 0.92),
  save_pars = save_pars(all = TRUE)
)

h1_re <- brm(
  extrapolation ~ blocked * rules + (1 | subj_id),
  data = extra_berni, family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 16000, warmup = 3000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

saveRDS(h1_re, "models/h1_transfer_re/h1_min.rds")

# h1_re_img <- brm(
#   extrapolation ~ blocked * rules + (1 | subj_id + image),
#   data = extra_berni, family = bernoulli(), prior = prior_effect,
#   cores = ncore, iter = 16000, warmup = 3000,
#   control = list(adapt_delta = 0.9),
#   save_pars = save_pars(all = TRUE)
# )

h1_re_img_slope <- brm(
  extrapolation ~ blocked * rules + (image | subj_id),
  data = extra_berni, family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 16000, warmup = 3000,
  control = list(adapt_delta = 0.92),
  save_pars = save_pars(all = TRUE)
)
saveRDS(h1_re_img_slope, "models/h1_transfer_re/h1_max.rds")

h1_med <- brm(
  extrapolation ~ blocked * rules + (image || subj_id),
  data = extra_berni, family = bernoulli(), prior = prior_effect,
  cores = ncore, iter = 16000, warmup = 3000,
  control = list(adapt_delta = 0.92),
  save_pars = save_pars(all = TRUE)
)
saveRDS(h1_med, "models/h1_transfer_re/h1_med.rds")


## hope for the best
re_min <- readRDS("models/h1_transfer_re/h1_min.rds")
re_med <- readRDS("models/h1_transfer_re/h1_med.rds")
re_max <- readRDS("models/h1_transfer_re/h1_max.rds")

l_max <- loo(re_max)
saveRDS(l_max, "models/l_max.rds")
l_med <- loo(re_med)
saveRDS(l_med, "models/l_med.rds")
l_min <- loo(re_min)
saveRDS(l_min, "models/l_min.rds")


l1 <- readRDS("models/l_min.rds") # and the winner is...!
l2 <- readRDS("models/l_med.rds")
l3 <- readRDS("models/l_max.rds")

loo_compare(l1, l2, l3)
