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

print(beobachtet)


## Priors ----
p0 <- c(
  set_prior("student_t(5, 0, 1)", class = "Intercept"),
  set_prior("student_t(5, 0, 1)", class = "sd", lb = 0), 
  set_prior("student_t(5, 0, 1)", class = "b")
)

## performs worst in terms of BF ##
p1 <- c(
  set_prior("student_t(5, 0, 1)", class = "Intercept", lb = -10, ub = 10), 
  set_prior("student_t(5, 0, 1)", class = "sd", lb = 0), 
  set_prior("student_t(5, 0, 1)", class = "b", lb = -10, ub = 10)
)
###################################

## best in terms of BF ##
p2 <- c(
  set_prior("student_t(5, -5.4, .7)", class = "Intercept"),
  set_prior("student_t(5,    4, .5)", class = "sd", lb = 0), 
  set_prior("student_t(5,  .74, .7)", class = "b")
)
#########################

p3 <- c(
  set_prior("student_t(5, -5.4, .7)", class = "Intercept", lb = -15, ub = 5), 
  set_prior("student_t(5,    4, .5)", class = "sd", lb = 0), 
  set_prior("student_t(5,  .74, .7)", class = "b", lb = -9, ub = 11)
)

# 🎵 qlogis(beobachtet$mean_p)
# [1] -2.572612 -1.398416 -1.916923 -1.177203
# 🎵 qlogis(mean(beobachtet$mean_p[2:4]) - beobachtet$mean_p[1])
# [1] -2.027418
p4 <- c(
  set_prior("student_t(5, -2.57, 1)", class = "Intercept"), 
  set_prior("student_t(5,     4, 1)", class = "sd", lb = 0), 
  set_prior("student_t(5, -2.03, 1)", class = "b")
)

p5 <- c(
  set_prior("student_t(7, -2.57, 1)", class = "Intercept"), 
  set_prior("student_t(7,     4, 1)", class = "sd", lb = 0), 
  set_prior("student_t(7, -2.03, 1)", class = "b")
)


p6 <- c(
  set_prior("student_t(3, -2.57, 1)", class = "Intercept"), 
  set_prior("student_t(3,     4, 1)", class = "sd", lb = 0), 
  set_prior("student_t(3, -2.03, 1)", class = "b")
)

p7 <- c(
  set_prior("student_t(3, 0, 1)", class = "Intercept"),
  set_prior("student_t(3, 0, 1)", class = "sd", lb = 0), 
  set_prior("student_t(3, 0, 1)", class = "b")
)

p8 <- c(
  set_prior("student_t(7, 0, 1)", class = "Intercept"),
  set_prior("student_t(7, 0, 1)", class = "sd", lb = 0), 
  set_prior("student_t(7, 0, 1)", class = "b")
)

#### Prior Comparison ----
## for simplicity and manageability only the full model(s) will be compared
fit_p0 <- brm(
  data = extra_binom,
  k|trials(n) ~ rules * blocked + (1 | subj_id),
  family = binomial(), prior = p0,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

fit_p1 <- update(fit_p0, prior = p1, cores = ncore)
fit_p2 <- update(fit_p0, prior = p2, cores = ncore)
fit_p3 <- update(fit_p0, prior = p3, cores = ncore)
fit_p4 <- update(fit_p0, prior = p4, cores = ncore)
fit_p5 <- update(fit_p0, prior = p5, cores = ncore)
fit_p6 <- update(fit_p0, prior = p6, cores = ncore)
fit_p7 <- update(fit_p0, prior = p7, cores = ncore)
fit_p8 <- update(fit_p0, prior = p8, cores = ncore)

## bridgesampling ----
bs0 <- bridge_sampler(fit_p0)
bs1 <- bridge_sampler(fit_p1)
bs2 <- bridge_sampler(fit_p2)
bs3 <- bridge_sampler(fit_p3)
bs4 <- bridge_sampler(fit_p4)
bs5 <- bridge_sampler(fit_p5)
bs6 <- bridge_sampler(fit_p6)
bs7 <- bridge_sampler(fit_p7)
bs8 <- bridge_sampler(fit_p8)

bs0
bs1
bs2
bs3
bs4
bs5
bs6
bs7
bs8
