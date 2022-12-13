## pick the "best" model
h1_both <- readRDS("models/h1_transfer_re/h131.rds")

# reminder of the priors
prior_effect <- c(
  set_prior("student_t(3, -1.4, 1)", class = "Intercept"), 
  set_prior("student_t(3,  2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(3,   .5, 1)", class = "b")
)

# set boundaries to priors
p2 <- c(
  set_prior("student_t(3, -1.4, 1)", class = "Intercept", lb = -10), 
  set_prior("student_t(3,  2.2, 1)", class = "sd", lb = 0, ub = 10),
  set_prior("student_t(3,   .5, 1)", class = "b", lb = -5, ub = 5)
)

h1_p2 <- update(h1_both, cores = ncore, prior = p2)

# fiddle with differences
p3 <- c(
  set_prior("student_t(3, -1.4, 1)", class = "Intercept", lb = -10), 
  set_prior("student_t(3,  2.2, 1)", class = "sd", lb = 0, ub = 10),
  set_prior("student_t(3,    1, 1)", class = "b", lb = -5, ub = 5)
)

h1_p3 <- update(h1_both, cores = ncore, prior = p3)



m1_crits <- add_criterion(h1_both, c("waic", "bayes_R2"))
m2_crits <- add_criterion(h1_p2, c("waic", "bayes_R2"))



report::report(m1_crits)













h1_summ <- summary(h1_draws)

oof <- h1_summ %>% 
  select(variable, mean) %>% 
  spread(variable, mean) %>% 
  gather(subj, val, starts_with("r_"), starts_with("z_")) %>% 
  mutate(
    param = ifelse(str_detect(subj, "Intercept"), "Intercept", "b"),
    id = ifelse(str_detect(subj, "Intercept"),
                str_remove(subj, "r_subj_id\\[") %>% str_remove(",Intercept\\]"),
                str_remove(subj, "z_1\\[1,") %>% str_remove("\\]")) %>% as.character()
  ) 



  mutate(
    across(starts_with("r_subj"), ~ inv_logit_scaled(Intercept + b_Intercept + .x))
  ) %>% glimpse()

