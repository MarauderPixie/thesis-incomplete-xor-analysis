prior_open <- c(
  set_prior("student_t(3, 0, 2.5)", class = "Intercept"), 
  set_prior("student_t(3, 0, 2.5)", class = "sd"), 
  set_prior("student_t(3, 0, 2.5)", class = "b")
)

prior_trunc <- prior_effect <- c(
  set_prior("student_t(3, 0, 1)", class = "Intercept", lb = -10, ub = 10), 
  set_prior("student_t(3, 0, 1)", class = "sd"), 
  set_prior("student_t(3, 0, 1)", class = "b", lb = -10, ub = 10)
)


h1_full_open <- brm(
  data = extra_all,
  extrapolation ~ rules * blocked + (1 | subj_id), 
  family = bernoulli(), prior = prior_open,
  cores = ncore, iter = 12000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

h1_full_trunc <- update(h1_full_open, prior = prior_trunc)

bayes_factor(h1_full_trunc, h1_full_open)
# Estimated Bayes factor in favor of 
# h1_full_trunc over h1_full_open: 0.09079

prior_trunc_sd <- prior_effect <- c(
  set_prior("student_t(3, 0, 1)", class = "Intercept", lb = -10, ub = 10), 
  set_prior("student_t(3, 0, 1)", class = "sd", lb = 0), 
  set_prior("student_t(3, 0, 1)", class = "b", lb = -10, ub = 10)
)

h1_full_trunc_sd <- update(h1_full_open, prior = prior_trunc_sd)
bayes_factor(h1_full_trunc_sd, h1_full_open)
# Estimated Bayes factor in favor of 
# h1_full_trunc_sd over h1_full_open: 0.08277