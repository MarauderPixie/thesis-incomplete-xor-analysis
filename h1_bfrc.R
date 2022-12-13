library(brms)
ncore <- parallel::detectCores()

#### Data ----
## transfer
h1_null <- readRDS("models/h1_transfer_re/h10.rds")

#### Priors ----
# 30% of subjs having at least 6/9 extrials
# p1   <- c(
#   set_prior("student_t(3, -1.4, 1)", class = "Intercept"),
#   set_prior("student_t(3,  2.2, 1)", class = "sd", lb = 0, ub = 5)
# )
# p2 <- c(
#   set_prior("student_t(3, -1.4, 1)", class = "Intercept"), 
#   set_prior("student_t(3,  2.2, 1)", class = "sd", lb = 0, ub = 5),
#   set_prior("student_t(3,   .5, 1)", class = "b")
# )


p3 <- c( # more t-df
  set_prior("student_t(7, -1.4, 1)", class = "Intercept"), 
  set_prior("student_t(7,  2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(7,   .5, 1)", class = "b")
)
p4 <- c( # assuming no effect
  set_prior("student_t(3, -1.4, 1)", class = "Intercept"), 
  set_prior("student_t(3,  2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(3,    0, 1)", class = "b")
)
p5 <- c( # lower & upper bounds
  set_prior("student_t(3, -1.4, 1)", class = "Intercept", lb = -10, ub = 10), 
  set_prior("student_t(3,  2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(3,   .5, 1)", class = "b")
)

p6 <- c( # NoFX & lower and upper bounds
  set_prior("student_t(3, -1.4, 1)", class = "Intercept", lb = -10, ub = 10), 
  set_prior("student_t(3,  2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(3,    0, 1)", class = "b")
)
p7 <- c( # more t-df & NoFX
  set_prior("student_t(7, -1.4, 1)", class = "Intercept"), 
  set_prior("student_t(7,  2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(7,    0, 1)", class = "b")
)
p8 <- c( # more t-df & lower and upper bounds
  set_prior("student_t(7, -1.4, 1)", class = "Intercept", lb = -10, ub = 10), 
  set_prior("student_t(7,  2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(7,   .5, 1)", class = "b")
)


p9 <- c( # more t-df, NoFX and lower & upper bounds
  set_prior("student_t(7, -1.4, 1)", class = "Intercept", lb = -10, ub = 10), 
  set_prior("student_t(7,  2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(7,    0, 1)", class = "b")
)


# full vs null ----
h1_b2 <- readRDS("models/h1_transfer_re/h132.rds")
h1_b3 <- update(h1_b2, cores = ncore, prior = p3)
h1_b4 <- update(h1_b2, cores = ncore, prior = p4)
h1_b5 <- update(h1_b2, cores = ncore, prior = p5)
h1_b6 <- update(h1_b2, cores = ncore, prior = p6)
h1_b7 <- update(h1_b2, cores = ncore, prior = p7)
h1_b8 <- update(h1_b2, cores = ncore, prior = p8)
h1_b9 <- update(h1_b2, cores = ncore, prior = p9)

bayestestR::bayesfactor_models(h1_null, h1_b2, h1_b3, h1_b4,
                               h1_b5, h1_b6, h1_b7, h1_b8, h1_b9)


# h1_both -----------------------------------------------------------------
# Bayes Factors for Model Comparison
# 
# Model                             BF
# [2] blocked + rules + (1 | subj_id) 8.15
# [3] blocked + rules + (1 | subj_id) 4.62
# [4] blocked + rules + (1 | subj_id) 4.25
# [5] blocked + rules + (1 | subj_id) 7.43
# [6] blocked + rules + (1 | subj_id) 3.79
# [7] blocked + rules + (1 | subj_id) 2.47
# [8] blocked + rules + (1 | subj_id) 4.60
# [9] blocked + rules + (1 | subj_id) 2.17
# 
# * Against Denominator: [1] 1 + (1 | subj_id)
# *   Bayes Factor Type: marginal likelihoods (bridgesampling)


# h1_inter ----------------------------------------------------------------
# Bayes Factors for Model Comparison
# 
# Model                                             BF
# [2] blocked + rules + blocked:rules + (1 | subj_id) 6.30
# [3] blocked + rules + blocked:rules + (1 | subj_id) 3.42
# [4] blocked + rules + blocked:rules + (1 | subj_id) 3.64
# [5] blocked + rules + blocked:rules + (1 | subj_id) 6.04
# [6] blocked + rules + blocked:rules + (1 | subj_id) 3.21
# [7] blocked + rules + blocked:rules + (1 | subj_id) 2.08
# [8] blocked + rules + blocked:rules + (1 | subj_id) 3.92
# [9] blocked + rules + blocked:rules + (1 | subj_id) 1.97
# 
# * Against Denominator: [1] 1 + (1 | subj_id)
# *   Bayes Factor Type: marginal likelihoods (bridgesampling)


# Blocked vs. null ----
h1_b2 <- readRDS("models/h1_transfer_re/h11.rds")
h1_b3 <- update(h1_b2, cores = ncore, prior = p3)
h1_b4 <- update(h1_b2, cores = ncore, prior = p4)
h1_b5 <- update(h1_b2, cores = ncore, prior = p5)
h1_b6 <- update(h1_b2, cores = ncore, prior = p6)
h1_b7 <- update(h1_b2, cores = ncore, prior = p7)
h1_b8 <- update(h1_b2, cores = ncore, prior = p8)
h1_b9 <- update(h1_b2, cores = ncore, prior = p9)

bayestestR::bayesfactor_models(h1_null, h1_b2, h1_b3, h1_b4,
                               h1_b5, h1_b6, h1_b7, h1_b8, h1_b9)

# Computation of Bayes factors: estimating marginal likelihood, please wait...
# Bayes Factors for Model Comparison
# 
# Model                     BF
# [2] blocked + (1 | subj_id) 6.45
# [3] blocked + (1 | subj_id) 3.86
# [4] blocked + (1 | subj_id) 4.95
# [5] blocked + (1 | subj_id) 6.57
# [6] blocked + (1 | subj_id) 4.52
# [7] blocked + (1 | subj_id) 2.62
# [8] blocked + (1 | subj_id) 3.46
# [9] blocked + (1 | subj_id) 2.74
# 
# * Against Denominator: [1] 1 + (1 | subj_id)
# *   Bayes Factor Type: marginal likelihoods (bridgesampling)


# Rules vs. null ----
h1_b2 <- readRDS("models/h1_transfer_re/h12.rds")
h1_b3 <- update(h1_b2, cores = ncore, prior = p3)
h1_b4 <- update(h1_b2, cores = ncore, prior = p4)
h1_b5 <- update(h1_b2, cores = ncore, prior = p5)
h1_b6 <- update(h1_b2, cores = ncore, prior = p6)
h1_b7 <- update(h1_b2, cores = ncore, prior = p7)
h1_b8 <- update(h1_b2, cores = ncore, prior = p8)
h1_b9 <- update(h1_b2, cores = ncore, prior = p9)

bayestestR::bayesfactor_models(h1_null, h1_b2, h1_b3, h1_b4,
                               h1_b5, h1_b6, h1_b7, h1_b8, h1_b9)

# [2] rules + (1 | subj_id) 0.865
# [3] rules + (1 | subj_id) 0.462
# [4] rules + (1 | subj_id) 0.687
# [5] rules + (1 | subj_id) 0.731
# [6] rules + (1 | subj_id) 0.731
# [7] rules + (1 | subj_id) 0.392
# [8] rules + (1 | subj_id) 0.574
# [9] rules + (1 | subj_id) 0.387