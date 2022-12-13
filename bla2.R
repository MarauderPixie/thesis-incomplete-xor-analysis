library(brms)
ncore <- parallel::detectCores()

# models
h2_b1 <- readRDS("models/h2_training/h21_null.rds")
h2_b2 <- readRDS("models/h2_training/h21_rules.rds")

# p2_null   <- c(
#   set_prior("student_t(3, 2, 1)", class = "Intercept"),
#   set_prior("student_t(3, 2.2, 1)", class = "sd", lb = 0, ub = 5)
# )
# p2_effect <- c(
#   set_prior("student_t(3, 2, 1)", class = "Intercept"), 
#   set_prior("student_t(3, 2.2, 1)", class = "sd", lb = 0, ub = 5),
#   set_prior("student_t(3, 0, 1)", class = "b")
# )

# intercept 0 -> 50% accuracy
p3 <- c(
  set_prior("student_t(3, 0, 1)", class = "Intercept"),
  set_prior("student_t(3, 2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(3, 0, 1)", class = "b")
)
# b = .4 -> about 10% effect
p4 <- c(
  set_prior("student_t(3,   2, 1)", class = "Intercept"),
  set_prior("student_t(3, 2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(3,  .4, 1)", class = "b")
)
# df = 7
p5 <- c(
  set_prior("student_t(7, 2, 1)", class = "Intercept"),
  set_prior("student_t(7, 2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(7, 0, 1)", class = "b")
)

## int = 0 & b = .4
p6 <- c(
  set_prior("student_t(3,   0, 1)", class = "Intercept"),
  set_prior("student_t(3, 2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(3,  .4, 1)", class = "b")
)
## int = 0 & df = 7
p7 <- c(
  set_prior("student_t(7, 0, 1)", class = "Intercept"),
  set_prior("student_t(7, 2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(7, 0, 1)", class = "b")
)
## b = .4 & df = 7
p8 <- c(
  set_prior("student_t(7,   2, 1)", class = "Intercept"),
  set_prior("student_t(7, 2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(7,  .4, 1)", class = "b")
)

### int = 0, b = .4 & df = 7
p9 <- c(
  set_prior("student_t(7,   0, 1)", class = "Intercept"),
  set_prior("student_t(7, 2.2, 1)", class = "sd", lb = 0, ub = 5),
  set_prior("student_t(7,  .4, 1)", class = "b")
)


h2_b3 <- update(h2_b2, cores = ncore, prior = p3)
h2_b4 <- update(h2_b2, cores = ncore, prior = p4)
h2_b5 <- update(h2_b2, cores = ncore, prior = p5)
h2_b6 <- update(h2_b2, cores = ncore, prior = p6)
h2_b7 <- update(h2_b2, cores = ncore, prior = p7)
h2_b8 <- update(h2_b2, cores = ncore, prior = p8)
h2_b9 <- update(h2_b2, cores = ncore, prior = p9)

# better save them
saveRDS(h2_b3, "models/h2_bfrc/h2_b3.rds")
saveRDS(h2_b4, "models/h2_bfrc/h2_b4.rds")
saveRDS(h2_b5, "models/h2_bfrc/h2_b5.rds")
saveRDS(h2_b6, "models/h2_bfrc/h2_b6.rds")
saveRDS(h2_b7, "models/h2_bfrc/h2_b7.rds")
saveRDS(h2_b8, "models/h2_bfrc/h2_b8.rds")
saveRDS(h2_b9, "models/h2_bfrc/h2_b9.rds")

bayestestR::bayesfactor_models(h2_b1, h2_b2, h2_b3, h2_b4,
                               h2_b5, h2_b6, h2_b7, h2_b8, h2_b9)

# Model                                           BF
# [2] rules + (1 | subj_id) + (0 + block | subj_id) 1.77
# [3] rules + (1 | subj_id) + (0 + block | subj_id) 2.28
# [4] rules + (1 | subj_id) + (0 + block | subj_id) 1.96
# [5] rules + (1 | subj_id) + (0 + block | subj_id) 1.95
# [6] rules + (1 | subj_id) + (0 + block | subj_id) 2.59
# [7] rules + (1 | subj_id) + (0 + block | subj_id) 2.50
# [8] rules + (1 | subj_id) + (0 + block | subj_id) 2.15
# [9] rules + (1 | subj_id) + (0 + block | subj_id) 2.62
