library(tidyverse)
library(brms)

### condition-assignment:
# 1: no rll, no ordering
# 2: no rr, simple rule first
# 3: rll, no ordering
# 4: rll, srf


# the data used here was collected in several developmental stages of the experiment;
# final results will look very similar in structure such that this example should
# completely suffice to serve as an example
dummy <- read_csv("data-raw/messy-dummy-data.csv") %>% 
  mutate(
    assignment = case_when(assignment == "1" ~ "correct1",
                           assignment == "2" ~ "correct2",
                           TRUE ~assignment)
  )

# incomplete category images:
utt <- paste0(
  "images/", c("e45_4000-366.7.jpg", "e44_4000-233.3.jpg", "e43_4000-100.jpg",
               "e38_3583-366.7.jpg", "e37_3583-233.3.jpg", "e36_3583-100.jpg",
               "e31_3167-366.7.jpg", "e30_3167-233.3.jpg", "e29_3167-100.jpg")
)

# data prep
train <- dummy %>% 
  filter(!is.na(correct1)) %>% 
  mutate(
    subj = as_factor(submission_id),
    condition = as_factor(condition),
    block = rep(rep(1:12, each = 8), 4) %>% as_factor(),
    image = as_factor(image),
    correct = ifelse(assignment == "correct1", correct1, correct2),
    score = ifelse(correct == response, 1, 0)
  ) %>% 
  select(-correct1, -correct2, -education, -gender, -submission_id)

aggr <- train %>% 
  group_by(subj, condition, block, image) %>% 
  summarise(
    acc = mean(response == correct)
  )

aggr_binom <- train %>% 
  group_by(subj, condition, block) %>% 
  summarise(
    k = sum(response == correct),
    n = n()
  )


general <- dummy %>% 
  filter(image %in% utt) %>% 
  mutate(
    subj = as_factor(submission_id),
    condition = as_factor(condition),
    exin = case_when(response == "A" & assignment == "correct1" ~ 0,
                     response == "B" & assignment == "correct2" ~ 0,
                     TRUE ~ 1) # needs to be validated very very much!!
  ) %>% 
  select(subj, condition, exin)

general_binom <- general %>% 
  group_by(subj, condition) %>% 
  summarise(
    k = sum(exin),
    n = n()
  )




# final models, approximately:
model_training <- brm(data = aggr, acc ~ condition * block + (1|subj + block))
model_general  <- brm(data = general, exin ~ condition + (1|subj), family = bernoulli)

alt_model_training <- brm(data = aggr_binom, 
                          k|trials(n) ~ condition * block + (1|subj + block), 
                          family = binomial)
alt_model_general  <- brm(data = general_binom, 
                          k|trials(n) ~ condition + (1|subj), 
                          family = binomial)



## model playground
alt_model_training <- brm(data = train, 
                          score ~ condition * block + (1|subj + image), 
                          family = bernoulli)

# "simulate" some data
subj <- paste0("S0", str_pad(1:100, 2, pad = "0")) %>% as_factor()
cond <- rep(LETTERS[1:4], each = 25) %>% as_factor()
block <- as_factor(1:12)
imgs <- paste0("img", 1:8) # %>% as_factor()

simdat <- tibble(
  subj = rep(subj, each = 96),
  condition = rep(cond, each = 96),
  block = rep(block, times = 800),
  imgage = rep(lapply(1:12, sample, x = imgs, size = 8) %>% flatten_chr() %>% factor(levels = imgs), 100),
  correct = c(
    rbernoulli(2400, .3),
    rbernoulli(2400, .5),
    rbernoulli(2400, .5),
    rbernoulli(2400, .7))
  )

sim_mod1 <- brm(data = simdat, 
                correct ~ condition * block + (1|subj + imgage), 
                family = bernoulli,
                save_pars = save_pars(all = TRUE))

saveRDS(sim_mod1, "models/simulation01.rds")

sim_mod2 <- brm(data = simdat, 
                correct ~ condition + (1|subj + imgage + block), 
                family = bernoulli,
                save_pars = save_pars(all = TRUE))

saveRDS(sim_mod2, "models/simulation02.rds")


loo1 <- loo::loo(sim_mod1, save_pars(all = TRUE))
loo2 <- loo::loo(sim_mod2, save_pars(all = TRUE))

loo::loo_compare(loo1, loo2)
