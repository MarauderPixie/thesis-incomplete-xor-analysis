### conditions as written on a napkin:
# 1: no rll, no ordering
# 2: no rr, simple rule first
# 3: rll, no ordering
# 4: rll, srf

dummy <- read_csv("Downloads/results_18_incomplete-xor-testrun-3_Tobi+A..csv") %>% 
  mutate(
    assignment = case_when(assignment == "1" ~ "correct1",
                           assignment == "2" ~ "correct2",
                           TRUE ~assignment)
  )

# untrained trials:
utt <- paste0(
  "images/", c("e45_4000-366.7.jpg", "e44_4000-233.3.jpg", "e43_4000-100.jpg",
                "e38_3583-366.7.jpg", "e37_3583-233.3.jpg", "e36_3583-100.jpg",
                "e31_3167-366.7.jpg", "e30_3167-233.3.jpg", "e29_3167-100.jpg")
  )

train <- dummy %>% 
  filter(!is.na(correct1)) %>% 
  mutate(
    subj = as_factor(submission_id),
    condition = as_factor(condition),
    block = rep(rep(1:12, each = 8), 4) %>% as_factor(),
    correct = ifelse(assignment == "correct1", correct1, correct2)
  ) %>% 
  select(-correct1, -correct2, -education, -gender, -submission_id)

general <- dummy %>% 
  filter(image %in% utt) %>% 
  mutate(
    subj = as_factor(submission_id),
    condition = as_factor(condition),
    exin = case_when(response == "A" & assignment == "correct1" ~ 0,
                     response == "B" & assignment == "correct2" ~ 0,
                     TRUE ~ 1) # needs to be validated very very much!!
  ) %>% 
  select(-correct1, -correct2, -education, -gender,
         -submission_id, -assignment)


aggr <- train %>% 
  group_by(subj, condition, block) %>% 
  summarise(
    acc = mean(response == correct)
  )


library(brms)
accmod1 <- brm(data = aggr, acc ~ block + (block|subj), iter = 2000)
accmod2 <- brm(data = aggr, acc ~ block + (1|subj), iter = 2000)
accmod3 <- brm(data = aggr, acc ~ condition * block + (1 | subj + block), iter = 2000)


binmod1 <- brm(data = general, exin ~ condition + (1|subj), family = binomial)
binmod2 <- brm(data = general, exin ~ condition + (1|subj), family = bernoulli)
