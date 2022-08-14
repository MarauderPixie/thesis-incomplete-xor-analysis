library(tidyverse)

## trial data
# trial_blocked_shape <- read_csv("data-trials/training-blocked-shape.csv")
# trial_blocked_size  <- read_csv("data-trials/training-blocked-size.csv")
# trial_mixed         <- read_csv("data-trials/training-mixed.csv")
trial_transfer      <- read_csv("data-trials/transfer.csv")

## transfer images
neutral <- trial_transfer %>% 
  filter(
    str_detect(image, "_2750-") | str_detect(image, "-500.")
  ) %>%
  pluck("image")
critical <- paste0(
  "images/", c("e45_4000-366.7.jpg", "e44_4000-233.3.jpg", "e43_4000-100.jpg",
               "e38_3583-366.7.jpg", "e37_3583-233.3.jpg", "e36_3583-100.jpg",
               "e31_3167-366.7.jpg", "e30_3167-233.3.jpg", "e29_3167-100.jpg")
)

## experimental data
all_data <- read_csv("data-raw/results_7_incomplete-xor_Tobi.csv") %>% 
  mutate(
    # condition = case_when(
    #   condition == 1 ~ "control",
    #   condition == 2 ~ "blocked",
    #   condition == 3 ~ "rules",
    #   condition == 4 ~ "both"
    #   ),
    rules    = ifelse(condition == 3 | condition == 4, 1, 0),
    blocked  = ifelse(condition == 2 | condition == 4, 1, 0),
    ext_cat  = ifelse(assignment == "correct1", "Grot", "Nobz"),
    duration = experiment_duration / 1000
  )

data_post <- all_data %>% 
  select(submission_id, rules, blocked, duration, age, languages, strategy) %>% 
  distinct()

dtrain <- all_data %>% 
  filter(!is.na(correct1)) %>% 
  mutate(
    correct_item = ifelse(assignment == "correct1", correct1, correct2),
    correct = response == correct_item
  ) %>% 
  group_by(submission_id) %>% 
  mutate(
    trial = seq(1:96),
    block = rep(1:12, each = 8)
  ) %>% 
  select(submission_id, rules, blocked, trial, 
         block, image, correct, response_time) %>% 
  ungroup()

# ich glaub, für dtrans und dprob braucht es jeweils das assignment nicht mehr
dtrans <- all_data %>% 
  filter(is.na(correct1), !is.na(response)) %>% 
  mutate(
    item = case_when(
      image %in% critical ~ "transfer",
      image %in% neutral ~ "neutral",
      TRUE ~ "training"
    ),
    extrapol = case_when(
      item == "transfer" & ext_cat == response ~ 1,
      item == "transfer" ~ 0,
      TRUE ~ NA_real_)
  ) %>% 
  select(submission_id, rules, blocked, assignment, image, 
         item, response, extrapol, response_time)

dprob <- all_data %>% 
  filter(is.na(correct1), is.na(response)) %>% 
  mutate(
    item = case_when(
      image %in% critical ~ "transfer",
      image %in% neutral ~ "neutral",
      TRUE ~ "training"
    ),
    probA = 100 - prob,
    probB = prob,
  ) %>% 
  select(submission_id, rules, blocked, assignment, image, 
         probA, probB, response_time)


# save to disk
write_csv(data_post, "data-clean/demographics.csv")
write_csv(dtrain,    "data-clean/trials-training.csv")
write_csv(dtrans,    "data-clean/trials-transfer.csv")
write_csv(dprob,     "data-clean/trials-probability.csv")

rm(trial_transfer, critical, neutral, all_data,
   data_post, dtrain, dtrans, dprob)
