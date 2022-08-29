source("init.R")

library(httr)
library(rvest)

# n_total <- GET(
#   "https://magpie.jemu.name/experiments", 
#   authenticate(user, pass)
# ) |>
#   content() |>
#   html_node("tr:nth-child(3) td:nth-child(4)") |>
#   html_text2() |>
#   as.integer()


## trial data
# trial_blocked_shape <- read_csv("data-trials/training-blocked-shape.csv")
# trial_blocked_size  <- read_csv("data-trials/training-blocked-size.csv")
# trial_mixed         <- read_csv("data-trials/training-mixed.csv")
trial_transfer <- read_csv("data-trials/transfer.csv") %>% 
  mutate(
    img_x = rep(1:7, each = 7),
    img_y = rep(1:7, times = 7)
  )

## transfer images
neutral <- trial_transfer %>% 
  filter(
    str_detect(image, "_2750-") | str_detect(image, "-500.")
  ) %>%
  pull("image")
critical <- paste0(
  "images/", c("e45_4000-366.7.jpg", "e44_4000-233.3.jpg", "e43_4000-100.jpg",
               "e38_3583-366.7.jpg", "e37_3583-233.3.jpg", "e36_3583-100.jpg",
               "e31_3167-366.7.jpg", "e30_3167-233.3.jpg", "e29_3167-100.jpg")
)

## experimental data
source("magpie-credentials.R")

all_data <- GET(
  "https://magpie.jemu.name/experiments/7/retrieve",
  authenticate(user, pass)
) |>
  content(as = "text", encoding = "UTF-8") |>
  read_csv() %>% 
# all_data <- read_csv("data-raw/results_7_incomplete-xor_Tobi--230822.csv") %>% 
  mutate(
    submission_id = as_factor(submission_id),
    rules    = ifelse(condition == 3 | condition == 4, "yes", "no"),
    blocked  = ifelse(condition == 2 | condition == 4, "yes", "no"),
    # rules    = as_factor(rules),
    # blocked  = as_factor(blocked),
    condition = case_when(condition == 1 ~ "control",
                          condition == 2 ~ "blocked",
                          condition == 3 ~ "rules",
                          condition == 4 ~ "both"),
    ext_cat  = ifelse(assignment == "correct1", "Grot", "Nobz"),
    duration = experiment_duration / 1000
  )

data_post <- all_data %>% 
  select(submission_id, condition, rules, blocked, age, duration, languages, strategy) %>% 
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
  select(submission_id, condition, rules, blocked, 
         trial, block, image, correct, response_time) %>% 
  ungroup()

# ich glaub, für dtrans und dprob braucht es jeweils das assignment nicht mehr
dtrans <- all_data %>% 
  filter(is.na(correct1), !is.na(response)) %>% 
  left_join(trial_transfer, by = "image") %>% 
  mutate(
    item = case_when(
      image %in% critical ~ "transfer",
      image %in% neutral ~ "neutral",
      TRUE ~ "training"
    ),
    extrapol = case_when(
      item == "transfer" & ext_cat == response ~ 1,
      item == "transfer" ~ 0,
      TRUE ~ NA_real_),
    correct_item = ifelse(assignment == "correct1", correct1.y, correct2.y),
    correct = case_when(
      item == "training" & response == correct_item ~ TRUE,
      item == "training" & response != correct_item ~ FALSE,
      TRUE ~ NA
    )
  ) %>% 
  select(submission_id, condition, rules, blocked, assignment, image, 
         item, response, correct, extrapol, response_time, img_x, img_y)

dprob <- all_data %>% 
  filter(is.na(correct1), is.na(response)) %>% 
  left_join(trial_transfer, by = "image") %>% 
  mutate(
    prob = ifelse(is.na(prob), 50, prob),
    item = case_when(
      image %in% critical ~ "transfer",
      image %in% neutral ~ "neutral",
      TRUE ~ "training"
    ),
    probA = 100 - prob,
    probB = prob,
  ) %>% 
  select(submission_id, condition, rules, blocked, assignment, image, 
         probA, probB, response_time, img_x, img_y)


## save to disk
# to-do: die ganzen type-casts gehen natürlcih wieder verloren hier m)
# write_csv(data_post, "data-clean/demographics.csv")
# write_csv(dtrain,    "data-clean/trials-training.csv")
# write_csv(dtrans,    "data-clean/trials-transfer.csv")
# write_csv(dprob,     "data-clean/trials-probability.csv")
saveRDS(data_post, "data-clean/demographics.rds")
saveRDS(dtrain,    "data-clean/trials-training.rds")
saveRDS(dtrans,    "data-clean/trials-transfer.rds")
saveRDS(dprob,     "data-clean/trials-probability.rds")

rm(pass, user, trial_transfer, critical, 
   neutral, all_data, data_post, dtrain, dtrans, dprob)
