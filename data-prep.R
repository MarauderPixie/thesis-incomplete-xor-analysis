source("init.R")
source("magpie-credentials.R")

library(httr)
library(rvest)

n_total <- GET(
  "https://magpie.jemu.name/experiments",
  authenticate(user, pass)
) |>
  content() |>
  html_node("tr:nth-child(3) td:nth-child(4)") |>
  html_text2() |>
  as.integer()


#### trial data ----
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

#### all experimental data ----
all_data <- GET(
  "https://magpie.jemu.name/experiments/7/retrieve",
  authenticate(user, pass)
) |>
  content(as = "text", encoding = "UTF-8") |>
  read_csv() %>% 
# all_data <- read_csv("data-raw/results_7_incomplete-xor_Tobi--230822.csv") %>% 
  mutate(
    phase = case_when(
      !is.na(correct1) ~ "training",
      is.na(correct1) & !is.na(response) ~ "transfer_cat",
      is.na(correct1) & is.na(response) ~ "transfer_prob"
    )
  ) %>% 
  select(-correct1, -correct2) %>% 
  left_join(trial_transfer, by = "image") %>% 
  mutate(
    item = case_when(
      image %in% critical ~ "transfer",
      image %in% neutral ~ "neutral",
      TRUE ~ "training"
    ),
    correct_normalized = case_when(
      assignment == "correct2" & correct2 == "Nobz" ~ "Grot",
      assignment == "correct2" & correct2 == "Grot" ~ "Nobz",
      TRUE ~ correct1
    ),
    response_normalized = case_when(
      assignment == "correct2" & response == "Nobz" ~ "Grot",
      assignment == "correct2" & response == "Grot" ~ "Nobz",
      TRUE ~ response
    ),
    subj_id  = as_factor(submission_id),
    rules    = ifelse(condition == 3 | condition == 4, "yes", "no"),
    blocked  = ifelse(condition == 2 | condition == 4, "yes", "no"),
    # rules    = as_factor(rules),
    # blocked  = as_factor(blocked),
    condition = case_when(condition == 1 ~ "control",
                          condition == 2 ~ "blocked",
                          condition == 3 ~ "rules",
                          condition == 4 ~ "both") %>% 
      as_factor() %>% fct_relevel("control", "rules", "blocked"),
    submitted = lubridate::as_datetime(experiment_end_time / 1000, 
                                       tz = "Europe/Berlin"),
    duration  = experiment_duration / 1000
  )

#### demographics ----
data_post <- all_data %>% 
  select(submitted, subj_id, condition, rules, 
         blocked, age, duration, languages, strategy) %>% 
  distinct()

#### training data ----
dtrain <- all_data %>% 
  filter(phase == "training") %>% 
  group_by(subj_id) %>% 
  mutate(
    correct = response_normalized == correct_normalized,
    trial   = seq(1:96),
    block   = rep(1:12, each = 8)
  ) %>% 
  select(subj_id, condition, rules, blocked, trial, block, image, 
         response_normalized, response_time, correct) %>% 
  ungroup() %>% 
  rename("response" = response_normalized)

#### transfer data - categories ----
dtrans <- all_data %>% 
  filter(phase == "transfer_cat") %>% 
  mutate(
    extrapolation = case_when(
      item == "transfer" & response_normalized == "Grot" ~ 1,
      item == "transfer" ~ 0,
      TRUE ~ NA_real_),
    correct = case_when(
      # leicht widersprüchlich zum modell:
      # wenn die generalisierung schmaler oder breiter ist, sind auch
      # die gelernten kategorien kleiner oder größer, ergo müsste man
      # vllt was machen, was die "korrektheit" entsprechend anpasst
      item == "training" & response_normalized == correct_normalized ~ TRUE,
      item == "training" & response_normalized != correct_normalized ~ FALSE,
      TRUE ~ NA
    )
  ) %>% 
  select(subj_id, condition, rules, blocked, image, item, img_x, img_y, 
         response_normalized, response_time, correct, extrapolation) %>% 
  rename("response" = response_normalized)

#### transfer data - probabilities ----
dprob <- all_data %>% 
  filter(phase == "transfer_prob") %>% 
  mutate(
    probN  = ifelse(is.na(prob), 50, prob),
    probA = 100 - probN,
    probB = probN,
    prob  = ifelse(assignment == "correct2", probA, probB)
  ) %>% 
  select(subj_id, condition, rules, blocked, image, item, img_x, img_y, 
         # probA, probB, 
         prob, response_time)


#### save to disk & cleanup ----
# to-do: die ganzen type-casts gehen natürlcih wieder verloren hier m)
# write_csv(data_post, "data-clean/demographics.csv")
# write_csv(dtrain,    "data-clean/trials-training.csv")
# write_csv(dtrans,    "data-clean/trials-transfer.csv")
# write_csv(dprob,     "data-clean/trials-probability.csv")
saveRDS(data_post, "data-clean/demographics.rds")
saveRDS(dtrain,    "data-clean/trials-training.rds")
saveRDS(dtrans,    "data-clean/trials-transfer.rds")
saveRDS(dprob,     "data-clean/trials-probability.rds")

fs::dir_copy(
  "data-clean/", 
  "../writing/data/", 
  overwrite = TRUE
)

rm(pass, user, trial_transfer, critical, 
   neutral, all_data, data_post, dtrain, dtrans, dprob)
