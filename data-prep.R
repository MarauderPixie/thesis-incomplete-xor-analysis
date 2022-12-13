source("init.R")
source("magpie-credentials.R")

library(httr)
library(rvest)
library(lubridate)

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

## time range where I f**k*d up the assignment (sorta)
date1 <- as.POSIXct("2022-10-26 12:59:00") # finished deployment github time stamp
date2 <- as.POSIXct("2022-10-26 19:15:00")
intvl <- lubridate::interval(date1, date2)

#### all experimental data ----
data_raw <- GET(
  "https://magpie.jemu.name/experiments/7/retrieve",
  authenticate(user, pass)
) |>
  content(as = "text", encoding = "UTF-8") |>
  read_csv() 

data_fixed <- data_raw %>% 
  mutate(
    started   = lubridate::as_datetime(experiment_start_time / 1000, 
                                       tz = "Europe/Berlin"),
    submitted = lubridate::as_datetime(experiment_end_time / 1000, 
                                       tz = "Europe/Berlin"),
    condition = ifelse(started %within% intvl, 3, condition)
  )


# all_data <- data_fixed %>% 
all_data <- read_csv("data-raw/fixed-data-27-10-2022.csv") %>% 
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
    started   = lubridate::as_datetime(experiment_start_time / 1000, 
                                       tz = "Europe/Berlin"),
    submitted = lubridate::as_datetime(experiment_end_time / 1000, 
                                       tz = "Europe/Berlin"),
    # condition = ifelse(started %within% intvl, 3, condition),
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
    # massively convoluted, but ain't nobody got time for aesthetics at this point:
    condition = case_when(condition == 1 ~ "control",
                          condition == 2 ~ "blocked",
                          condition == 3 ~ "rules",
                          condition == 4 ~ "both") %>% 
      as_factor() %>% fct_relevel("control", "rules", "blocked"),
    duration  = experiment_duration / 1000,
    Group = case_when(
      condition == "control" ~ 1, 
      condition == "blocked" ~ 3,
      condition == "rules" ~ 2,
      condition == "both" ~ 4
    ) %>% factor(labels = c("No Treatment", 
                            "Instructions", 
                            "Blocked", 
                            "Blocked + Instructions"))
  )

#### demographics ----
data_post <- all_data %>% 
  select(started, submitted, subj_id, condition, rules, Group,
         blocked, age, duration, languages, strategy) %>% 
  distinct()

#### training data ----
dtrain <- all_data %>% 
  filter(phase == "training") %>% 
  group_by(subj_id) %>% 
  mutate(
    correct = response_normalized == correct_normalized,
    trial   = seq(1:96),
    block   = rep(1:12, each = 8),
    .groups = "drop"
  ) %>% 
  select(subj_id, condition, rules, blocked, trial, block, image, Group,
         response_normalized, response_time, correct) %>% 
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
  select(subj_id, condition, rules, blocked, image, item, img_x, img_y, Group,
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
         Group, prob, response_time)


#### Apply exclusion criteria ----
exctest <- dtrain %>% 
  filter(block > 9) %>% 
  # group_by(subj_id, image) %>% 
  group_by(subj_id) %>% 
  summarise(
    k = sum(correct)
  ) %>% 
  filter(k < 17)

## deemed unfit by manual inspection, a bit clunky, but meh...
exclusions <- c("207", "246", "350", as.character(exctest$subj_id))

exclud_train <- dtrain %>% 
  filter(subj_id %in% exclusions)
exclud_trans <- dtrans %>% 
  filter(subj_id %in% exclusions)

data_post <- data_post %>%
  filter(!(subj_id %in% exclusions)) %>% 
  droplevels()
dtrain <- dtrain %>% 
  filter(!(subj_id %in% exclusions)) %>% 
  droplevels()
dtrans <- dtrans %>% 
  filter(!(subj_id %in% exclusions)) %>% 
  droplevels()
dprob  <- dprob %>%
  filter(!(subj_id %in% exclusions)) %>% 
  droplevels()


#### save to disk & cleanup ----
# to-do: die ganzen type-casts gehen natürlich wieder verloren hier m)
# write_csv(data_post, "data-clean/demographics.csv")
# write_csv(dtrain,    "data-clean/trials-training.csv")
# write_csv(dtrans,    "data-clean/trials-transfer.csv")
# write_csv(dprob,     "data-clean/trials-probability.csv")
write_csv(data_raw,   "data-raw/raw-data-27-10-2022.csv")
write_csv(data_fixed, "data-raw/fixed-data-27-10-2022.csv")
saveRDS(data_raw,     "data-raw/raw-data-27-10-2022.rds")
saveRDS(data_fixed,   "data-raw/fixed-data-27-10-2022.rds")

saveRDS(exclud_train, "data-clean/exclusions_training.rds")
saveRDS(exclud_trans, "data-clean/exclusions_transfer.rds")
saveRDS(data_post, "data-clean/demographics.rds")
saveRDS(dtrain,    "data-clean/trials-training.rds")
saveRDS(dtrans,    "data-clean/trials-transfer.rds")
saveRDS(dprob,     "data-clean/trials-probability.rds")


fs::dir_copy(
  "data-clean/", 
  "../writing/data/", 
  overwrite = TRUE
)

cat("total n:", n_total, "\n")
cat("after exclusion: ", n_total - length(exclusions), "\n")
rm(pass, user, trial_transfer, critical, date1, date2, intvl, 
   data_raw, excluded, neutral, all_data, data_post, dtrain, 
   dtrans, dprob, exctest, data_fixed, exclusions, n_total)
