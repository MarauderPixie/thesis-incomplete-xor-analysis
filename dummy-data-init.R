setwd("~/Dokumente/Thesis/analysis/")
library(tidyverse)

d00 <- read_csv("data-raw/dummy-results-00.csv") %>% mutate(id = LETTERS[1])
d01 <- read_csv("data-raw/dummy-results-01.csv") %>% mutate(id = LETTERS[2])
d10 <- read_csv("data-raw/dummy-results-10.csv") %>% mutate(id = LETTERS[3])
d11 <- read_csv("data-raw/dummy-results-11.csv") %>% mutate(id = LETTERS[4])

dummy <- rbind.data.frame(d00, d01, d10, d11)

demographics <- dummy %>% 
  select(id, age, gender, education, languages, timeSpent, comments) %>% 
  distinct()

# I need:
# - a list of the untrained trials
# - a mapping of the trained trials to their category
#   -> I should randomize the assignment of categories

# untrained trials:
utt <- c("e45_4000-366.7.jpg", "e44_4000-233.3.jpg", "e43_4000-100.jpg",
         "e38_3583-366.7.jpg", "e37_3583-233.3.jpg", "e36_3583-100.jpg",
         "e31_3167-366.7.jpg", "e30_3167-233.3.jpg", "e29_3167-100.jpg")


results <- dummy %>% 
  select(id, trial_number, trial_name, picture, RT, correct, response) %>% 
  rename(expected = "correct") %>% 
  mutate(
    correct = expected == response,
    picture = str_remove_all(picture, "stimuli-training/"),
    picture = str_remove_all(picture, "stimuli-full/"),
    stimulus = if_else(picture %in% utt, "untrained", "trained") %>% as_factor()
  )

rm(d00, d01, d10, d11, dummy, utt)
write_csv(demographics, "data-raw/dummy-demographics.csv")
write_csv(results, "data-raw/dummy-results.csv")
