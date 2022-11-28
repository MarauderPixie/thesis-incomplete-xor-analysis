#### Extrapolations ----
tbl2 <- extra_binom %>% 
  gather(cutoff, n, starts_with("exab")) %>% 
  filter(n != 0)

extra_binom %>% 
  group_by(rules, blocked) %>% 
  summarise(
    mean_p = mean(p),
    mean_k = mean(k),
    mean_ckab6 = mean(exab6)
  )

table(tbl2$condition, tbl2$cutoff) # |> prop.table(margin = 2) |> round(2)

#### Plots ----
## accuracy per picture per person
training <- transfer %>% 
  select(subj_id, image, img_x, img_y) %>% 
  right_join(training, by = c("subj_id", "image"))

training %>% 
  filter(block > 2) %>% 
  group_by(subj_id, img_x, img_y) %>% 
  summarise(
    p_correct = mean(correct),
    acc_state = ifelse(p_correct < 0.7, "critical", "fine"),
    img_x = ifelse(img_x > 2, img_x - 2, img_x),
    img_y = ifelse(img_y > 2, img_y - 2, img_y),
    .groups = "drop"
  ) %>% 
  ggplot(aes(img_x, img_y, fill = p_correct)) +
  facet_wrap(~subj_id) +
  geom_tile(size = .5, color = "white") +
  geom_text(color = "black", size = 2, aes(label = p_correct)) +
  scale_fill_viridis_c(option = "E") +
  # scale_fill_brewer(palette = "Set1") +
  theme_subjects

## it's time (pause) to take a Closer Look
## not at all arbitrarily chosen ofc, no Sir
oddballs <- c("147", "155", "168", "178", "181", "195",
              "207", "246", "260", "296", "338", "350")

learnd <- training %>% 
  filter(block > 8,
         subj_id %in% oddballs) %>% 
  group_by(subj_id, img_x, img_y) %>% 
  summarise(
    p_correct = mean(correct),
    .groups = "drop"
  ) 

ggplot(learnd, aes(img_x, img_y, fill = p_correct)) +
  facet_wrap(~subj_id) +
  geom_tile(size = .5, color = "white") +
  scale_fill_viridis_c() +
  theme_subjects

transfer %>% 
  filter(subj_id %in% oddballs) %>% 
  mutate(
    strip = paste0(subj_id, " (", condition, ")")
  ) %>% 
  ggplot(aes(img_x, img_y, fill = response)) +
    facet_wrap(~subj_id) +
    geom_tile(size = .5, color = "white") +
    geom_label(data = learnd, aes(label = p_correct, fill = NULL)) +
    scale_fill_viridis_d(option = "E") +
    theme_subjects

# Upon closer inspection of the actual data it was deemed necessary to change 
# the data exclusion criterion from a minimum accuracy over all trials in the 
# last three blocks of training to a minimum on a by-stimulus basis in the last 
# three blocks. This is due to some participants seemingly having classified 
# the stimuli along one dimension only, leading to extreme cases of 0% accuracy
# on the stimuli on which they had to apply the XOR rule (see fig. x for all 
# participants affected by the change).

#### Extrapolations / Transfer Phase ----
## transfer phase per person ----
transfer %>% 
  ggplot(aes(img_x, img_y, fill = response)) +
  facet_wrap(~subj_id) +
  geom_tile(size = .5, color = "white") +
  scale_fill_brewer(palette = "Set1") +
  theme_subjects


## transfer gradients per condition
extra_pols <- filter(extra_binom, exab6 == 1)

transfer %>% 
  count(condition, img_x, img_y, response) %>% 
  spread(response, n) %>% 
  mutate(
    extra = ifelse(subj_id %in% extra_pols$subj_id, "Extrapolators", "Proximators"),
    Nobz = ifelse(is.na(Nobz), 0, Nobz),
    Grot = ifelse(is.na(Grot), 0, Grot),
    n = Nobz + Grot,
    p_Grot = Grot / n
  ) %>% 
  ggplot(aes(img_x, img_y, fill = p_Grot)) +
  facet_wrap(~condition) +
  geom_tile(size = .5, color = "white") +
  # scale_fill_brewer(palette = "Set1") +
  scale_fill_viridis_c() +
  theme_subjects




#### Response Times ----
## rts, outliers, sowas
e_nobz <- transfer %>% 
  count(subj_id, response) %>% 
  filter(response == "Grot", n <= 3)

rts <- transfer %>% 
  group_by(subj_id) %>% 
  summarise(
    mean_rt = mean(response_time),
    median_rt = median(response_time),
    sd_rt = sd(response_time),
    ymin = mean_rt - sd_rt / sqrt(n()),
    ymax = mean_rt + sd_rt / sqrt(n())
  )

rts %>% 
  mutate(
    xtremes = ifelse(subj_id %in% e_nobz$subj_id, "xt", "norm")
  ) %>% 
  ggplot(aes(subj_id, color = xtremes)) +
  geom_pointrange(aes(y = mean_rt, ymin = ymin, ymax = ymax))
