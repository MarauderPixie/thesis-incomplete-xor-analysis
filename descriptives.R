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


beobachtet <- extra_binom %>% 
  group_by(rules, blocked) %>% 
  summarise(
    mean_p = mean(p),
    sd_p   = sd(p),
    mean_k = mean(k),
    sd_k   = sd(k),
    mean_ckab6 = mean(exab6),
    mean_ckab5 = mean(exab5),
    .groups = "drop"
  )

#### Plots ----
## Accuracy b/w nassing & rules
tr_plot_data <- training %>%  
  filter(condition %in% c("control", "rules")) %>% 
  group_by(rules, block, subj_id) %>% 
  summarise(
    n = n(),
    k = sum(correct),
    accuracy = mean(correct),
    .groups  = "drop_last"
  ) %>% 
  summarise(
    n = sum(n),
    k = sum(k),
    acc_mean = mean(accuracy),
    acc_hi   = Hmisc::binconf(k, n, return.df=TRUE)$Upper,
    acc_lo   = Hmisc::binconf(k, n, return.df=TRUE)$Lower,
    .groups  = "drop"
  ) 

ggplot(tr_plot_data, aes(block, acc_mean, fill = rules, color = rules, group = rules)) +
    gghalves::geom_half_violin(data = filter(tr_plot_data, rules == "yes", aes())) +
    gghalves::geom_half_violin() +
    geom_line(position = position_dodge(0.3)) +
    # geom_smooth(se = F, lty = "dotted", method = 'loess') +
    geom_point(position = position_dodge(0.3), size = 3, shape = 21, color = "#f0f0f0") +
    geom_linerange(aes(ymax = acc_hi, ymin = acc_lo),
                   position = position_dodge(0.3), size = 1.2) +
    labs(x = "Training Block", y = "Mean Accuracy",
         color = "Rule Instructions:", fill = "Rule Instructions:") +
    scale_color_manual(values = c("#4d4d4d", "#ac0634")) +
    scale_fill_manual(values = c("#4d4d4d", "#ac0634")) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
    scale_y_continuous(labels = scales::label_percent()) +
    theme(legend.position = "top",
          panel.grid.minor.x = element_blank())



## accuracy per picture per person
training <- transfer %>%
  select(subj_id, image, img_x, img_y) %>% 
  right_join(training, by = c("subj_id", "image"))

excluded <- transfer %>%
  select(image, img_x, img_y) %>% 
  right_join(excluded, by = "image")

training %>% 
  filter(block > 9) %>% 
  group_by(subj_id, img_x, img_y) %>% 
  summarise(
    p_correct = round(mean(correct), 2),
    acc_state = ifelse(p_correct < 0.7, "critical", "fine"),
    img_x = ifelse(img_x > 2, img_x - 2, img_x),
    img_y = ifelse(img_y > 2, img_y - 2, img_y),
    .groups = "drop"
  ) %>% 
  distinct() %>% 
  ggplot(aes(img_x, img_y, fill = p_correct)) +
  facet_wrap(~subj_id) +
  geom_tile(size = .5, color = "white") +
  # geom_text(color = "black", size = 4, aes(label = p_correct)) +
  scale_fill_viridis_c(option = "E") +
  # scale_fill_brewer(palette = "Set1") +
  theme_subjects


# condensed blocks
trainvis <- training %>% 
  group_by(subj_id, condition, rules, blocked, block) %>% 
  summarise(
    k = sum(correct),
    n = n(),
    accuracy = mean(correct),
    acc_sd   = sd(correct),
    rt_mean  = mean(response_time),
    rt_sd    = sd(response_time)
  ) %>% 
  mutate(
    block = as_factor(block),
    tick  = rep(1:6, each = 2) |> as_factor()
  ) %>% ungroup()

trainvis %>% 
  group_by(condition, tick) %>% 
  summarise(
    acc_mean = mean(accuracy),
    acc_sd   = mean(acc_sd),
    acc_min  = acc_mean - 1.97 * (acc_sd / sqrt(n())),
    acc_max  = acc_mean + 1.97 * (acc_sd / sqrt(n()))
  ) %>% 
  ggplot(aes(tick, acc_mean, color = condition, group = condition)) +
    geom_line(position = position_dodge(0.3), alpha = .3) +
    geom_point(position = position_dodge(0.3), size = 2) +
    geom_linerange(aes(ymax = acc_max, ymin = acc_min),
                   position = position_dodge(0.3)) +
    scale_color_brewer(palette = "Set1") +
    scale_x_discrete(breaks = scales::pretty_breaks(n = 6))





## it's time (pause) to take a Closer Look
## not at all arbitrarily chosen ofc, no Sir
# oddballs <- c("147", "155", "168", "178", "181", "195",
#               "207", "246", "260", "296", "338", "350")
learnd <- excluded %>% 
  filter(block > 9) %>% 
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
  # filter(subj_id %in% oddballs) %>% 
  mutate(
    strip = paste0(subj_id, " (", condition, ")")
  ) %>% 
  ggplot(aes(img_x, img_y, fill = response)) +
    facet_wrap(~subj_id) +
    geom_tile(size = .5, color = "white") +
    geom_tile(data = excluded, size = .5, color = "white") +
    # geom_label(data = learnd, aes(label = p_correct, fill = NULL)) +
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
    # extra = ifelse(subj_id %in% extra_pols$subj_id, "Extrapolators", "Proximators"),
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

## and by extrapolators 
transfer %>% 
  mutate(extra = ifelse(subj_id %in% extra_pols$subj_id, "Extrapolators", "Proximators")) %>% 
  count(extra, img_x, img_y, response) %>% 
  spread(response, n) %>% 
  mutate(
    Nobz = ifelse(is.na(Nobz), 0, Nobz),
    Grot = ifelse(is.na(Grot), 0, Grot),
    n = Nobz + Grot,
    p_Grot = Grot / n
  ) %>% 
  ggplot(aes(img_x, img_y, fill = p_Grot)) +
    facet_grid(~extra) +
    geom_tile(size = .5, color = "white") +
    # scale_fill_brewer(palette = "Set1") +
    scale_fill_viridis_c() +
    theme_subjects

## and by extrapolators x condition
transfer %>% 
  mutate(extra = ifelse(subj_id %in% extra_pols$subj_id, "Extrapolators", "Proximators")) %>% 
  count(condition, extra, img_x, img_y, response) %>% 
  spread(response, n) %>% 
  mutate(
    Nobz = ifelse(is.na(Nobz), 0, Nobz),
    Grot = ifelse(is.na(Grot), 0, Grot),
    n = Nobz + Grot,
    p_Grot = Grot / n
  ) %>% 
  ggplot(aes(img_x, img_y, fill = p_Grot)) +
    facet_grid(extra~condition) +
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
