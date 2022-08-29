source("init.R")
# demo <- read_csv("data-clean/demographics.csv")
# training <- read_csv("data-clean/trials-training.csv")
# transfer <- read_csv("data-clean/trials-transfer.csv")
demo <- readRDS("data-clean/demographics.rds")
training <- readRDS("data-clean/trials-training.rds")
transfer <- readRDS("data-clean/trials-transfer.rds")
stimprob <- readRDS("data-clean/trials-probability.rds")

exctest <- training %>% 
  filter(block > 9) %>% 
  group_by(submission_id) %>% 
  summarise(
    n = n(),
    k = sum(correct),
    p = k / n
  )

sumstats <- training %>% 
  group_by(submission_id) %>% 
  summarise(
    accuracy = mean(correct),
    rt_mean  = mean(response_time),
    rt_sd    = sd(response_time)
  )

## training accuracy between groups
training %>% 
  group_by(condition, submission_id, block) %>% 
  summarise(
    accuracy = mean(correct),
    rt_mean  = mean(response_time),
    rt_sd    = sd(response_time)
  ) %>% 
  ungroup() %>% 
  group_by(condition, block) %>% 
  summarise(
    acc_mean = mean(accuracy),
    acc_sd   = sd(accuracy),
    acc_hi   = acc_mean + acc_sd / sqrt(n()),
    acc_lo   = acc_mean - acc_sd / sqrt(n()),
    rt_mean  = mean(rt_mean),
    rt_sd    = sd(rt_sd)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(block, acc_mean, color = condition, group = condition)) +
  geom_line(position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), size = 2) +
  geom_linerange(aes(ymax = acc_hi, ymin = acc_lo),
                 position = position_dodge(0.3)) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12))

## learning curves
## ...or so I thought, pretty darn uninteresting :c
training %>% 
  group_by(submission_id, condition) %>% 
  summarise(
    cmsm = cumsum(correct)
  ) %>% 
  mutate(
    trial = seq_along(cmsm)
  ) %>% 
  ungroup() %>% 
  group_by(condition, trial) %>% 
  summarise(
    acc = mean(cmsm)
  ) %>% 
  ggplot(aes(trial, acc, color = condition)) +
  geom_line()



## histogram of proximations & extrapolations
transfer %>% 
  filter(item == "transfer") %>% 
  group_by(submission_id) %>% 
  summarise(ext = sum(extrapol)) %>% 
  ggplot(aes(ext)) +
  geom_histogram(fill = "#f0f0f0", color = "#13191C", binwidth = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))


## somehow this now somewhat works -> p_nobz!
cat_labs <- tibble(
  img_x = c(1, 1, 2, 2, 6, 7),
  img_y = c(1, 7, 2, 6, 6, 7),
  categ = c("Grot", "Nobz", "Grot", 
            "Nobz", "Grot", "Grot")
)

transfer %>% 
  mutate(
    k_nobz = ifelse(response == "Nobz", 1, 0)
  ) %>%
  group_by(condition, assignment, img_x, img_y) %>% 
  summarise(
    p_nobz = mean(k_nobz)
  ) %>% 
  mutate(
    p_all = ifelse(assignment == "correct1", 1 - p_nobz, p_nobz)
  ) %>% 
  ggplot(aes(img_x, img_y, fill = p_all)) +
    facet_wrap(~condition, nrow = 2) +
    geom_tile(color = "#F0F0F0", size = 1) +
    geom_label(data = cat_labs, aes(label = categ, fill = NULL), color = "black") +
    labs(fill = "Nobz") +
    scale_fill_viridis_c() + 
    theme_void(12) +
    theme(legend.text = element_blank())
          # legend.position = "top")


## 'Nobz' percentages for trained categories?
transfer %>% 
  filter(item == "training") %>% 
  mutate(
    nobz = ifelse(response == "Nobz", 1, 0)
  ) %>% 
  group_by(condition, assignment, img_x, img_y) %>% 
  summarise(
    p = mean(nobz)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(img_x, img_y, fill = p)) +
    facet_grid(assignment ~ condition) +
    geom_tile(color = "#F0F0F0", size = .5) +
    labs(fill = "Nobz") +
    scale_fill_viridis_c() +
    theme_transfer


## plausibility & sanity check: didn't they learn anything?
transfer %>% 
  filter(item == "training") %>% 
  group_by(submission_id) %>% 
  summarise(
    n = n(),
    k = sum(correct),
    p = k / n
  ) %>% 
  filter(p < .7) %>% 
  nrow()


## shrug
acc_trans <- transfer %>% 
  filter(item == "training") %>% 
  group_by(submission_id, condition) %>% 
  summarise(
    n = n(),
    k = sum(correct),
    p = k / n
  )

acc_corr <- sumstats %>% 
  left_join(acc_trans, by = "submission_id") %>% 
  select(submission_id, condition, accuracy, p) %>% 
  rename("training" = accuracy, "transfer" = p)

ggplot(acc_corr, aes(training, transfer)) +
  facet_wrap(~condition, nrow = 2) +
  geom_abline(intercept = 0, slope = 1, color = "saddlebrown") +
  geom_jitter(height = .02, width = .02, shape = 23,
              color = "black", fill = "#F0F0F0")


## probabilities
stimprob %>%  
  mutate(
    unified = ifelse(assignment == "correct2", probA, probB)
  ) %>% 
  group_by(img_x, img_y) %>% 
  summarise(
    p = mean(unified)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(img_x, img_y, fill = p)) +
    # facet_grid(assignment ~ condition) +
    geom_tile(color = "#F0F0F0", size = .5) +
    geom_label(
      # data = cat_labs, aes(label = categ, fill = NULL), 
      aes(label = round(p, 2)),
      color = "black", fill = "#F0F0F0") +
    labs(
      subtitle = "Wahrscheinlichkeitsschätzung der Kategorienzugehörigkeit",
      fill = "Nobz") +
    scale_fill_viridis_c() +
    theme_transfer
