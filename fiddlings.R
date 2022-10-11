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
  group_by(subj_id) %>% 
  summarise(
    n = n(),
    k = sum(correct),
    p = k / n
  ) %>% 
  filter(p < .7)

demo <- demo %>% 
  filter(!(subj_id %in% exctest$subj_id))
training <- training %>% 
  filter(!(subj_id %in% exctest$subj_id))
transfer <- transfer %>% 
  filter(!(subj_id %in% exctest$subj_id))
stimprob <- stimprob %>% 
  filter(!(subj_id %in% exctest$subj_id))

dur <- quantile(demo$duration, c(.025, .95)) |> as.numeric()

demo %>% 
  filter(between(duration, dur[1], dur[2])) %>% 
  gather(var, val, age, duration) %>% 
  group_by(var) %>% 
  summarise(
    min    = min(val),
    mean   = mean(val),
    median = median(val),
    max    = max(val),
    sd     = sd(val)
  )

sumstats <- training %>% 
  group_by(subj_id) %>% 
  summarise(
    accuracy = mean(correct),
    rt_mean  = mean(response_time),
    rt_sd    = sd(response_time)
  )

## training accuracy between groups
training %>% 
  group_by(condition, subj_id, block) %>% 
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

## sorta replica of zhe paper
training %>% 
  group_by(subj_id, rules, blocked, block) %>% 
  summarise(
    accuracy = mean(correct),
    acc_sd   = sd(correct),
    rt_mean  = mean(response_time),
    rt_sd    = sd(response_time)
  ) %>% 
  mutate(
    block = as_factor(block),
    tick = rep(1:6, each = 2) |> as_factor()
  ) %>% 
  ungroup() %>% 
  group_by(subj_id, rules, blocked, tick) %>% 
  summarise(
    accuracy = mean(accuracy),
    acc_sd   = mean(acc_sd),
    rt_mean  = mean(rt_mean),
    rt_sd    = mean(rt_sd)
  ) %>% 
  ggplot(aes(tick, accuracy, fill = rules)) +
    geom_half_violin(side = "r") +
    geom_half_dotplot(binwidth = .125, dotsize = .125)


## funny; how to deal with RT outliers?
train_subj <- training %>% 
  group_by(subj_id) %>% 
  mutate(
    block = as_factor(block),
    tick = rep(1:6, each = 16) |> as_factor()
  ) %>% 
  filter(response_time < quantile(response_time, .95)) %>% 
  group_by(subj_id, rules, blocked, tick) %>% 
  summarise(
    accuracy = mean(correct),
    acc_sd   = sd(correct),
    rt_mean  = mean(response_time),
    rt_sd    = sd(response_time)
  ) %>% 
  ungroup()

train_aggr <- train_subj %>% 
  group_by(tick, rules, blocked) %>% 
  summarise(
    n = n(),
    accuracy = mean(accuracy),
    acc_sd   = sd(acc_sd),
    acc_se_hi = accuracy + acc_sd / sqrt(n), 
    acc_se_lo = accuracy - acc_sd / sqrt(n), 
    acc_ci_hi = accuracy + qt(.975, n-1) * (acc_sd / sqrt(n)),
    acc_ci_lo = accuracy - qt(.975, n-1) * (acc_sd / sqrt(n)),
    rt_mean  = mean(rt_mean),
    rt_sd    = sd(rt_sd),
    rt_se_hi = rt_mean + rt_sd / sqrt(n), 
    rt_se_lo = rt_mean - rt_sd / sqrt(n), 
    rt_ci_hi = rt_mean + qt(.975, n-1) * (rt_sd / sqrt(n)),
    rt_ci_lo = rt_mean - qt(.975, n-1) * (rt_sd / sqrt(n)),
  ) %>% 
  ungroup()
  

ggplot() +
  geom_half_violin(data = train_subj, color = NA,
                   aes(tick, rt_mean, fill = rules),
                   side = "r") +
  geom_pointrange(data = train_aggr,
                  aes(tick, rt_mean, color = rules,
                      ymax = rt_se_hi, ymin = rt_se_lo),
                  position = position_dodge(.5)) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

  

## learning curves
## ...or so I thought, pretty darn uninteresting :c
training %>% 
  group_by(subj_id, condition) %>% 
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
  ggplot(aes(acc, log(trial), color = condition)) +
    geom_line()



## histogram of proximations & extrapolations
transfer %>% 
  filter(item == "transfer") %>% 
  group_by(subj_id) %>% 
  summarise(ext = sum(extrapolation)) %>% 
  ggplot(aes(ext)) +
  geom_histogram(fill = "#3c4c72", color = "#f0f0f0", binwidth = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))


## somehow this now somewhat works -> p_nobz!
cat_labs <- tibble(
  img_x = c(1, 1, 2, 2, 6, 7),
  img_y = c(1, 7, 2, 6, 6, 7),
  categ = c("Nobz", "Grot", "Nobz", 
            "Grot", "Nobz", "Nobz")
)

transfer %>% 
  mutate(
    k_nobz = ifelse(response == "Grot", 1, 0)
  ) %>%
  group_by(subj_id) %>% 
  mutate(
    extraprox = ifelse(sum(extrapolation, na.rm = T) > 4, 
                       "extra", "proxy")
  ) %>% 
  ungroup() %>% 
  group_by(extraprox, condition, img_x, img_y) %>% 
  summarise(
    p_nobz = mean(k_nobz)
  ) %>% 
  ggplot(aes(img_x, img_y, fill = p_nobz)) +
    facet_wrap(extraprox~condition, nrow = 2) +
    geom_tile(color = "#F0F0F0", size = 1) +
    geom_label(data = cat_labs, aes(label = categ, fill = NULL), color = "black") +
    labs(fill = "Grot") +
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
  group_by(condition, img_x, img_y) %>% 
  summarise(
    p = mean(nobz)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(img_x, img_y, fill = p)) +
    facet_grid(~condition) +
    geom_tile(color = "#F0F0F0", size = .5) +
    labs(fill = "Nobz") +
    scale_fill_viridis_c() +
    theme_transfer


## plausibility & sanity check: didn't they learn anything?
transfer %>% 
  filter(item == "training") %>% 
  group_by(subj_id) %>% 
  summarise(
    n = n(),
    k = sum(correct),
    p = k / n
  ) %>% 
  filter(p < .7) %>% 
  nrow()


## cross tableing / do it like C&K
catype <- transfer %>%
  filter(item == "transfer") %>% 
  group_by(subj_id, condition) %>% 
  summarise(
    k = sum(extrapolation),
    type = ifelse(k > 4, "extra", "proxy")
  )

table(catype$condition, catype$type)


## shrug
acc_trans <- transfer %>% 
  filter(item == "training") %>% 
  group_by(subj_id, condition) %>% 
  summarise(
    n = n(),
    k = sum(correct),
    p = k / n
  )

acc_corr <- sumstats %>% 
  left_join(acc_trans, by = "subj_id") %>% 
  select(subj_id, condition, accuracy, p) %>% 
  rename("training" = accuracy, "transfer" = p)

ggplot(acc_corr, aes(training, transfer)) +
  facet_wrap(~condition, nrow = 2) +
  geom_abline(intercept = 0, slope = 1, color = "saddlebrown") +
  geom_jitter(height = .02, width = .02, shape = 23,
              color = "black", fill = "#F0F0F0")


## probabilities
exin <- transfer %>% 
  mutate(
    k_nobz = ifelse(response == "Grot", 1, 0)
  ) %>%
  group_by(subj_id) %>% 
  mutate(
    extraprox = ifelse(sum(extrapolation, na.rm = T) > 4, 
                       "extra", "proxy")
  ) %>% 
  ungroup()

stimprob %>%
  select(-img_x, -img_y) %>% 
  left_join(exin, by = c("subj_id", "image")) %>% 
  group_by(img_x, img_y, extraprox) %>% 
  summarise(
    p = mean(prob)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(img_x, img_y, fill = p)) +
    facet_grid(~ extraprox) +
    geom_tile(color = "#F0F0F0", size = .5) +
    geom_label(
      # data = cat_labs, aes(label = categ, fill = NULL), 
      aes(label = round(p, 2)),
      color = "black", fill = "#F0F0F0") +
    labs(
      subtitle = "Wahrscheinlichkeitsschätzung der Kategorienzugehörigkeit",
      fill = "Grot") +
    scale_fill_viridis_c() +
    theme_transfer


stimprob %>%
  select(-img_x, -img_y) %>% 
  left_join(exin, by = c("subj_id", "image")) %>% 
  filter(extraprox == "extra") %>% 
  ggplot(aes(prob)) +
    facet_grid(cols = vars(img_x), 
               rows = vars(img_y),
               as.table = F) +
    geom_histogram(fill = "#3c4c72", color = "#f0f0f0", bins = 11) +
    labs(
      subtitle = "Wahrscheinlichkeitsschätzung der Kategorienzugehörigkeit",
      # fill = "Grot"
    ) +
    scale_fill_viridis_c() +
    theme_transfer +
    theme(panel.spacing = unit(5, "pt"))

stimprob %>%
  select(-img_x, -img_y) %>% 
  left_join(exin, by = c("subj_id", "image")) %>% 
  filter(extraprox == "proxy") %>% 
  ggplot(aes(prob)) +
    facet_grid(cols = vars(img_x), 
               rows = vars(img_y),
               as.table = F) +
    geom_histogram(fill = "#3c4c72", color = "#f0f0f0", binwidth = 5) +
    labs(
      subtitle = "Wahrscheinlichkeitsschätzung der Kategorienzugehörigkeit",
      # fill = "Grot"
    ) +
    scale_fill_viridis_c() +
    theme_transfer +
    theme(panel.spacing = unit(5, "pt"))



## some fun with probs & correlations n stuff
stimprob %>% 
  mutate(
    unified = ifelse(assignment == "correct2", probA, probB),
    prob_assign = case_when(
      unified > 60 ~ "A",
      unified < 40 ~ "B",
      TRUE ~ "indecisive"
    ),
    pa_uni = case_when(
      prob_assign == "A" & assignment == "correct2" ~ "Nobz",
      prob_assign == "B" & assignment == "correct2" ~ "Grot",
      prob_assign == "A" & assignment == "correct1" ~ "Grot",
      prob_assign == "B" & assignment == "correct1" ~ "Nobz",
      TRUE ~ prob_assign
    )
  ) %>% 
  left_join(transfer, by = c("subj_id", "image")) %>% 
  mutate(
    samesies = case_when(
      pa_uni == response ~ 1,
      pa_uni == "indecisive" ~ 0.5,
      pa_uni != response ~ 0
    )
  ) %>% 
  filter(item == "training") %>% 
  group_by(subj_id) %>% 
  summarise(
    m = mean(samesies)
  ) %>% 
  ggplot(aes(m)) + 
    geom_histogram(binwidth = 0.02, fill = "#3c4c72", color = "#f0f0f0") +
    labs(
      y = "Anzahl", x = "Mean Accuracy of Learned Category",
      caption = "Percentual agreement between categorical transfer responses 
      and probability transfer responses; Items with a >60 (<40) probability 
      rating were assigned to their respective label."
    ) +
    scale_x_continuous(labels = scales::percent_format(),
                       breaks = scales::pretty_breaks())



#### THIS IS A FINDING!! I guess... ---
# This plot may show that extrapolators tend to default on Grot!
# Like, their mean_grot_percentage is way higher overall, not just
# on the previously untrained category
transfer %>% 
  mutate(
    k_nobz = ifelse(response == "Grot", 1, 0)
  ) %>%
  group_by(subj_id) %>% 
  mutate(
    extraprox = ifelse(sum(extrapolation, na.rm = T) > 4, 
                       "extra", "proxy")
  ) %>% 
  ungroup() %>% 
  group_by(extraprox, img_x, img_y) %>% 
  summarise(
    p_nobz = mean(k_nobz)
  ) %>% 
  ggplot(aes(img_x, img_y, fill = p_nobz)) +
    facet_wrap(~extraprox, nrow = 1) +
    geom_tile(color = "#F0F0F0", size = 1) +
    geom_label(data = cat_labs, aes(label = categ, fill = NULL), color = "black") +
    labs(fill = "Grot") +
    scale_fill_viridis_c() + 
    theme_void(12) +
    theme(legend.text = element_blank())
