# demo <- read_csv("data-clean/demographics.csv")
# training <- read_csv("data-clean/trials-training.csv")
# transfer <- read_csv("data-clean/trials-transfer.csv")
demo <- readRDS("data-clean/demographics.rds")
training <- readRDS("data-clean/trials-training.rds")
transfer <- readRDS("data-clean/trials-transfer.rds")

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
    scale_color_brewer(palette = "Set1")


transfer %>% 
  filter(item == "transfer") %>% 
  group_by(submission_id) %>% 
  summarise(ext = sum(extrapol)) %>% 
  ggplot(aes(ext)) +
    geom_histogram(fill = "#f0f0f0", color = "#13191C", binwidth = 1) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 9))


transfer %>% 
  mutate(
    norm_response = case_when(
      assignment == "corect2" & response == "Nobz" ~ "Grot",
      assignment == "corect2" & response == "Grot" ~ "Nobz",
      TRUE ~ correct1
    )
  ) %>% 
  
