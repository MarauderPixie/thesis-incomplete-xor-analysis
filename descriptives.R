#### Extrapolations ----
extra_all <- filter(transfer, item == "transfer")
extra_tbl <- extra_all %>% 
  group_by(subj_id, condition) %>% 
  summarise(
    k = sum(extrapolation),
    n = n(),
    p = k / n
  ) %>% 
  ungroup() %>% 
  mutate(
    exab4 = ifelse(k > 3, 1, 0),
    exab5 = ifelse(k > 4, 1, 0),
    exab6 = ifelse(k > 5, 1, 0)
  )

tbl2 <- extra_tbl %>% 
  gather(cutoff, n, starts_with("exab")) %>% 
  filter(n != 0)

extra_tbl %>% 
  group_by(condition) %>% 
  summarise(
    mean_p = mean(p),
    mean_k = mean(k)
  )

table(tbl2$condition, tbl2$cutoff) # |> prop.table(margin = 2) |> round(2)
