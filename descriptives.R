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
