tibble(
  "(3, 6)" = rbeta(10000, 3, 6),
  "(4, 6)" = rbeta(10000, 4, 6),
  "(5, 6)" = rbeta(10000, 5, 6),
  "(5, 4)" = rbeta(10000, 4, 5),
  "(6, 3)" = rbeta(10000, 4, 5),
  "(3, 5)" = rbeta(10000, 3, 5),
  "(3, 4)" = rbeta(10000, 3, 4),
  "(3, 3)" = rbeta(10000, 3, 3),
  "(4, 5)" = rbeta(10000, 4, 5),
  "(5, 5)" = rbeta(10000, 5, 5),
  "(6, 5)" = rbeta(10000, 6, 5)
) %>% 
  gather(betas, y) %>% 
  group_by(betas) %>% 
  summarise(
    min = min(y),
    med = median(y),
    mean = mean(y),
    max = max(y),
    sd = sd(y)
  ) %>% 
  ggplot(aes(betas, mean)) +
    geom_hline(size = .7, color = "red", lty = "dashed", aes(yintercept = .3)) +
    geom_hline(size = .7, color = "red", lty = "dashed", aes(yintercept = .45)) +
    geom_point(size = 2) +
    geom_linerange(aes(ymin = mean + sd * -1, ymax = mean + sd)) +
    coord_flip()