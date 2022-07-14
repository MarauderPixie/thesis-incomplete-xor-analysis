tibble(
  y = rep(-3:3, times = 7) %>% as_factor(), 
  x = rep(-3:3, each = 7) %>% as_factor()
) %>% 
  ggplot(aes(x, y)) + 
  geom_point(aes(size = x, color = y), shape = "square") + 
  scale_color_manual(values = grey(seq(0, .9, length.out = 7)))
