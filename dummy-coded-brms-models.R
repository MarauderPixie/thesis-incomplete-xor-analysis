# prepwork
learn <- simt %>% 
  mutate(
    rrl  = ifelse(condition == "B" | condition == "D", 1, 0),
    srf  = ifelse(condition == "C" | condition == "D", 1, 0),
    num_block = as.numeric(block)
  )

# H2
h2 <- brm(
  data = learn,
  correct ~ rrl * num_block + (1 + image | subj) + (1 + subj | image),
  family = bernoulli(), # prior = priors5,
  cores = ncore, iter = 10000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)



# H1
# formula = k|trials(n) ~ rrl * srf + (1 | subj + item))