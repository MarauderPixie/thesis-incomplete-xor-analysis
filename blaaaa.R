mb1 <- brm(mpg ~ disp, data = x, save_pars = save_pars(all = TRUE))
mb2 <- brm(mpg ~ wt, data = x, save_pars = save_pars(all = TRUE))

bayes_factor(mb2, mb1)
# Estimated Bayes factor in favor of mb2 over mb1: 872.91260

lm(correct ~ condition + block + (1 + block | subj), 
   data = simdat, family = bernoulli())


simdat %>% 
  group_by(condition, subj) %>% 
  summarise(
    trial = seq(subj),
    cumcorr = cumsum(correct)
  ) %>% 
  ggplot(aes(trial, cumcorr, group = subj, color = condition)) +
    geom_line(size = .4)

q1 <- "conditionD > Intercept"
q4 <- "conditionD + block4 + block5 + block6 + block7 + block8 + block9 + block10 + block11 + block12 > Intercept + block4 + block5 + block6 + block7 + block8 + block9 + block10 + block11 + block12"
brms::hypothesis(m1, q1) %>% plot()
brms::hypothesis(m1, q4) %>% plot()

fuller_mod <- brm(data = mutate(simdat, block = as.numeric(block)),  
                  correct ~ condition * block + (1 + image|subj), 
                  family = bernoulli, iter = 4000, # 2000 -> low ESS, 4k seem fine
                  save_pars = save_pars(all = TRUE))
