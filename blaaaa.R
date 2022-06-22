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


p_subj_t <- simt %>%
  group_by(condition, block, subj) %>%
  summarise(
    k = sum(correct),
    n = n(),
    p = k/n
  ) %>% 
  ungroup()

## sth sth lme4
library(lme4)
lme4 <- glmer(p ~ condition + block + (block|subj), family = binomial(), data = p_by_subj)
brms <- brm(p ~ condition + block + (block|subj), family = binomial(), data = p_by_subj)


## sth sth correlations
p_subj_g <- simg %>%
  group_by(condition, subj) %>%
  summarise(
    k = sum(response),
    n = n(),
    p = k/n
  ) %>% 
  ungroup()







#### important notes I guess!
#
# wenn ich 'k|n ~ bla, family = binomial' rechne, 
# habe ich genau eine beobachtung pro person pro block 
# -> ist block als RE dann noch sinnvoll?

#### ----
## anyhoo, here's sanity checking assupmtions! 
mod_all <- readRDS("models/srs_full.rds")
mod_rll <- readRDS("models/srs_rll.rds")
mod_srf <- readRDS("models/srs_srf.rds")

bayes_factor(mod_srf, mod_rll)

q0 <- "conditionB < conditionC"
q1 <- "Intercept + conditionB < Intercept + conditionC"
q2 <- "Intercept + conditionB + block < Intercept + conditionC + block"
q3 <- "conditionB:block < conditionC:block"
q4 <- "Intercept + conditionB:block > Intercept + conditionC:block"

hypothesis(mod_all, q0)
hypothesis(mod_all, q1)
hypothesis(mod_all, q2)
hypothesis(mod_all, q3)
hypothesis(mod_all, q4)


## sanity check
q1_rll <- "Intercept + block < conditionC + block + conditionC:block"
q1_srf <- "Intercept + block < conditionB + block + conditionB:block"

hypothesis(mod_rll, q1_rll)
hypothesis(mod_srf, q1_srf)




## sth sth priors... ----
priors_default <- get_prior(data = mutate(full_aggr, block = as.numeric(block)),
                            k|trials(n) ~ condition * block + (1 + block || subj), 
                            family = binomial)

priors <- c(
  set_prior("beta(2, 4)", class = "Intercept"), 
  set_prior('normal(0.14, 0.1)', coef = 'block'),
  set_prior('normal(-0.93, 0.5)', coef = 'conditionB'),
  set_prior('normal(0.16, 0.1)', coef = 'conditionB:block'),
  set_prior('normal(-0.65, 0.5)', coef = 'conditionC'),
  set_prior('normal(0.11, 0.1)', coef = 'conditionC:block'),
  set_prior('normal(-1.53, 0.5)', coef = 'conditionD'),
  set_prior('normal(0.26, 0.1)', coef = 'conditionD:block')
)

mod_default <- readRDS("models/srs_full.rds")

mod_priors <- brm(data = mutate(full_aggr, block = as.numeric(block)),
                  k|trials(n) ~ condition * block + (1 + block || subj), 
                  family = binomial, cores = ncore, 
                  iter = 4000, prior = priors, 
                  save_pars = save_pars(all = TRUE))






### idk maybe explore the sim'd data maybe?
full_dat <- simt %>% 
  filter(as.numeric(block) > 2) %>% 
  group_by(subj, condition, block) %>% 
  summarise(
    k = sum(correct),
    n = n()
  ) %>% 
  mutate(
    block = as.numeric(block),
    p = k / n
  ) %>% 
  ungroup()

full_aggr <- full_dat %>% 
  group_by(condition, block) %>% 
  summarise(
    k = mean(k),
    n = mean(n),
    p = mean(p),
    sdp = sd(p)
  ) %>% 
  ungroup()

ggplot(full_dat, aes(block, p, group = subj)) +
  geom_line(alpha = .2, size = .4, 
            position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2), 
             alpha = .3, size = .8) +
  geom_line(data = full_aggr, aes(block, p, group = NA, color = condition), 
            alpha = .8, size = .8, 
            position = position_dodge(.2)) +
  geom_point(data = full_aggr, aes(block, p, group = NA, color = condition), 
             alpha = .8, size = 2,
             position = position_dodge(.2)) +
  facet_wrap(~condition)

ggplot(full_aggr, aes(condition, p, color = condition)) +
  geom_jitter(color = "#617886", alpha = .3,
              height = .3, width = .3) +
  geom_violin(alpha = .2, draw_quantiles = .5)









# transfer stuff
critems <- simg %>% 
  filter(image == "critical") %>% 
  group_by(condition, subj) %>% 
  summarise(
    k = sum(response),
    n = n(),
    p = k / n
  ) %>% ungroup()

priors <- c(
  set_prior("normal(-1.2, 0.15)", class = "Intercept"), 
  set_prior('normal(-0.92, 0.22)', coef = 'conditionB'),
  set_prior('normal(-0.92, 0.22)', coef = 'conditionC'),
  set_prior('normal(-0.51, 0.22)', coef = 'conditionD')
)

# lme4 for comparison
frmod <- glm(p ~ condition, family = binomial(), data = critems)

# no priors
tmod1 <- brm(data = critems,
             k|trials(n) ~ condition,
             family = binomial(), 
             cores = ncore, iter = 10000, warmup = 2000,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE)
)

# some priors
tmod2 <- brm(data = critems,
             k|trials(n) ~ condition,
             family = binomial(), prior = priors,
             cores = ncore, iter = 10000, warmup = 2000,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE)
)



## intbla
tmod3 <- brm(data = critems,
             k|trials(n) ~ 1,
             family = binomial(), 
             cores = ncore, iter = 10000, warmup = 2000,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE)
)
