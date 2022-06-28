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
    p = k / n,
    ext = ifelse(k > 5, 1, 0)
  ) %>% ungroup()

priors1 <- c(
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

# some priors of my own devise
tmod2 <- brm(data = critems,
             k|trials(n) ~ condition,
             family = binomial(), prior = priors2,
             cores = ncore, iter = 10000, warmup = 2000,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE)
)

tmod3 <- brm(data = critems,
             k|trials(n) ~ condition,
             family = binomial(), prior = priors3,
             cores = ncore, iter = 10000, warmup = 2000,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE)
)

tmod4 <- brm(data = critems,
             k|trials(n) ~ condition,
             family = binomial(), prior = priors4,
             cores = ncore, iter = 10000, warmup = 2000,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE)
)

# some priors as "recommended" by gelman
# // prior sd based on se, that is
priors2 <- c(
  set_prior("normal(-1, 0.32)", class = "Intercept"), 
  set_prior("normal(0, 0.5)", class = "b")
)

priors3 <- c(
  set_prior("normal(-1, 0.32)", class = "Intercept"), 
  # set_prior("normal(0, 0.5)", class = "b"),
  set_prior('normal(.83, 0.43)', coef = 'conditionB'),
  set_prior('normal(1.05, 0.43)', coef = 'conditionC'),
  set_prior('normal(1.78, 0.44)', coef = 'conditionD')
)

priors4 <- c(
  set_prior("student_t(5, 0, .32)", class = "Intercept"), 
  # set_prior("normal(0, 0.5)", class = "b"),
  set_prior('student_t(5, 0, .43)', coef = 'conditionB'),
  set_prior('student_t(5, 0, .43)', coef = 'conditionC'),
  set_prior('student_t(5, 0, .44)', coef = 'conditionD')
)

priors5 <- c(
  set_prior("normal(-1, 0.32)", class = "Intercept"), 
  set_prior("normal(0, 0.5)", class = "b")
)

## compare to descriptive odds
table(simg$condition, simg$response)
( 624/2450) / (1826/2450) |> log() # odds of extrapolation when in A
(937 /2450) / ( 714/2450) |> log() # odds of extrapolation when in B
(1098/1352) / ( 624/1826) |> log() # odds of extrapolation in B over A


## intbla
m_int <- brm(data = critems,
             k|trials(n) ~ 1,
             family = binomial(), 
             cores = ncore, iter = 10000, warmup = 2000,
             control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE)
)

mod5 <- brm(data = critems,
            k|trials(n) ~ condition,
            family = binomial(), prior = priors5,
            cores = ncore, iter = 10000, warmup = 2000,
            control = list(adapt_delta = 0.9),
            save_pars = save_pars(all = TRUE)
)

mod5_re <- brm(data = critems,
               k|trials(n) ~ condition + (1 + condition || subj),
               family = binomial(), prior = priors5,
               cores = ncore, iter = 10000, warmup = 2000,
               control = list(adapt_delta = 0.9),
               save_pars = save_pars(all = TRUE)
)

berndat <- simg %>% 
  filter(image == "critical")

mod5_bern <- brm(
  data = berndat,
  response ~ condition + (1 + condition || subj),
  family = bernoulli(), prior = priors5,
  cores = ncore, iter = 10000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)


bayes_factor(mod5, m_int)


#### do it like C&K'16
mod6 <- brm(data = critems,
            ext ~ condition,
            family = bernoulli(), prior = priors5,
            cores = ncore, iter = 10000, warmup = 2000,
            control = list(adapt_delta = 0.9),
            save_pars = save_pars(all = TRUE)
)

#### back to percent and all ----
# estimates are log-odds, therefore:
exp(c(-.84, .42, .46))
# which gives us the (normal) odds, that is...
 .43 # intercept_only odds; 'chances of extrapolation without any experimental modulation are 0.43:1"
1.52 # odds of srf & rrl each in relation to intercept_only
1.58 # odss of srf + rrl in rel. to int.
# which means:
 .43 / (1 +  .43) # probability of extrapolation in condA/intercept_only
1.52 / (1 + 1.52) # change in probability of extrapolation of either srf & rrl
# therefore
.3 + 0.3 * 0.6 # estimated probability of extrapolation in condB/C (srf / rrl)

# in short:
odds <- fixef(mod5) %>% exp()
odds / (1 + odds)

# from SO
# https://stackoverflow.com/questions/70575292/converting-logistic-regression-output-from-log-odds-to-probability
z <- fixef(mod5)[1] + sum(fixef(mod5)[-1,] * critems[1, ])
1 / (1 + exp(-z))     # same as: 
exp(z) / (exp(z) + 1)



#### auch eine Möglichkeit: ----
# Interaction zw. rrl & srf
intact <- critems %>% 
  mutate(
    intercept = ifelse(condition == "A", 1, 0),
    rrl  = ifelse(condition == "B", 1, 0),
    srf  = ifelse(condition == "C", 1, 0),
    both = ifelse(condition == "D", 1, 0)
  )

mod7 <- brm(data = intact,
            k|trials(n) ~ rrl + srf * both,
            family = binomial(), prior = priors5,
            cores = ncore, iter = 10000, warmup = 2000,
            control = list(adapt_delta = 0.9),
            save_pars = save_pars(all = TRUE)
)
