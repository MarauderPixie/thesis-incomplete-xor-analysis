#### dummy-testing parts of the BF-workflow ----
# specify models like in the paper
mod0 <- brm(correct ~ 1 + (1+condition|subj), data = simt,
            family = bernoulli(), cores = ncore,
            save_pars = save_pars(all = TRUE),
            warmup=2000, iter=10000,
            control=list(adapt_delta=0.9, max_treedepth=12))
mod1 <- brm(correct ~ 1 + condition + (1+condition|subj), data = simt,
            family = bernoulli(), cores = ncore,
            save_pars = save_pars(all = TRUE),
            warmup=2000, iter=10000,
            control=list(adapt_delta=0.9, max_treedepth=12))

saveRDS(mod0, "models/paper0.rds")
saveRDS(mod1, "models/paper1.rds")

# (naively) get BFs
bayes_factor(mod1, mod0)


# folllow the paper: bridge-sample first, then BF
lml0 <- bridge_sampler(mod0)
lml1 <- bridge_sampler(mod1)

bayes_factor(lml1, lml0)




#### better yet: closer to the real thang ----
# these bits need srs revisioning
bm0 <- brm(k|trials(n) ~ 1 + (1|subj), data = full_dat,
           family = binomial(), cores = ncore,
           save_pars = save_pars(all = TRUE),
           warmup = 1500, iter = 6000,
           control=list(adapt_delta=0.9))

bm1 <- brm(k|trials(n) ~ 1 + condition + (1|subj), data = full_dat,
           family = binomial(), cores = ncore,
           save_pars = save_pars(all = TRUE),
           warmup = 1500, iter = 6000,
           control=list(adapt_delta=0.9))

bml0 <- bridge_sampler(bm0)
bml1 <- bridge_sampler(bm1)

bayes_factor(bml1, bml0)


bm2 <- brm(k|trials(n) ~ 1 + condition + block + (1 + block | subj), data = full_dat,
           family = binomial(), cores = ncore,
           save_pars = save_pars(all = TRUE),
           warmup = 1500, iter = 6000,
           control=list(adapt_delta=0.9))

bml2 <- bridge_sampler(bm2)

bayes_factor(bml2, bml1)

### IDEA:
# instead of comparing models only containing data from condition(s) A, B, C - that is:
# data_rll = filter(c("A", "B") %in% condition)
# data_srf = filter(c("A", "C") %in% condition)
# rather keep all data and specify null model to compare to? e.g. brm(y ~ 1 + (1 | subj), data)
# ...doesn't help with differentiating h1 from h2 tho /o\
mod_test <- brm(k|trials(n) ~ 1 + condition + block + (1 | subj), 
                data = full_dat,
                family = binomial(), cores = ncore,
                save_pars = save_pars(all = TRUE),
                iter = 3000,
                control=list(adapt_delta=0.9))

mod_test2 <- brm(k|trials(n) ~ condition + block + (1 | subj), 
                 data = full_dat,
                 family = binomial(), cores = ncore,
                 save_pars = save_pars(all = TRUE),
                 iter = 3000,
                 control=list(adapt_delta=0.9))

## welp; results are virtually the same, might as well scrap that first "1 +"

dat_AB <- full_dat %>% filter(condition %in% c("A", "B"))
dat_AC <- full_dat %>% filter(condition %in% c("A", "C"))
# dat_rll <- full_dat %>% filter(condition == "B")
# dat_srf <- full_dat %>% filter(condition == "C")

mod_AB <- brm(k|trials(n) ~ condition + block + (1 | subj), 
              data = dat_AB, iter = 3000,
              family = binomial(), cores = ncore,
              save_pars = save_pars(all = TRUE),
              control=list(adapt_delta=0.9))

mod_AC <- brm(k|trials(n) ~ condition + block + (1 | subj), 
              data = dat_AC, iter = 3000,
              family = binomial(), cores = ncore,
              save_pars = save_pars(all = TRUE),
              control=list(adapt_delta=0.9))
##### fun (and soothing) fact:
# single condition data (without a contrast, that is) do not even work
# mod_rll <- brm(k|trials(n) ~ condition + block + (1 | subj), 
#                data = dat_rll, iter = 3000,
#                family = binomial(), cores = ncore,
#                save_pars = save_pars(all = TRUE),
#                control=list(adapt_delta=0.9))
# 
# mod_srf <- brm(k|trials(n) ~ condition + block + (1 | subj), 
#                data = dat_srf, iter = 3000,
#                family = binomial(), cores = ncore,
#                save_pars = save_pars(all = TRUE),
#                control=list(adapt_delta=0.9))

ml_AB <- bridge_sampler(mod_AB)
ml_AC <- bridge_sampler(mod_AC)

bayes_factor(ml_AB, ml_AC)
bayes_factor(mod_AB, mod_AC) # soo... if the models haven't been run through the bridge sampler,
                             # bayes_factor() will just do that for us - nice to know, I guess!

#### about the real specification:
# we want block to be able to vary by subjects and by conditions
# we also expet an interaction of blocks and condition
# so a possible formula for my final model might be:
# k|trials(n) ~ condition * block + (condition + block || subj)
# (slopes and intercepts of condition and block shouldn't be correlated, 
#  but I'm not at all too sure about that - the '||' simplifies the model though)
#
# so let's try that:
mod_full_AB <- brm(data = dat_AB,  
  k|trials(n) ~ condition * block + (condition + block || subj),
  family = binomial(), cores = ncore,
  iter = 10000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

mod_full_AC <- brm(data = dat_AC, 
  k|trials(n) ~ condition * block + (condition + block || subj),
  family = binomial(), cores = ncore,
  iter = 10000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)


bayes_factor(mod_full_AC, mod_full_AB)


## and just for the lols:
## (question even here is: test this for all conditions? each on it's own? and then BF every comparison?)
mod_full_null <- brm(data = dat_AB, 
  k|trials(n) ~ condition * block + (condition + block || subj),
  family = binomial(), cores = ncore,
  iter = 10000, warmup = 2000,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)
# intuitively it makes sense to compute BFs for every subset against their respective null-model
# and then compute BFs for models which result in some (or more than anecdotic) evidence