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
