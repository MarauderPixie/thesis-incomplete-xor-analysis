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

# (naively) get BFs
bayes_factor(mod1, mod0)


# folllow the paper: bridge-sample first, then BF
lml0 <- bridge_sampler(mod0)
lml1 <- bridge_sampler(mod1)

bayes_factor(lml1, lml0)