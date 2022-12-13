library(brms)
h1_inter   <- readRDS("models/h1_transfer_re/h132.rds")

no_rules <- update(h1_inter, cores = ncore, formula = ~. -rules -blocked:rules)
no_block <- update(h1_inter, cores = ncore, formula = ~. -blocked -blocked:rules)
no_inter <- update(h1_inter, cores = ncore, formula = ~. -blocked:rules)

bayes_factor(no_block, h1_inter)
bayes_factor(no_rules, h1_inter)
bayes_factor(no_inter, h1_inter)


# or in short:
bayestestR::bayesfactor_models(h1_inter, no_block, no_rules, no_inter)


