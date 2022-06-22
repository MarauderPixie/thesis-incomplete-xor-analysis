## playing with n stuff
tdat <- simg %>% filter(
  image == "critical", 
  condition %in% c("A", "B")
  ) %>% 
  mutate(
    lbl = ifelse(response == 0, "intra", "extra"),
    condition = fct_drop(condition)
  )

tagg <- tdat %>% 
  group_by(condition, subj) %>% 
  summarize(
    k = sum(response),
    n = n(),
    p = k/n
  ) %>% ungroup()
  

gen0 <- glm(p ~ 1, family = binomial(), weights = n, data = tagg)
gen1 <- glm(p ~ condition, family = binomial(), weights = n, data = tagg)

### intercept only model 
### // overall proportion/probability of extrapolation
# odds are:
prop0 <- prop.table(table(tdat$lbl))[1]
odds0 <- prob / (1 - prob)
log(odds)
beta0 <- gen0$coefficients
beta0
# prob of (Int.) is:
exp(beta0)/(1 + exp(beta0))

### srf model
### // prob & odds of extrapolation when exposed to srf
t1 <- table(tdat$lbl, tdat$condition)
t1

# odds for extra are:
odds1a <- (121/450) / (329/450) # in condA
odds1b <- (206/450) / (244/450) # in condB
# ratio of odds for condB to odds for condA
odds1ab <- (206/244) / (121/329)
c(odds1a, odds1b, odds1ab)
log(c(odds1a, odds1b, odds1ab))

beta1 <- gen1$coefficients
beta1      # log-odds
exp(beta1) # odds

# and in reverse, prob is:


