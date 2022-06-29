n_subj <- 200

subj  <- paste0("S", str_pad(seq(n_subj), 2, pad = "0")) %>% as_factor()
cond  <- rep(LETTERS[1:4], each = n_subj/4) %>% as_factor()
block <- as_factor(1:12)
imgs1 <- rep(paste0("img", 1:4), 2)
imgs2 <- c(paste0("img", 1:6), imgs1[1:2])
# gimgs <- paste0("img", str_pad(1:49, 2, pad = "0"))
img_cluster <- c(rep("untrained", 9), rep("equivocal", 13), rep("trained", 27))

simdat <- tibble(
  subj = rep(subj, each = 96),
  condition = rep(cond, each = 96),
  block = rep(block, each = 8, times = n_subj),
  image = c(
    replicate((n_subj/4)*12, sample(imgs2)) %>% as.vector(),
    replicate((n_subj/4), c(replicate(2, sample(imgs1)), 
                            replicate(10, sample(imgs2)))) %>% as.vector(),
    replicate((n_subj/4)*12, sample(imgs2)) %>% as.vector(),
    replicate((n_subj/4), c(replicate(2, sample(imgs1)),
                            replicate(10, sample(imgs2)))) %>% as.vector()
  ) %>% factor(),
  correct = c(
    map(replicate((n_subj/4), rbeta(12, 3, 6) %>% sort()) %>% as.vector(), ~rbernoulli(n = 8, p = .x)) %>% flatten_int(),
    map(replicate((n_subj/4), rbeta(12, 4, 5) %>% sort()) %>% as.vector(), ~rbernoulli(n = 8, p = .x)) %>% flatten_int(),
    map(replicate((n_subj/4), rbeta(12, 5, 4) %>% sort()) %>% as.vector(), ~rbernoulli(n = 8, p = .x)) %>% flatten_int(),
    map(replicate((n_subj/4), rbeta(12, 6, 3) %>% sort()) %>% as.vector(), ~rbernoulli(n = 8, p = .x)) %>% flatten_int()
  ) # ,
  # rt = c(
  #   map(rnorm(25, 220, 20), ~rnorm(n = 8, mean = 2200 - .x, sd = 250)) %>% flatten_dbl()
  # )
)

gensim <- tibble(
  subj      = rep(subj, each = 49),
  condition = rep(cond, each = 49),
  img_cluster = replicate(n_subj, sample(img_cluster, 49)) %>% as.vector(),
  # img_id    = c(), 
  response  = c( # taking it easy...
    replicate((n_subj/4), rbernoulli(49, rbeta(1, 3, 6))) %>% as.integer(),
    replicate((n_subj/4), rbernoulli(49, rbeta(1, 3, 5))) %>% as.integer(),
    replicate((n_subj/4), rbernoulli(49, rbeta(1, 4, 6))) %>% as.integer(),
    replicate((n_subj/4), rbernoulli(49, rbeta(1, 3, 4))) %>% as.integer()
  )
)
rm(subj, cond, block, imgs1, imgs2, img_cluster)

saveRDS(simdat, "data-raw/simulated_training.rds")
saveRDS(gensim, "data-raw/simulated_generalization.rds")
