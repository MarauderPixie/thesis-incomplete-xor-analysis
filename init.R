library(tidyverse)
library(brms)
# library(bayesplot)
# library(loo)

ncore <- parallel::detectCores()

simt <- readRDS("data-raw/simulated_training.rds")
simg <- readRDS("data-raw/simulated_generalization.rds")
