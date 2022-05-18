library(tidyverse)
library(brms)
# library(bayesplot)
# library(loo)

simt <- readRDS("data-raw/simulated_training.rds")
simg <- readRDS("data-raw/simulated_generalization.rds")