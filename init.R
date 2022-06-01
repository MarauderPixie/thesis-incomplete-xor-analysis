library(tidyverse)
library(brms)
# library(bayesplot)
# library(loo)

theme_set(hrbrthemes::theme_ipsum_ps(plot_margin = margin(10, 10, 10, 10)) + 
            theme(panel.background = element_rect(fill = "#f0f0f0"),
                  plot.background = element_rect(fill = "#f0f0f0")))
ncore <- parallel::detectCores()

simt <- readRDS("data-raw/simulated_training.rds")
simg <- readRDS("data-raw/simulated_generalization.rds")
