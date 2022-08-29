library(tidyverse)
# library(brms)
# library(bayesplot)
# library(loo)

theme_set(hrbrthemes::theme_ipsum_ps(plot_margin = margin(10, 10, 10, 10)) + 
            theme(panel.background = element_rect(fill = "#f0f0f0"),
                  plot.background = element_rect(fill = "#f0f0f0")))

theme_transfer <- theme(legend.text = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.title.x  = element_blank(),
                        axis.title.y  = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank())
ncore <- parallel::detectCores()
