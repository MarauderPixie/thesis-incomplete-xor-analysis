library(tidyverse)
library(gghalves)
library(brms)
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
theme_subjects <- theme(legend.position = "top",
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.title.x  = element_blank(),
                        axis.title.y  = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        # panel.border = element_rect(fill = NA),
                        panel.spacing = unit(5, "pt"),
                        strip.text = element_text(size = 6))
ncore <- parallel::detectCores()
