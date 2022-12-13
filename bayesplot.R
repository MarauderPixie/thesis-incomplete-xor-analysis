library(bayesplot)

yrep <- posterior_predict(h1_inter, ndraws = 500)

ppc_dens_overlay(y = extra_binom$k,
                 yrep = yrep[1:50, ]) + xlim(0, 5)

ppc_hist(y = extra_binom$k, binwidth = 1,
         yrep = yrep[sample(1:183, 5), ]) 

ppc_ecdf_overlay(y = extra_binom$k, yrep = yrep[sample(1:183, 50), ],
                 discrete = TRUE)

ppc_stat(extra_binom$k, yrep, stat = mean, binwidth = 0.01)

ppc_bars_grouped(y = extra_binom$k, yrep = yrep[sample(1:183, 5), ], 
                 group = extra_binom$rules)


##
ck6_inter <- readRDS("models/conkurz/ck_exab6_interaction.rds")
ck5_inter <- readRDS("models/conkurz/ck_exab5_interaction.rds")

yrep_ck <- posterior_predict(ck_ab6_both, ndraws = 500)


ppc_ecdf_overlay(y = extra_binom$exab6, yrep = yrep_ck[sample(1:183, 50), ],
                 discrete = TRUE)

# ppc_stat(extra_binom$exab6, yrep_ck, stat = mean, binwidth = 0.01)

ppc_bars(y = extra_binom$exab6, yrep = yrep_ck[sample(1:183, 5), ])
ppc_bars_grouped(y = extra_binom$exab6, yrep = yrep_ck[sample(1:183, 5), ], 
                 group = extra_binom$subj_id) +
  theme(legend.position = "top",
        panel.spacing = unit(5, "pt"),
        strip.text = element_text(size = 4))



##
yrep_re <- posterior_predict(h1_re, ndraws = 500)

ppc_ecdf_overlay(y = extra_berni$extrapolation, yrep = yrep_re[sample(1:183, 50), ],
                 discrete = TRUE)

# ppc_stat(extra_binom$exab6, yrep_re, stat = mean, binwidth = 0.01)

ppc_bars(y = extra_berni$extrapolation, yrep = yrep_re[sample(1:500, 5), ])
ppc_bars_grouped(y = extra_berni$extrapolation, yrep = yrep_re[sample(1:183, 5), ], 
                 group = extra_berni$subj_id) +
  theme(legend.position = "top",
        panel.spacing = unit(5, "pt"),
        strip.text = element_text(size = 6))
