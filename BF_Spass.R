bf_list <- numeric(50)

t_start <- Sys.time()
for (i in seq(bf_list)) {
  bf <- bayes_factor(h2_inter, 
                     h2_null, 
                     cores = ncore)
  bf_list[i] <- bf$bf
  cat(
    paste0("--------------------------------\n",
           "Another one done, that makes ", i, "!\n",
           "--------------------------------\n\n")
    )
}
t_end <- Sys.time()
t_end - t_start
