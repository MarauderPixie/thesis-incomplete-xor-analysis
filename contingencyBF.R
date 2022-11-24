ab4 <- table(extra_binom$combi, extra_binom$exab4)
ab5 <- table(extra_binom$combi, extra_binom$exab5)
ab6 <- table(extra_binom$combi, extra_binom$exab6)

BayesFactor::contingencyTableBF(ab6, sampleType = "indepMulti", fixedMargin = "rows")


# single effects:
BayesFactor::contingencyTableBF(
  table(extra_binom$rules, extra_binom$exab6),
  sampleType = "indepMulti", fixedMargin = "rows"
)

BayesFactor::contingencyTableBF(
  table(extra_binom$blocked, extra_binom$exab6),
  sampleType = "indepMulti", fixedMargin = "rows"
)
