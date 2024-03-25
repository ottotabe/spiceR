# B = Baseline variables,
# D = Dex. dose
# P = Propofol dose
# M = Midazolam dose
# Y = 90-day mortality

library(cfid)
# We first define the causal graph, the DAG.
g <- dag("B -> {D, P, M, Y}; {D, P, M} -> Y")

# Then we get the formula of causal effect.
causal_effect(g, y = "Y", x = c("D", "P", "M"))
