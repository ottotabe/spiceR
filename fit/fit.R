library(brms)

# The first model with the interactions of dex, propofol and midazolam doses.
# Data "dat" is assumed to be loaded in the environment.
fit <- brm(died90 ~ (dex_avg + prop_avg + mid_avg)^2 +
             trt_group + sex + adm_reason + adm_source + location +
             age + apacheII + weight, 
           data = dat, family = "bernoulli", iter = 5000, chains = 4)

# The second model with additional interaction between doses and age.
fit2 <- brm(died90 ~ (dex_avg + prop_avg + mid_avg + age)^2 +
              trt_group + sex + adm_reason + adm_source + location +
              apacheII + weight, 
            data = dat, family = "bernoulli", iter = 5000, chains = 4)
