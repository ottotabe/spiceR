library(tidyverse)
library(patchwork)

#####################
# CODE FOR FIGURE 4 #
#####################

## This function returns the samples of posterior distribution in condition
## of newdex, newprop and newmid
trt_samples <- Vectorize(function(fit, newdex, newprop, newmid) {
  newdat <- dat %>% mutate(dex_avg = newdex, 
                           prop_avg = newprop, 
                           mid_avg = newmid)
  return(posterior_predict(fit, newdat) %>% rowMeans())
}, vectorize.args = c("newdex", "newprop", "newmid")

## This function returns median and 95% posterior interval of the samples
## of the function above
trt_estimates <- function(fit, newdex, newprop, newmid, alpha = 0.05) {
  samp <- trt_samples(fit, newdex, newprop, newmid)
  est <- apply(X = samp, 
               MARGIN = 2, 
               FUN = \(x) quantile(x, probs = c(alpha / 2, 0.5, 1 - alpha / 2)))
  est <- as.data.frame(t(est))
  colnames(est) <- c("lwr", "est", "upp")
  return(est)
}

## This function plots the estimates from the function above
trt_plot <- function(fit, newdex, newprop, newmid, x, alpha = 0.05) {
  est <- trt_estimates(fit, newdex, newprop, newmid, alpha)
  est <- cbind(est, x)
  ggplot(est, aes(x = x, y = est)) + 
    geom_ribbon(aes(ymin = lwr, ymax = upp), alpha = 0.35) +
    geom_line()
}


dexplot <- trt_plot(fit, newdex = seq(0, 1, l = 50), newprop = 0, newmid = 0,
                    x = seq(0, 1, l = 50))

propplot <- trt_plot(fit, newprop = seq(0, 1.5, l = 50), newdex = 0, newmid = 0,
                                x = seq(0, 1.5, l = 50))

midplot <- trt_plot(fit, newmid = seq(0, 50, l = 50), newdex = 0, newprop = 0,
                     x = seq(0, 50, l = 50))

(dexplot + labs(title = "Kasvava keskim. dex-annos, muut hoidot 0",
                x = "Dex-annos (mikrog/kg/h", y = "Kuolleisuus") + theme_bw()) /
  ((propplot + labs(title = "Kasvava keskim. prop-annos \nmuut hoidot 0",
                    x = "Propofoliannos (mg/kg/h", y = "Kuolleisuus") + theme_bw()) |
     (midplot + labs(title = "Kasvava keskim. mid-annos \nmuut hoidot 0",
                     x = "Midatsolaamiannos (mikrog/kg/h", y = "Kuolleisuus") + theme_bw()))

#####################
# CODE FOR FIGURE 5 #
#####################

# This function draws a boxplot based on trt_samples
trt_boxplot <- function(fit, newdex, newprop, newmid) {
  samp <- trt_samples(fit, newdex, newprop, newmid)
  samp <- pivot_longer(as.data.frame(samp), cols = everything())
  ggplot(data = samp, aes(x = name, y = value)) + geom_boxplot()
}

dexpropplot <- trt_boxplot(fit, newdex = c(0.103, 0.382, 0.669, 0.961),
                           newprop = c(0.137, 0.512, 1.03, 1.93), newmid = 0)
dexmidplot <- trt_boxplot(fit, newdex = c(0.103, 0.382, 0.669, 0.961),
                          newmid = c(0.485, 3.73, 12.9, 42.5), newprop = 0)
propmidplot <- trt_boxplot(fit, newprop = c(0.137, 0.512, 1.03, 1.93),
                           newmid = c(0.485, 3.73, 12.9, 42.5), newdex = 0)

((dexproppplot + labs(title = "Kasvava dex & prop, \n tasainen mid.",
                      y = "Kuolleisuus", x = "Dex/Prop annos")) +
    scale_x_discrete(labels = c("0.103/0.137", "0.382/0.512", "0.669/1.03", "0.961/1.93")) +
    theme_bw() /
    dexmidplot + labs(title = "Kasvava dex & mid, \n tasainen mid.",
                      y = "Kuolleisuus", x = "Dex/Mid annos")) +
  scale_x_discrete(labels = c("0.103/0.485", "0.382/3.73", "0.669/12.9", "0.961/42.5")) +
  theme_bw()) |
  (propmidplot + labs(title = "Kasvava prop & mid, \n tasainen dex.",
                      y = "Kuolleisuus", x = "Prop/Mid annos")) +
  scale_x_discrete(labels = c("0.137/0.485", "0.512/3.73", "1.03/12.9", "1.93/42.5")) +
  theme_bw())

#####################
# CODE FOR FIGURE 6 #
#####################
#

age_samples <- Vectorize(function(fit, newdex, newprop, newmid, newage) {
  newdat <- dat %>% mutate(dex_avg = newdex, 
                           prop_avg = newprop, 
                           mid_avg = newmid,
                           age = newage)
  return(posterior_predict(fit, newdat) %>% rowMeans())
}, vectorize.args = c("newdex", "newprop", "newmid")

age_estimates <- function(fit, newdex, newprop, newmid, newage, alpha = 0.05) {
  samp <- age_samples(fit, newdex, newprop, newmid)
  est <- apply(X = samp, 
               MARGIN = 2, 
               FUN = \(x) quantile(x, probs = c(alpha / 2, 0.5, 1 - alpha / 2)))
  est <- as.data.frame(t(est))
  colnames(est) <- c("lwr", "est", "upp")
  return(est)
}

age_plot <- function(fit, newdex, newprop, newmid, newage, x, alpha = 0.05) {
  res <- NULL
  for (i in 1:length(newage)) {
    est <- cbind(age_estimates(fit, newdex, newprop, newmid, newage[i]), 
                 x, age = newage[i])
    res <- rbind(res, est)
  }
  res <- as.data.frame(res)
  res$age <- factor(res$age)
  ggplot(res, aes(x = x, y = est, col = age, linetype = age)) +
    geom_line(linewidth = 1)
}

dexage <- age_plot(fit2, newdex = (0:10) / 10, newprop = 0, newmid = 0,
                  newage = c(35, 45, 55, 65), x = (0:10) / 10)

propage <- age_plot(fit2, newprop = (0:15) / 15, newdex = 0, newmid = 0,
                    newage = c(35, 45, 55, 65), x = (0:15) / 15)

midage <- age_plot(fit2, newmid = (0:10) * 5, newdex = 0, newprop = 0,
                   newage = c(35, 45, 55, 65), x = (0:10) * 5)