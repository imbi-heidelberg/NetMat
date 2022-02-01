install.packages("rtools")
library(rtools)

library(BUGSnet)
library(tidyverse)
library(magrittr)

dat <- data.prep(nsclc_data, varname.t = "treatment", varname.s = "study")


model_re003 <- nma.model(dat = dat, outcome = "event", N = "n", reference = "Chemo", family = "binomial",
                      link = "logit", effects = "random", enrichment = "prior", prior.ww = "dunif(0,0.3)", covariate = "x",
                      prior.beta = "EXCHANGEABLE", prior.sigma = "dunif(0,2)", prior.mu = "dnorm(0,1.0E-6)", prior.d = "dnorm(0,1.0E-6)")

model_re037 <- nma.model(dat = dat, outcome = "event", N = "n", reference = "Chemo", family = "binomial",
                         link = "logit", effects = "random", enrichment = "prior", prior.ww = "dunif(0.3, 0.7)", covariate = "x",
                         prior.beta = "EXCHANGEABLE")

model_re071 <- nma.model(dat = dat, outcome = "event", N = "n", reference = "Chemo", family = "binomial",
                         link = "logit", effects = "random", enrichment = "prior", prior.ww = "dunif(0.7,1)", covariate = "x",
                         prior.beta = "EXCHANGEABLE")


### ww 0 0.3

set.seed(1234)
results_re003 <- nma.run(model_re003, n.adapt = 5000, n.burnin = 50000, n.iter = 20000, thin = 2, DIC = FALSE)

s_re <- summary(results_re003$samples)
tbl_re <- cbind(s_re$statistics[,1:2], s_re$quantiles[,c(3,1,5)])
tbl_re
summary(results_re003$samples)

jags.enrich_commonw0003


### ww 0.3 0.7


set.seed(1234)
results_re037 <- nma.run(model_re037, n.adapt = 5000, n.burnin = 50000, n.iter = 20000, DIC = F, thin = 2)

s_re <- summary(results_re037$samples)
tbl_re <- cbind(s_re$statistics[,1:2], s_re$quantiles[,c(3,1,5)])
tbl_re
summary(results_re037$samples)

jags.enrich_commonw0307


### ww 0.7 1


set.seed(1234)
results_re071 <- nma.run(model_re071, n.adapt = 5000, n.burnin = 50000, n.iter = 20000, DIC = F, thin = 2)

s_re <- summary(results_re071$samples[,2:5])
tbl_re <- cbind(s_re$statistics[,1:2], s_re$quantiles[,c(3,1,5)])
tbl_re

summary(results_re071$samples)
summary(results_re003$samples)
summary(results_re037$samples)

jags.enrich_commonw071

jags.enrich_commonw0003

jags.enrich_commonw0307

