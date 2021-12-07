# Model from bugsnet for enriching through weighting 
"r[i,k] ~ dbin(p[i,k],n[i,k]) # binomial likelihood"
monitor.str <- "rhat[i,k] <- p[i,k] * n[i,k] # expected value of the numerators
    dev_a[i,k] <- 2 * (r[i,k] * (log(r[i,k])-log(rhat[i,k])) #Deviance contribution
    + (n[i,k]-r[i,k]) * (log(n[i,k]-r[i,k]) - log(n[i,k]-rhat[i,k])))"