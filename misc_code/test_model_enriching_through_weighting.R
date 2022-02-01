# Model from bugsnet for enriching through weighting 
# Ich brauche diese Teile um das package fertig zu stellen
#makeBUGScode <- function(family, link, effects, inconsistency, prior.mu.str, prior.d.str, prior.sigma2.str, meta.covariate, prior.meta.reg, auto, arm, contrast)
# Die oben stehende Parameter werden direkt eingegeben?
# Hier müsste man eventuell prior.ww eingeben? Dann wäre es für alle enrichment through weighting


family.str <-"r[i,k] ~ dbin(p[i,k],n[i,k]) # binomial likelihood"
monitor.str <- "rhat[i,k] <- p[i,k] * n[i,k] # expected value of the numerators
    dev_a[i,k] <- 2 * (r[i,k] * (log(r[i,k])-log(rhat[i,k])) #Deviance contribution
    + (n[i,k]-r[i,k]) * (log(n[i,k]-r[i,k]) - log(n[i,k]-rhat[i,k])))"
link.str <- "logit(p[i,k]) <- mu[i] + delta[i,k]"
dev.str <- "resdev_a[i] <- sum(dev_a[i,1:na_a[i]])"

### Neu hinzufuegen for enriching through weighting
delta.str <- "delta[i,k] ~ dnorm(md[i,k],taud[i,k]*(x_a[i,k]))"          

model.str.a <- sprintf("for(i in 1:ns_a){                      # LOOP THROUGH STUDIES

        w[i,1] <- 0    # adjustment for multi-arm trials is zero for control arm
        delta[i,1] <- 0             # treatment effect is zero for control arm
        for (k in 1:na_a[i]) {             # LOOP THROUGH ARMS
          %s
          # model for linear predictor
          %s
          %s
        }
        %s
        for (k in 2:na_a[i]) {             # LOOP THROUGH ARMS
          # trial-specific LOR distributions
          #delta[i,k] ~ dnorm(md[i,k],taud[i,k]*(x_a[i,k]))
          %s
          # mean of LOR distributions, with multi-arm trial correction
          md[i,k] <-  d[t_a[i,k]] - d[t_a[i,1]] + sw[i,k]
          # precision of LOR distributions (with multi-arm trial correction)
          taud[i,k] <- pow(sigma2,-1) *2*(k-1)/k
          # adjustment, multi-arm RCTs
          w[i,k] <- (delta[i,k] - d[t_a[i,k]] + d[t_a[i,1]])
          # cumulative adjustment for multi-arm trials
          sw[i,k] <- sum(w[i,1:(k-1)])/(k-1)
        }
      }", monitor.str,family.str, link.str, dev.str, delta.str)


# zweiter Teil mit Prior ....
code.str <- sprintf("

      # Random effects model for multi-arm trials

      %s
      # arm-based trials
        %s
        
      # contrast-based trials
        %s

      totresdev <- sum(resdev_a[], resdev_c[])
      d[1]<-0       # treatment effect is zero for reference treatment
      %s
      %s
      %s
      tau <- pow(sigma,-2)
      %s
    %s", paste0(ifelse(auto, "", "model{                               # *** PROGRAM STARTS")),
                    model.str.a,
                    model.str.c,
                    ifelse(arm, prior.mu.str, ""), # only include mu prior if arm-based studies
                    prior.d.str,
                    prior.sigma2.str,
                    prior.meta.reg,
                    paste0(ifelse(auto, "", "}")))
