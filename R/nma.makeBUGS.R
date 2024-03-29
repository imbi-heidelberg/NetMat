#add warning messages for incompatible link and family

makeBUGScode <- function(family, link, effects, enrichment, inconsistency, prior.mu.str, prior.d.str, prior.sigma2.str,
                         meta.covariate, prior.meta.reg, prior.ww.str, auto, arm, contrast){
  
  # Set up family and monitor strings for arm-based reporting trials
  
  if (family=="binomial"){
    family.str <- "r[i,k] ~ dbin(p[i,k],n[i,k]) # binomial likelihood"
    monitor.str <- "rhat[i,k] <- p[i,k] * n[i,k] # expected value of the numerators
    dev_a[i,k] <- 2 * (r[i,k] * (log(r[i,k])-log(rhat[i,k])) #Deviance contribution
    + (n[i,k]-r[i,k]) * (log(n[i,k]-r[i,k]) - log(n[i,k]-rhat[i,k])))"
  }
  
  if (family=="normal"){
    family.str <- "y[i,k] ~ dnorm(theta_a[i,k],prec[i,k])"
    monitor.str <- "prec[i,k] <- pow(se[i,k],-2)
    dev_a[i,k] <- (y[i,k]-theta_a[i,k])*(y[i,k]-theta_a[i,k])*prec[i,k] #Deviance contribution"
  }
  
  if (family=="poisson"){
    family.str <- "r[i,k] ~ dpois(theta_a[i,k]) # Poisson likelihood"
    monitor.str <- "theta_a[i,k] <- lambda[i,k]*E[i,k] # failure rate * exposure
    dev_a[i,k] <- 2*((theta_a[i,k]-r[i,k]) + r[i,k]*log(r[i,k]/theta_a[i,k])) #Deviance contribution"
  }
  
  # Set up family and monitor strings for contrast-based reporting trials
    
    family.str.c <- "for (k in 1:(na_c[i]-1)) {
                     for (j in 1:(na_c[i]-1)) {
                     Sigma1[i,j,k] <- se.diffs[i,k+1]^2*(equals(j,k)) + var.arm1[i,1]*(1-equals(j,k))
                     }
    }
    Omega[i, 1:(na_c[i]-1), 1:(na_c[i]-1)] <- inverse(Sigma1[i,1:(na_c[i]-1), 1:(na_c[i]-1)])
    y_c[i,2:na_c[i]] ~ dmnorm(theta_c[i,2:na_c[i]], Omega[i, 1:(na_c[i]-1), 1:(na_c[i]-1)])"
    monitor.str.c <- "for(k in 1:(na_c[i]-1)) {
    # CHANGEBACK
    ydiff[i,k] <- y_c[i,(k+1)]-theta_c[i,(k+1)]
    # ydiff[i,k] <- 1
    }"
  
    
    # Deviance strings
    dev.str.c <- "dev_c[i] <- t(ydiff[i,1:(na_c[i]-1)])%*%Omega[i,1:(na_c[i]-1), 1:(na_c[i]-1)]%*%ydiff[i,1:(na_c[i]-1)]
    resdev_c[i] <- dev_c[i]"
    
    dev.str <- "resdev_a[i] <- sum(dev_a[i,1:na_a[i]])"
  
    # TODO testing metareg for contrast models?
  if (!is.null(meta.covariate) && is.null(enrichment)){ #only do meta-regression if there is no enrichment chosen.
    metareg.str <- "+ (beta[t_a[i,k]]-beta[t_a[i,1]])*(x_a[i,k])"
    metareg.str.c <- "+ (beta[t_c[i,k]]-beta[t_c[i,1]])*(x_c[i,k])"
  } else {
      metareg.str <- ""
      metareg.str.c <- ""
  }
  
  if (effects == "fixed"){ # in fixed effect meta-analysis no enrichment model possible
    
    # Set up link for arm-based reporting trials
    if (family == "binomial" && link=="logit"){
      link.str <- "logit(p[i,k]) <- mu[i] + d[t_a[i,k]] - d[t_a[i,1]]"
    }  else if (family == "binomial" && link=="log"){
      link.str <- "log(p[i,k]) <- mu[i] + d[t_a[i,k]] - d[t_a[i,1]]"
    } else if (family == "normal" && link == "identity"){
      link.str <- "theta_a[i,k] <- mu[i] + d[t_a[i,k]] - d[t_a[i,1]] # model for linear predictor"
    } else if (family == "poisson" && link=="log"){
      link.str <- "log(lambda[i,k]) <- mu[i] + d[t_a[i,k]] - d[t_a[i,1]] # model for linear predictor"
    } else if (family== "binomial" && link=="cloglog"){
      link.str <- "cloglog(p[i,k]) <- log(time[i,k]) + mu[i] + d[t_a[i,k]] - d[t_a[i,1]] # model for linear predictor"
    } else {
      link.str <- ""
    }
    
    # Set up link for contrast-based reporting trials
    link.str.c <- "theta_c[i,k] <- d[t_c[i,k]] - d[t_c[i,1]]"

    # TODO test metareg
      link.str <- paste0(link.str, metareg.str)
      link.str.c <- paste0(link.str.c, metareg.str.c)
    
    prior.ww.str <- ""
      
      
    # Fixed Effects Consistency Model
    if(!inconsistency){
      
      if(arm) {
        
        model.str.a <- sprintf("for(i in 1:ns_a){                      # LOOP THROUGH ARM-BASED STUDIES
      
        for (k in 1:na_a[i]) {             # LOOP THROUGH ARMS
          %s
          %s
          %s
        }
      %s
      }", family.str, monitor.str, link.str, dev.str)
        
      } else {model.str.a <- "resdev_a <- 0"}
      
      if(contrast) {
        
        model.str.c <- sprintf("for(i in 1:ns_c) {                  # LOOP THROUGH CONTRAST-BASED STUDIES
      
        %s
        %s
      
        for(k in 1:na_c[i]) {
      
          %s
      
        }
        
        %s
      
      }", family.str.c, monitor.str.c, link.str.c, dev.str.c)
        
      } else { model.str.c <- "resdev_c <- 0"}
      
      code.str <- sprintf("#This code is adapted from
    #Dias, S., Welton, N.J., Sutton, A.J. & Ades, A.E. NICE DSU Technical Support Document 2: 
    #A Generalised Linear Modelling Framework for Pairwise and Network Meta-Analysis of Randomised
    #Controlled Trials. 2011; last updated September 2016 (available from http:
    #//www.nicedsu.org.uk).
                          
    # fixed effects model for multi-arm trials
                          
    %s
    # arm-based trials
      %s 
    # contrast - based trials
      %s
      totresdev <- sum(resdev_a) + sum(resdev_c[])
      d[1]<-0
      %s
                         
      %s
      %s
      %s
                     
    %s", paste0(ifelse(auto, "", "model{                               # *** PROGRAM STARTS")),
        model.str.a,
        model.str.c,
        ifelse(arm, prior.mu.str, ""), # only include priors for mu if arm-based data is included
        prior.ww.str,
        prior.d.str,
        prior.meta.reg,
        paste0(ifelse(auto, "", "}")))
    }
    
    if(inconsistency){

      if (family == "binomial" && link=="logit"){
        link.str <- "logit(p[i,k]) <- mu[i] + d[t_a[i,1],t_a[i,k]]"
      }  else if (family == "binomial" && link=="log"){
        link.str <- "log(p[i,k]) <- mu[i] + d[t_a[i,1],t_a[i,k]]"
      } else if (family == "normal" && link == "identity"){
        link.str <- "theta_a[i,k] <- mu[i] + d[t_a[i,1],t_a[i,k]]"
      } else if (family == "poisson" && link=="log"){
        link.str <- "log(lambda[i,k]) <- mu[i] + d[t_a[i,1],t_a[i,k]]"
      } else if (family== "binomial" && link=="cloglog"){
        link.str <- "cloglog(p[i,k]) <- log(time[i,k]) + mu[i] + d[t_a[i,1],t_a[i,k]]"
      }  else {
        link.str <- ""
      }

      # Set up link for contrast-based reporting trials
      link.str.c <- "theta_c[i,k] <- d[t_c[i,1],t_c[i,k]]"

      link.str <- paste0(link.str, metareg.str)
      link.str.c <- paste0(link.str.c, metareg.str.c)
      
      
      if(arm) {
        
        model.str.a <- sprintf("for(i in 1:ns_a){             # LOOP THROUGH STUDIES

        for (k in 1:na_a[i])  {   # LOOP THROUGH ARMS
          %s
          %s
          %s
          }
          %s
      }", family.str, monitor.str, link.str, dev.str)
        
      } else {model.str.a <- "resdev_a <- 0"}
      
      if(contrast) {
        
        model.str.c <- sprintf("for(i in 1:ns_c) {

        %s
        %s

        for(k in 1:na_c[i]) {

          %s

        }

        %s

      }",family.str.c, monitor.str.c, link.str.c, dev.str.c)
        
      } else { model.str.c <- "resdev_c <- 0"}

      code.str <- sprintf("# inconsistency model
    # fixed effects model

    %s
    
    # arm-based trials
      %s
      
    # contrast-based trials
      %s

      totresdev <- sum(resdev_a[], resdev_c[])
        for (k in 1:nt){d[k,k]<-0}  #set effects of k vs k to zero
        %s

        %s
        %s
        %s

      %s
      ", paste0(ifelse(auto, "", "model{                      # *** PROGRAM STARTS")),
         model.str.a,
         model.str.c,
        ifelse(arm, prior.mu.str, ""), # only include mu priors if arm-based trials included
        prior.d.str,
        prior.ww.str,
        prior.meta.reg,
        paste0(ifelse(auto, "", "}")))
    }

  } 
  
  if (effects == "random"){

    if (family == "binomial" && link=="logit"){
      link.str <- "logit(p[i,k]) <- mu[i] + delta[i,k]"
    } else if (family == "binomial" && link=="log"){
      link.str <- "log(p[i,k]) <- mu[i] + delta[i,k]"
    } else if (family == "normal"){
      link.str <- "theta_a[i,k] <- mu[i] + delta[i,k]"
    } else if (family == "poisson" && link=="log"){
      link.str <- "log(lambda[i,k]) <- mu[i] + delta[i,k]"
    } else if (family == "binomial" && link=="cloglog"){
      link.str <- "cloglog(p[i,k]) <- log(time[i,k]) + mu[i] + delta[i,k]"
    }  else {
      link.str <- ""
    }

    link.str.c <- "theta_c[i,k] <- delta[i+ns_a,k]"

    link.str <- paste0(link.str, metareg.str)
    link.str.c <- paste0(link.str.c, metareg.str.c)

    if(!inconsistency){
      
      if(arm) {
        
        if(is.null(enrichment)){
          delta.str <- "delta[i,k] ~ dnorm(md[i,k],taud[i,k])"
        }else if(enrichment == "covariate"){
          delta.str <- "delta[i,k] ~ dnorm(md[i,k],taud[i,k]*x_a[i,k])"
        }else{  #stimmt das so?# i oder [i,k]
          delta.str <- "ind[i,k] <- ifelse(x_a[i,k]==1,taud[i,k],taud[i,k]*ww) 
                        delta[i,k] ~ dnorm(md[i,k],ind[i,k])"
        }
      
        
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
          #delta[i,k] ~ dnorm(md[i,k],taud[i,k])
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
        
      } else {model.str.a <- "resdev_a <- 0"}
      
      if(contrast) {
        
        model.str.c <- sprintf("
        for(i in 1:ns_c){                      # LOOP THROUGH STUDIES
        %s
        %s
        w_c[i,1] <- 0    # adjustment for multi-arm trials is zero for control arm
        delta[i+ns_a, 1] <- 0
        for (k in 1:na_c[i]) {             # LOOP THROUGH ARMS

          # model for linear predictor
          %s

        }
        %s
        for (k in 2:na_c[i]) {             # LOOP THROUGH ARMS
          # trial-specific LOR distributions
          delta[i+ns_a,k] ~ dnorm(md_c[i,k],taud_c[i,k])
          # mean of LOR distributions, with multi-arm trial correction
          md_c[i,k] <-  d[t_c[i,k]] - d[t_c[i,1]] + sw_c[i,k]
          # precision of LOR distributions (with multi-arm trial correction)
          taud_c[i,k] <- pow(sigma2,-1) *2*(k-1)/k
          # adjustment, multi-arm RCTs
          w_c[i,k] <- (delta[i+ns_a,k] - d[t_c[i,k]] + d[t_c[i,1]])
          # cumulative adjustment for multi-arm trials
          sw_c[i,k] <- sum(w_c[i,1:(k-1)])/(k-1)
        }
      }", family.str.c, monitor.str.c, link.str.c, dev.str.c)
        
      } else {model.str.c <- "resdev_c <-0"}

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
      %s
      tau <- pow(sigma,-2)
      %s
    %s", paste0(ifelse(auto, "", "model{                               # *** PROGRAM STARTS")),
        model.str.a,
        model.str.c,
        ifelse(arm, prior.mu.str, ""), # only include mu prior if arm-based studies
        ifelse(arm, prior.ww.str, ""), # only include ww prior if arm-based studies
        prior.d.str,
        prior.sigma2.str,
        prior.meta.reg,
        paste0(ifelse(auto, "", "}")))

    }

    if(inconsistency){
      prior.ww.str <-""
      
      if(arm) {
        
        model.str.a <- sprintf(" for(i in 1:ns_a){             # LOOP THROUGH STUDIES
        delta[i,1]<-0           # treatment effect is zero in control arm

        for (k in 1:na_a[i])  {   # LOOP THROUGH ARMS
          %s
          %s
          %s
        }
        %s
        for (k in 2:na_a[i]) {  # LOOP THROUGH ARMS
          delta[i,k] ~ dnorm(d[t_a[i,1], t_a[i,k]] , pow(sigma2,-1)) # trial-specific LOR distributions
        }
      }", family.str, monitor.str, link.str, dev.str)
        
      } else {model.str.a <- "resdev_a <- 0"}
      
      if (contrast) {
        
        model.str.c <- sprintf("for(i in 1:ns_c){             # LOOP THROUGH STUDIES
        delta[i+ns_a,1]<-0           # treatment effect is zero in control arm
        %s
        %s
        for (k in 1:na_c[i])  {   # LOOP THROUGH ARMS
          %s

        }
        %s
        for (k in 2:na_c[i]) {  # LOOP THROUGH ARMS
          delta[i+ns_a,k] ~ dnorm(d[t_c[i,1], t_c[i,k]] , pow(sigma2,-1)) # trial-specific LOR distributions
        }
      }", family.str.c, monitor.str.c, link.str.c, dev.str.c)
        
      } else {model.str.c <- "resdev_c <- 0"}

      code.str <- sprintf("# Binomial likelihood, inconsistency model
      # Random effects model
      %s

      # arm-based trials
        %s

      # contrast-based trials
        %s

      totresdev <- sum(resdev_a[], resdev_c[])
      %s
      %s
      %s
      %s
      tau <- 1/sigma2
      %s

    %s
    ", paste0(ifelse(auto, "", "model{                      # *** PROGRAM STARTS")),
                          model.str.a,
                          model.str.c,
                          ifelse(arm, prior.mu.str, ""), # only need mu prior if arm-based trials included
                          prior.ww.str, 
                          prior.d.str,
                          prior.sigma2.str,
                          prior.meta.reg,
                          paste0(ifelse(auto, "", "}")))

    }
  }
return(code.str)

}
