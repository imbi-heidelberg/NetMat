#' Create Bugs Model for Arm-Level Data
#' @description Creates BUGS code which can be ran through \code{nma.run()}.
#' 
#' @param data A \code{BUGSnetData} object containing the data from arm-based trials produced by \code{data.prep()}
#' @param outcome A string indicating the name of your outcome variable.
#' @param N A string indicating the name of the variable containing the number of participants in each arm.
#' @param sd A string (only required for continuous outcomes) indicating variable name
#' of the standard deviation of the outcome. Standard errors should be converted to standard deviation by multiplying by the square root of the sample size prior to using this function.
#' @param reference A string for the treatment that will be seen as the 'referent' comparator and labeled as treatment 1 in the BUGS code. This is often
#' a placebo or control drug of some kind.  
#' @param family A string indicating the family of the distribution of the outcome. Options are:
#' "binomial", "normal", "poisson".
#' @param link The link function for the nma model. Options are "logit" (binomial family), "log" (binomial or poisson family), "cloglog" (binomial family), "identity" (normal family).
#' @param time A string (only required for binomial-cloglog or poisson-log models) indicating the name of variable 
#'   indicating person-time followup (e.g person years) or study followup time.
#' @param effects A string indicating the type of treatment effect relative to baseline. Options are "fixed" or "random".
#' @param prior.mu A string of BUGS code that defines priors on the baseline treatment effects. By default, independent normal priors are used with mean 0 and standard deviation 15u, where u is the largest maximum likelihood estimator in single trials \insertCite{@see @gemtc}{BUGSnet}.
#' @param prior.d A string of BUGS code that defines define priors on relative treatment effects. By default, independent normal priors are used with mean 0 and standard deviation 15u, where u is the largest maximum likelihood estimator in single trials \insertCite{@see @gemtc}{BUGSnet}.
#' @param prior.sigma A string of BUGS code that defines the prior on the standard deviation of relative treatment effects. By default, a uniform distribution with range 0 to u is used, where u is the largest maximum likelihood estimator in single trials \insertCite{@see @gemtc}{BUGSnet}.
#' @param prior.beta Optional string that defines the prior on the meta-regression coefficients. Options are "UNRELATED", "EXCHANGEABLE", "EQUAL" \insertCite{@TSD3}{BUGSnet} or a string of BUGS code.
#' @param prior.ww Optional string that defines the prior on the enrichment through weighting. Options are "dunif(0,0.3)", "dunif(0.3, 0.7)", "dunif(0.7,1)" (see paper from Efthimiou))
#' @param covariate Optional string indicating the name of the variable in your data set that you would like to
#' adjust for via meta regression. By default, covariate=NULL and no covariate adjustment is applied. If the specified covariate is numeric then
#' it will be centered for the analysis. If it is a character or factor then it will be treated as categorical. Currently only categorical variables
#' with fewer than 3 levels are supported.
#' @param enrichment String indicating if an enrichment model through weighting should be used. By default
#' enrichment= NULL, Options are "prior" or "covariate". More prior needs to be defined later.
#' @param type If type="inconsistency", an inconsistency model will be built. By default, type="consistency" and a consistency model is built.
#' will be built.
#' @return \code{nma.model} returns an object of class \code{BUGSnetModel} which is a list containing the following components:
#' @return \code{bugs} - A long character string containing BUGS code that will be run in \code{jags}.
#' @return \code{data} - The data used in the BUGS code.
#' @return \code{scale} - The scale of the outcome, based on the chosen family and link function
#' examples are "Risk Ratio" (relative risk), "Odds Ratio", "Mean Difference", "Hazard Ratio"
#' @return \code{trt.key} - Treatments mapped to integer numbers, used to run BUGS code.
#' @return ...
#' 
#' @details 
#' For meta-regression, the prespecified prior choices for the regression coefficients \eqn{\beta_{(1,2)},…,\beta_{(1,K)}} are
#' \describe{
#'   \item{Unrelated:}{\deqn{iid t(0, u^2, 1)}}
#'   \item{Exchangeable:}{\deqn{iid N(b, \gamma^2), b ~ t(0, u^2, 1), \gamma ~ U(0,u)}}
#'   \item{Equal:}{\deqn{\beta_2=...=\beta_T=B, B ~ t(0, u^2, 1)}}
#' }
#' where \eqn{u} is the largest maximum likelihood estimator in single trials \insertCite{@see @gemtc}{BUGSnet}.
#' 
#' 
#' @examples
#' 
#' 
#' data(diabetes.sim)
#' 
#' diabetes.slr <- data.prep(arm.data = diabetes.sim, 
#' varname.t = "Treatment", 
#' varname.s = "Study")
#' 
#' #Random effects, consistency model.
#' #Binomial family, cloglog link. This implies that the scale will be the Hazard Ratio.
#'diabetes.re.c <- nma.model(data = diabetes.slr,
#'        outcome = "diabetes", 
#'        N = "n",
#'        reference = "Placebo",
#'        family = "binomial",
#'        link = "cloglog",
#'        effects = "random",
#'        type="consistency",
#'        time="followup"
#'        )
#'        
#' #Fixed effects, consistency model.
#' #Binomial family, cloglog link. This implies that the scale will be the Hazard Ratio.
#'diabetes.fe.c <- nma.model(data = diabetes.slr,
#'        outcome = "diabetes", 
#'        N = "n",
#'        reference = "Placebo",
#'        family = "binomial",
#'        link = "cloglog",
#'        effects = "fixed",
#'        type="consistency",
#'        time="followup"
#'        )
#'         
#' @export
#'
#' @references
#' \insertRef{gemtc}{BUGSnet}
#' 
#' \insertRef{TSD3}{BUGSnet}
#' 
#' 
#' @seealso \code{\link{data.prep}}, \code{\link{nma.run}}


nma.model <- function(data = NULL,
                          outcome,
                          N = NULL,
                          sd=NULL,
                          reference,
                          type="consistency",
                          time=NULL,
                          family = NULL,
                          link = NULL,
                          effects,
                          prior.mu = "DEFAULT",
                          prior.d = "DEFAULT",
                          prior.sigma = "DEFAULT",
                          prior.beta = NULL,
                          prior.ww = NULL,
                          covariate = NULL,
                          enrichment = NULL){
  
  armdat <- TRUE
  contrast <- FALSE
  # 
  # if(is.null(data)) {arm <- F}
  # if(is.null(data_contrast)) {contrast <- F}
  if((!is.null(data) && class(data) != "BUGSnetData"))
    stop("\'data\' must be a valid BUGSnetData object created using the data.prep function.")

  # Bind variables to function
  trt.ini <- NULL
  trt <- NULL
  trial <- NULL
  trt.jags <- NULL
  arm <- NULL
  value <- NULL
  variable <- NULL
  n.arms <- NULL
  differences <- NULL
  
  if(effects!="fixed" & effects!="random") stop("Effects must be either fixed or random.")
  
  if(type!="consistency" & type!="inconsistency") stop("Type must be either consistency or inconsistency.")
    
  
  if(!is.null(covariate)){
    if(is.null(enrichment) & is.null(prior.beta)){stop("prior.beta must be specified when covariate is specified")
    }
  }
  if(is.null(covariate) & !is.null(prior.beta))stop("covariate must be specified when prior.beta is specified")
  if(!is.null(prior.beta)){
    if(!(prior.beta %in% c("UNRELATED","EQUAL","EXCHANGEABLE"))){
      stop("prior.beta must be either UNRELATED, EQUAL, or EXCHANGEABLE")
    }
  }
  
  # Warnings for different families and data requirements
  if(family=="normal" & is.null(sd)) stop("sd must be specified for continuous outcomes")
  if(family=="normal" & !(link%in% c("identity"))) stop("This combination of family and link is currently not supported in BUGSnet.")
  if(family=="poisson" & link!="log") stop("This combination of family and link is currently not supported in BUGSnet.")
  if(family=="binomial" & !(link %in% c("log","logit", "cloglog"))) stop("This combination of family and link is currently not supported in BUGSnet.")
  
  # Specify a scale name (odds ratio, risk ratio, etc)
  if(link=="logit" & family %in% c("binomial", "binary", "bin", "binom")){
    scale <- "Odds Ratio"
  }else if(link=="log" & family %in% c("binomial", "binary", "bin", "binom")){
    scale <- "Risk Ratio"
  }else if(link== "identity" & family =="normal"){
    scale <- "Mean Difference"
  }else if(link =="cloglog" & family %in% c("binomial", "binary", "bin", "binom")){
    if(is.null(time)) stop("time must be specified when using a binomial family with the cloglog link")
    scale <- "Hazard Ratio"
  }else if(link == "log" & family =="poisson"){
    if(is.null(time)) stop("time must be specified when using a poisson family with the log link")
    scale <- "Rate Ratio"

  }
  
  
  #pull relevant fields from the data and apply naming convention
  avarlist <- c(trt = data$varname.t, trial = data$varname.s,
                r1 = outcome, N = N, sd = sd, 
                timevar = time, covariate = covariate) #se.diffs = se.diffs, var.ref = var.ref
  adata <- data$arm.data[, avarlist]
  names(adata) <- names(avarlist)
  trts <- adata$trt
  
  if(!(reference %in% trts)) {
    
    stop("Reference is not present in network - pick a reference treatment that is in the network.")
    
  }
  
  if(!(reference %in% adata$trt)) stop("Reference treatment is not present in the list of treatments.")
  
  #Trt mapping
  trt.key <- trts %>% unique %>% sort %>% tibble(trt.ini=.) %>%
    filter(trt.ini!=reference) %>% add_row(trt.ini=reference, .before=1) %>%
    mutate(trt.jags = 1:dim(.)[1])
  
  atrt <- trt.key[trt.key$trt.ini %in% adata$trt,]
  #add treatment mapping to data
  adata %<>% mutate(trt.jags=mapvalues(trt,
                                       from=atrt$trt.ini,
                                       to=atrt$trt.jags) %>% as.integer)
  
  
  if(!is.null(adata$sd)){ ifelse(!is.numeric(adata$sd) | adata$sd<=0, stop("sd must be numeric and greater than 0."), 1)}
  
  if(!is.numeric(adata$N)) stop("Sample size must be an integer greater than 0.")
  ifelse(floor(adata$N) != adata$N | adata$N<1, stop("Sample size must be an integer greater than 0."), 1)
  
  if(!is.numeric(adata$r1)) stop("Outcome must be numeric.")
  
  #pre-process the covariate if specified
  if (!is.null(covariate)) {
    covariates <- adata$covariate
    if (is.numeric(covariates) == TRUE){
      #issue warning if covariate appears to be categorical
      if (length(unique(covariates)) < 5) {
        warning(paste0("The specified covariate is being treated as continuous. Ignore this warning if this is the intended behaviour. ",
                       "For the covariate to be treated as a categorical variable it must be converted to a factor in the data that is ",
                       "passed to data.prep."))
      }
      if(is.null(enrichment)) {
      #de-mean covariate if continuous
      mean.cov <- mean(covariates, na.rm=TRUE)
      adata$covariate <- adata$covariate-mean.cov
      } else {adata$covariate <- covariates
             mean.cov <- NULL}
    } else if (is.factor(covariates) == TRUE || is.character(covariates) == TRUE) {
      #check that covariate has fewer than 3 levels and convert strings and factors to binary covariates
      if (length(unique(covariates)) > 2)
        stop("BUGSnet does not currently support meta-regression with categorical variables that have more than two levels.")
      if (length(unique(adata$covariate)) == 1)
        stop("Covariate should have more than one unique value.")
      if (is.character(covariates) == TRUE) {
        adata$covariate <- as.factor(adata$covariate)
        adata$covariate <- as.numeric(adata$covariate != levels(adata$covariate)[1])
        mean.cov <- 0
      }
    } else {stop("Invalid datatype for covariate.")}
  } else{mean.cov <- NULL}
  
  #Enrichment options 
  if(is.null(covariate) & !is.null(enrichment))stop("covariate must be specified when covariate as method for enrichment is chosen")
  if(!is.null(enrichment) & effects!="random")stop("enrichment method only works for random-effect model")
  if(!is.null(enrichment) & !(family %in% c("binomial", "binary", "bin", "binom")))stop("Enrichment method can only be chosen for binary outcome")
  
  if(!is.null(enrichment)){if(enrichment == "prior" & is.null(prior.ww))stop("prior.ww must be specified when enrichment is prior")}
  if(is.null(enrichment) & !is.null(prior.ww))stop("enrichment must be specified as prior when prior.ww is specified")
  if(!is.null(enrichment)){if(enrichment == "covariate" & !is.null(prior.ww))stop("enrichment must be specified as prior when prior.ww is specified")}
  if(!is.null(prior.ww)){
    if(!(prior.ww %in% c("dunif(0,0.3)", "dunif(0.3, 0.7)", "dunif(0.7,1)"))){
      stop("prior.ww must be either dunif(0,0.3), dunif(0.3, 0.7) or dunif(0.7,1)")
    }
  }
  
  
    # determine number of treatments in dataset
  nt <- length(unique(trts))
  
  #generate BUGS data object for arm-based data
  
  bugstemp <- adata %>% arrange(trial, trt.jags) %>% group_by(trial) %>% mutate(arm = row_number()) %>%
    ungroup() %>% select(-trt) %>% gather("variable", "value", -trial, -arm) %>% spread(arm, value)
  bugsdata2 <- list()
  for (v in unique(bugstemp$variable))
    bugsdata2[[v]] <- as.matrix(bugstemp %>% filter(variable == v) %>% select(-trial, -variable))
  #modify BUGS arm object for the various family/link combinations
  names(bugsdata2)[names(bugsdata2) == "trt.jags"] <- "t_a"
  names(bugsdata2)[names(bugsdata2) == "N"] <- "n"
  names(bugsdata2)[names(bugsdata2) == "covariate"] <- "x_a"
  if (family == "binomial" && link %in% c("log","logit")){
    names(bugsdata2)[names(bugsdata2) == "r1"] <- "r"
    bugsdata2 <- bugsdata2[names(bugsdata2) %in% c("ns_a", "nt", "na_a", "r", "n", "t_a", "x_a")]
  } else if (family == "normal" && link=="identity"){
    names(bugsdata2)[names(bugsdata2) == "r1"] <- "y"
    bugsdata2$se <- bugsdata2$sd / sqrt(bugsdata2$n)
    bugsdata2 <- bugsdata2[names(bugsdata2) %in% c("ns_a", "nt", "na_a", "y", "se", "t_a", "x_a")]
  } else if (family == "poisson" && link == "log"){
    names(bugsdata2)[names(bugsdata2) == "r1"] <- "r"
    names(bugsdata2)[names(bugsdata2) == "timevar"] <- "E"
    bugsdata2 <- bugsdata2[names(bugsdata2) %in% c("ns_a", "nt", "na_a", "r", "E", "t_a", "x_a")]
  } else if (family == "binomial" && link == "cloglog"){
    names(bugsdata2)[names(bugsdata2) == "r1"] <- "r"
    names(bugsdata2)[names(bugsdata2) == "timevar"] <- "time"
    bugsdata2 <- bugsdata2[names(bugsdata2) %in% c("ns_a", "nt", "na_a", "r", "n", "t_a", "x_a", "time")]
  }
# <<<<<<< HEAD
# =======
#   
#   #add number of treatments, studies, and arms to BUGS data object
#   bugsdata2$nt <- data$treatments %>% nrow()
#   bugsdata2$ns <- data$studies %>% nrow()
#   bugsdata2$na <- data$n.arms %>% select(n.arms) %>% t() %>% as.vector
# >>>>>>> upstream/master
  
  bugsdata2$ns_a <- data$studies %>% nrow()
  bugsdata2$na_a <- data$n.arms %>% select(n.arms) %>% t() %>% as.vector
  
  # # generate BUGS data object for cb data
  # 
  # if(contrast) {
  #   
  #   bugstemp2 <- cdata %>% arrange(trial) %>% group_by(trial) %>% mutate(arm = row_number()) %>% # changed
  #     ungroup() %>% select(-trt) %>% gather("variable", "value", -trial, -arm) %>% spread(arm, value)
  #   bugsdata3 <- list()
  #   
  #   for (v in unique(bugstemp2$variable))
  #     bugsdata3[[v]] <- as.matrix(bugstemp2 %>% filter(variable == v) %>% select(-trial, -variable))
  #   
  #   # modify BUGS contrast object and add treatment index, response, covariate, studies, number of arms to data
  #   
  #   names(bugsdata3)[names(bugsdata3) == "trt.jags"] <- "t_c"  
  #   names(bugsdata3)[names(bugsdata3) == "r1"] <- "y_c"
  #   names(bugsdata3)[names(bugsdata3) == "covariate"] <- "x_c"
  #   bugsdata3 <- bugsdata3[names(bugsdata3) %in% c("ns_c", "nt", "na_c", "y_c", "se.diffs", "var.ref", "t_c", "x_c")]
  #   
  #   bugsdata3$ns_c <- data_contrast$studies %>% nrow()
  #   bugsdata3$na_c <- data_contrast$n.arms %>% select(n.arms) %>% t() %>% as.vector
  #   
  #   # Data checking for contrast input
  #   
  #   # check that first arm difference is NA, change them to 0
  #   if(!all(is.na(bugsdata3$y_c[,1]))) {
  #     
  #     stop("The response in the first arm of each contrast-based trial should be NA.")
  #     
  #   } else {
  #     #convert to 0's
  #     bugsdata3$y_c[,1] <- 0
  #   }
  #   
  #   # check that first arm se is NA
  #   if(!all(is.na(bugsdata3$se.diffs[,1]))) {
  #     
  #     stop("The standard errors in the first arm of each contrast-based trial should be NA.")
  #     
  #   }
  #   
  #   # check if there are multi-arm trials, and if there are, check that var.ref is specified for the first arm
  #   if(!all(bugsdata3$na_c == 2)) {
  #     
  #     message("there are multi-arm trials")
  #     if(is.null(var.ref)) { 
  #       stop("var.ref must be specified if there are multi-arm contrast-based trials.") 
  #     } else {
  #       
  #       #check that only the first arm is specified
  #       if(!all(is.na(c(bugsdata3$var.ref[,-c(1)])))) {
  #         
  #         stop("Only the observed variances for the control arms for contrast-based trials should be included, all other arms should be NA")
  #         
  #       }  else if(!all(is.numeric(bugsdata3$var.ref[bugsdata3$na_c >2,1]))) { # make sure the control arms for all multi arm trials is numeric
  #         
  #         stop("Control arm observed variances for multi-arm conrtast-based trials must be numeric")
  #         
  #         bugsdata3$var.ref <- matrix(0, nrow = bugsdata3$ns_c, ncol = 2)
  #         
  #       }
  #       
  #     }
  #   } else {
  #     
  #     if(!is.null(var.ref)) { # if var.ref is specified when there are no multi-armed trials, set them all to 0 and print warning
  #       
  #       message("Control arm variances are not used for contrast-based trials with two arms.")
  #       
  #     }
  #     
  #     bugsdata3$var.ref <- matrix(0, nrow = bugsdata3$ns_c, ncol = 2) # set to zero to avoid compilation error
  #     
  #   }
  #   
  # } else {bugsdata3 <- data.frame()}
  
  
  
  # make legend for treatment names and numbering in jags program
  add.to.model <- trt.key %>%
    transmute(Treatments = paste0("# ", trt.jags, ": ", trt.ini, "\n")) %>%
    t() %>%
    paste0() %>%
    paste(collapse="") %>%
    paste0("\n\n# Treatment key\n",.)
  
  ############
  ###Priors###
  ############
  
  max.delta <- paste0(nma.prior(data, data_contrast=NULL, 
                                outcome=outcome, differences = differences,
                                scale=scale, N=N, sd=sd, time = time))
  
  # BASELINE EFFECTS PRIOR
  if (prior.mu == "DEFAULT"){
    prior.mu.str <- sprintf("for(i in 1:ns_a){
                               mu[i] ~ dnorm(0,(%s*15)^(-2))
                               }", max.delta)
  } else {
    prior.mu.str <- sprintf("for(i in 1:ns_a){
                               mu[i] ~ %s
  }", prior.mu)
  }
  
  # RELATIVE EFFECTS PRIOR
  if (prior.d =="DEFAULT"){
    if(type=="consistency"){
      prior.d.str <- sprintf("for(k in 2:nt){
                             d[k] ~ dnorm(0,(%s*15)^(-2))
    }", max.delta)
      
    } else if(type=="inconsistency"){
      prior.d.str <- sprintf("for (c in 1:(nt-1)) {
                           for (k in (c+1):nt)  {
                           d[c,k] ~ dnorm(0,(%s*15)^(-2))
                           d[k,c] <- d[c,k]
                           }
    }", max.delta)
    }
  } else {
    if(type=="consistency"){
      prior.d.str <- sprintf("for(k in 2:nt){
                             d[k] ~ %s
    }", prior.d)
      
    }else if(type=="inconsistency"){
      prior.d.str <- sprintf("for (c in 1:(nt-1)) {
                           for (k in (c+1):nt)  {
                           d[c,k] ~ %s
                           d[k,c] <- d[c,k]
                           }
  }", prior.d)
    }
  }
  
  # RANDOM EFFECTS VARIANCE PRIOR
  if (prior.sigma == "DEFAULT"){
    prior.sigma2.str <-  sprintf("sigma ~ dunif(0,%s)
                                 sigma2 <- sigma^2", max.delta)
  } else {
    prior.sigma2.str <-  sprintf("sigma ~ %s
                                 sigma2 <- sigma^2", prior.sigma)
  }
  
  ###hot fix for binomial family with log link.
  if(scale == "Risk Ratio"){
    prior.mu.str <-  "for(i in 1:ns_a){
     mu[i] <- log(p.base[i])           #priors for all trial baselines
     p.base[i] ~ dunif(0, 1)
   }"
  }
  
  #meta regression string --> can only use meta-regression when enrichment is null, otherwise meta-regression is used
  if (!is.null(covariate) & is.null(enrichment)){
    
    if (prior.beta=="UNRELATED"){
      prior.meta.reg <- sprintf("beta[1]<-0
    for (k in 2:nt){
      beta[k] ~ dt(0, (%s)^(-2), 1)
    }", max.delta)
    }else if(prior.beta=="EXCHANGEABLE"){
      prior.meta.reg <- sprintf("beta[1]<-0
    for (k in 2:nt){
      beta[k] ~ dnorm(b, gamma^(-2))
    }
    b~dt(0, %s^(-2), 1)
    gamma~dunif(0, %s)", max.delta, max.delta)
    }else if(prior.beta=="EQUAL"){
      prior.meta.reg <- sprintf("beta[1]<-0
    for (k in 2:nt){
      beta[k] <- B
    }
    B~dt(0, %s^(-2), 1)", max.delta)
    }else {
      prior.meta.reg <- prior.beta
    }
  }else prior.meta.reg <- ""
  # #remove covariate from bugsdata2 if unused
  # if (is.null(covariate)){bugsdata2 <- bugsdata2[names(bugsdata2)!="x"]}
  
  if(!is.null(enrichment)){if(enrichment == "prior"){
        prior.ww.str <-  sprintf("ww ~ %s", prior.ww)}
    else prior.ww.str <- ""}
    else{
      prior.ww.str <-""}
    
  
  
  
  # make the code for the model
  
  model <- makeBUGScode(family=family,       ################ BUG seems to be here!!!!!!!!!!! Outputs have confirmed
                        link=link,
                        effects=effects,
                        inconsistency=(type=="inconsistency"),
                        enrichment = enrichment,
                        prior.mu.str,
                        prior.d.str,
                        prior.sigma2.str,
                        covariate,
                        prior.meta.reg,
                        prior.ww.str,
                        auto = FALSE, # for compatibility with auto-run function - can change this if the feature is added
                        arm = armdat,
                        contrast = contrast) %>%
    paste0(add.to.model)
  
  # if(!arm) {
  #   
  #   if(effects == "random") { # for random effects, need to specify ns_a=0
  #     
  #     bugsdata <- c(bugsdata3, nt=nt, ns_a = 0)
  #     
  #   } else {
  #     
  #     bugsdata <- c(bugsdata3, nt=nt) # fixed effects - don't specify ns_a=0 to avoid warning about unused variable from JAGS
  #     
  #   }
  #   
  #   
  # } else { # otherwise (just arm or both arm and contrast)
  #   
  #   bugsdata <- c(bugsdata2,bugsdata3, nt = nt)
  #   
  # }
  
  bugsdata <- c(bugsdata2, nt=nt)
  
  
  bmodel <- structure(list(bugs=model,
                           data=bugsdata,
                           scale=scale,
                           trt.key=trt.key,
                           family=family,
                           link=link,
                           type=type,
                           effects=effects,
                           enrichment = enrichment,
                           covariate=covariate,
                           prior.mu=prior.mu,
                           prior.d=prior.d,
                           prior.ww = prior.ww,
                           prior.sigma=prior.sigma,
                           prior.beta=prior.beta,
                           reference=reference,
                           time=time,
                           outcome=outcome,
                           N=N,
                           sd=sd,
                           mean.cov=mean.cov),
                      class = "BUGSnetModel")
  return(bmodel)
}

