
#context("NSCLC")

#tc <- textConnection(NULL, "w")
#sink(tc)

library(BUGSnet)

dataprep <- data.prep(arm.data = nsclcdata,
                      varname.t = "treatment",
                      varname.s = "study")

set.seed(1234)
seeds <- sample.int(4, n = .Machine$integer.max)
enrichment_covariate_model <- nma.model(data = dataprep,
                                        outcome = "event",
                                        N = "n",
                                        reference = "1",
                                        family = "binomial",
                                        link = "logit",
                                        effects = "random",
                                        covariate = "x",
                                        enrichment = "covariate")



model$inits <- mapply(c, model$inits, list(
  list(.RNG.name="base::Wichmann-Hill", .RNG.seed=seeds[1]),
  list(.RNG.name="base::Marsaglia-Multicarry", .RNG.seed=seeds[2]),
  list(.RNG.name="base::Super-Duper", .RNG.seed=seeds[3]),
  list(.RNG.name="base::Mersenne-Twister", .RNG.seed=seeds[4])), SIMPLIFY=FALSE)

enrichment_covariate_results <- nma.run(enrichment_covariate_model,
                                        n.adapt = 1000,
                                        n.burnin = 10000,
                                        n.iter = 100000)

s_enrichment_cov <- summary(enrichment_covariate_results$samples[,2:5])
tbl_enrichment_cov <- cbind(s_enrichment_cov$statistics[1:2,1:2], 
                            s_enrichment_cov$quantiles[1:2,c(3,1,5)])



########################benchmarking prior Unif(0, 0.3)##########################################

set.seed(1234)
seeds <- sample.int(4, n = .Machine$integer.max)

enrichment_prior_model <- nma.model(data = dataprep,
                                    outcome = "event",
                                    N = "n",
                                    reference = "1",
                                    family = "binomial",
                                    link = "logit",
                                    effects = "random",
                                    covariate = "x",
                                    prior.beta = "EXCHANGEABLE",
                                    enrichment = "prior",
                                    prior.ww = "dunif(0,0.3)")


enrichment_prior_model$inits <- mapply(c, enrichment_prior_model$inits, list(
  list(.RNG.name="base::Wichmann-Hill", .RNG.seed=seeds[1]),
  list(.RNG.name="base::Marsaglia-Multicarry", .RNG.seed=seeds[2]),
  list(.RNG.name="base::Super-Duper", .RNG.seed=seeds[3]),
  list(.RNG.name="base::Mersenne-Twister", .RNG.seed=seeds[4])), SIMPLIFY=FALSE)

enrichment_prior_results <- nma.run(enrichment_prior_model,
                                    n.adapt=1000,
                                    n.burnin=10000,
                                    n.iter=50000)


s_enrichment_p <- summary(enrichment_prior_results$samples[,2:5])
tbl_enrichment_p <- cbind(s_enrichment_p$statistics[1:2,1:2], 
                          s_enrichment_p$quantiles[1:2,c(3,1,5)])



########################benchmarking prior Unif(0.3, 0.7)##########################################

set.seed(1234)
seeds <- sample.int(4, n = .Machine$integer.max)

enrichment_prior_model0307 <- nma.model(data = dataprep,
                                    outcome = "event",
                                    N = "n",
                                    reference = "1",
                                    family = "binomial",
                                    link = "logit",
                                    effects = "random",
                                    covariate = "x",
                                    prior.beta = "EXCHANGEABLE",
                                    enrichment = "prior",
                                    prior.ww = "dunif(0.3, 0.7)")


enrichment_prior_model0307$inits <- mapply(c, enrichment_prior_model0307$inits, list(
  list(.RNG.name="base::Wichmann-Hill", .RNG.seed=seeds[1]),
  list(.RNG.name="base::Marsaglia-Multicarry", .RNG.seed=seeds[2]),
  list(.RNG.name="base::Super-Duper", .RNG.seed=seeds[3]),
  list(.RNG.name="base::Mersenne-Twister", .RNG.seed=seeds[4])), SIMPLIFY=FALSE)

enrichment_prior_results_0307 <- nma.run(enrichment_prior_model0307,
                                    n.adapt=1000,
                                    n.burnin=10000,
                                    n.iter=50000)


s_enrichment_p0307 <- summary(enrichment_prior_results_0307$samples[,2:5])
tbl_enrichment_p0307 <- cbind(s_enrichment_p0307$statistics[1:2,1:2], 
                          s_enrichment_p0307$quantiles[1:2,c(3,1,5)])


########################benchmarking prior Unif(0.7, 1)##########################################

set.seed(1234)
seeds <- sample.int(4, n = .Machine$integer.max)

enrichment_prior_model071 <- nma.model(data = dataprep,
                                        outcome = "event",
                                        N = "n",
                                        reference = "1",
                                        family = "binomial",
                                        link = "logit",
                                        effects = "random",
                                        covariate = "x",
                                        prior.beta = "EXCHANGEABLE",
                                        enrichment = "prior",
                                        prior.ww = "dunif(0.7,1)")


enrichment_prior_model071$inits <- mapply(c, enrichment_prior_model071$inits, list(
  list(.RNG.name="base::Wichmann-Hill", .RNG.seed=seeds[1]),
  list(.RNG.name="base::Marsaglia-Multicarry", .RNG.seed=seeds[2]),
  list(.RNG.name="base::Super-Duper", .RNG.seed=seeds[3]),
  list(.RNG.name="base::Mersenne-Twister", .RNG.seed=seeds[4])), SIMPLIFY=FALSE)

enrichment_prior_results_071 <- nma.run(enrichment_prior_model071,
                                         n.adapt=1000,
                                         n.burnin=10000,
                                         n.iter=50000)


s_enrichment_p071 <- summary(enrichment_prior_results_071$samples[,2:5])
tbl_enrichment_p071 <- cbind(s_enrichment_p071$statistics[1:2,1:2], 
                          s_enrichment_p071$quantiles[1:2,c(3,1,5)])

#round and append tables
results <- as.data.frame(round(rbind(tbl_enrichment_cov, 
                                     tbl_enrichment_p,
                                     tbl_enrichment_p0307,
                                     tbl_enrichment_p071), 2))
rownames(results) <- NULL


benchmark <- BUGSnet:::nsclc_test_new$results[,c(1,2,5,3,7)]
benchmark1 <- round(benchmark,2)
  rownames(benchmark) <- NULL

test_that("nma.run results match benchmark", { expect_equal(benchmark1, results) })



#TSD2 Example 5 Fixed Effects Model
model_fe <- nma.model(dat, outcome = "y", N = "n", sd = "sd", reference = "1", family = "normal",
                      link = "identity", effects = "fixed")
results_fe <- nma.run(model_fe, n.adapt = 5000, n.burnin = 50000, n.iter = 100000)

s_fe <- summary(results_fe$samples[,2:5])
tbl_fe <- cbind(s_fe$statistics[,1:2], 
                s_fe$quantiles[,c(3,1,5)])

#TSD2 Example 5 Random Effects Model
model_re <- nma.model(dat, outcome = "y", N = "n", sd = "sd", 
                      reference = "1", family = "normal",
                      link = "identity", effects = "random")
results_re <- nma.run(model_re, n.adapt = 5000, n.burnin = 50000, n.iter = 100000)
s_re <- summary(results_re$samples[,2:5])
tbl_re <- cbind(s_re$statistics[,1:2], s_re$quantiles[,c(3,1,5)])

#round and append tables
results <- as.data.frame(round(rbind(tbl_fe, tbl_re), 2))
rownames(results) <- NULL

sink()
close(tc)

benchmark <- BUGSnet:::tsd2ex5$bugsnet[,3:7]
rownames(benchmark) <- NULL

test_that("nma.run results match benchmark", { expect_equal(benchmark, results) })
