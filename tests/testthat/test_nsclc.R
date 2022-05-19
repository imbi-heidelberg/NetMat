
#context("NSCLC")

#tc <- textConnection(NULL, "w")
#sink(tc)

library(BUGSnet)

set.seed(1234)

dataprep <- data.prep(arm.data = nsclc,
                      varname.t = "treatment",
                      varname.s = "study")



enrichment_covariate_model <- nma.model(data=dataprep,
                                        outcome="event",
                                        N="n",
                                        reference="Chemo",
                                        family="binomial",
                                        link="logit",
                                        effects="random",
                                        covariate="x",
                                        enrichment = "covariate")



enrichment_covariate_results <- nma.run(enrichment_covariate_model,
                                        n.adapt = 1000,
                                        n.burnin = 1000,
                                        n.iter = 5000)

s_enrichment_cov <- summary(enrichment_covariate_results$samples[,2:5])
tbl_enrichment_cov <- cbind(s_enrichment_cov$statistics[,1:2], s_fe$quantiles[,c(3,1,5)])






########################benchmarking prior##########################################



enrichment_prior_model <- nma.model(data = dataprep,
                                    outcome = "event",
                                    N = "n",
                                    reference = "Chemo",
                                    family = "binomial",
                                    link = "logit",
                                    effects = "random",
                                    covariate = "x",
                                    prior.beta = "EXCHANGEABLE",
                                    enrichment = "prior",
                                    prior.ww = "dunif(0,0.3)")




enrichment_prior_results <- nma.run(enrichment_prior_model,
                                    n.adapt=1000,
                                    n.burnin=1000,
                                    n.iter=5000)


s_enrichment_p <- summary(enrichment_prior_results$samples[,2:5])
tbl_enrichment_p <- cbind(s_enrichment_p$statistics[,1:2], s_fe$quantiles[,c(3,1,5)])


benchmark <- 
rownames(benchmark) <- NULL

test_that("nma.run results match benchmark", { expect_equal(benchmark, tbl_enrichment_p) })


#results.league <- nma.league(enrichment_covariate_results)

#results.table <- results.league$table
#results <- summary(enrichment_covariate_results$samples)



#TSD2 Example 5 Fixed Effects Model
model_fe <- nma.model(dat, outcome = "y", N = "n", sd = "sd", reference = "1", family = "normal",
                      link = "identity", effects = "fixed")
results_fe <- nma.run(model_fe, n.adapt = 5000, n.burnin = 50000, n.iter = 100000)

s_fe <- summary(results_fe$samples[,2:5])
tbl_fe <- cbind(s_fe$statistics[,1:2], s_fe$quantiles[,c(3,1,5)])

#TSD2 Example 5 Random Effects Model
model_re <- nma.model(dat, outcome = "y", N = "n", sd = "sd", reference = "1", family = "normal",
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
