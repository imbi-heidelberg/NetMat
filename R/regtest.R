##### Testing Regression



nsclc_regression_re <- nma.model(data=dataprep,
                                  outcome="event",
                                  N="n",
                                  reference="1",
                                  family="binomial",
                                  link="logit",
                                  effects="random",
                                  covariate="x",
                                  prior.beta="EXCHANGEABLE")




random_effects_results <- nma.run(nsclc_regression_re,
                                  n.adapt=1000,
                                  n.burnin=1000,
                                  n.iter=5000)

s_re_results <- summary(random_effects_results$samples)
tbl_reg_re_results <- cbind(random_effects_results$statistics, 
                              random_effects_results$quantiles)
