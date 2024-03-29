Package: BugsNet
Title: Bayesian network meta-analyses for targeted and non-targeted therapies
Version: 1.1.1
Authors@R: c(
  person("Audrey", "Beliveau", email = "audrey.beliveau@uwaterloo.ca", role = c("aut","cph", "cre")),
  person("Justin", "Slater", email = "jslater@lighthouseoutcomes.com", role = c("aut")),
  person("Devon", "Boyne", email = "devon.boyne1@ucalgary.ca", role = c("aut", "cph")),
  person("Eric", "Mackay", email = "emackay@lighthouseoutcomes.com", role = c("ctb")),
  person("Lighthouse Outcomes Inc.", role=c("cph")),
  person("Harmohit Singh", "Bindra", email = "hsbindra@uwaterloo.ca", role = c("ctb")),
  person("Augustine", "Wigle", email = "amhwigle@uwaterloo.ca", role = c("ctb")),
  person("Tanja", "Proctor", email = "proctor@imbi.uni-heidelberg.de", role = c("ctb")),
  person("Samuel", "Zimmermann", email = "zimmermann@imbi.uni-heidelberg.de", role =c("ctb"))
) 
Description: The NetMat package is an extension of the BUGSnet package.
BUGSnet (Bayesian inference Using Gibbs Sampling to conduct 
         NETwork meta-analysis) is a feature-rich R package to conduct Bayesian 
network meta-analyses in compliance with best practice and reporting 
guidelines. Bayesian analyses are conducted with JAGS. Outputs are 
highly customizable and include network plots, tables of network 
characteristics, league tables and league heat plots, SUCRA plots, 
rankograms, forest plots, leverage plots, traceplots, posterior mean 
deviance comparison plots.
Depends: R (>= 3.6),
rjags (>= 4.9)
License: CC BY-NC-SA 4.0
Encoding: UTF-8
LazyData: yes
RoxygenNote: 7.1.1
Imports:
  tidyr (>= 1.0),
plyr (>= 1.8.4),
ggplot2 (>= 3.2.1),
igraph (>= 1.2.4.1),
extrafont (>= 0.17),
magrittr (>= 1.5),
dplyr (>= 0.8.3),
meta (>= 5.0.0),
purrr (>= 0.3.3),
scales (>= 1.0.0),
RColorBrewer (>= 1.1.2),
Rdpack (>= 0.11.0),
graphics (>= 3.6.1),
utils (>= 3.6.1),
rlang (>= 0.4.1),
tibble (>= 2.1.3),
stringr (>= 1.4.0),
gridExtra (>= 2.3),
mcmcr
Suggests: 
  knitr (>= 1.25),
rmarkdown (>= 1.16),
devtools (>= 2.2.1),
roxygen2 (>= 6.1.1),
testthat (>= 2.2.1)
VignetteBuilder: knitr
Roxygen: list(markdown = TRUE, roclets = c("rd", "namespace", "collate"))
RdMacros: Rdpack
