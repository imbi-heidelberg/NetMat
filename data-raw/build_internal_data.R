
#############################################################
# Description: builds internal use R/sysdata.rda data file  #
#   which is used to implement bugsnet.test                 #
#                                                           #
#############################################################

tsd2ex5data <- read.csv(paste0(getwd(), "/data-raw/TSD2_ex5_parkinsons.csv"), 
                    stringsAsFactors = FALSE)
tsd2ex5results <- read.csv(paste0(getwd(), "/data-raw/TSD2_ex5_parkinsons_results.csv"), 
                       stringsAsFactors = FALSE)
tsd2ex5bugsnet <- read.csv(paste0(getwd(), "/data-raw/TSD2_ex5_parkinsons_bugsnet.csv"), 
                           stringsAsFactors = FALSE, check.names = F)
tsd2ex5 <- list(data = tsd2ex5data, results = tsd2ex5results, bugsnet = tsd2ex5bugsnet)
usethis::use_data(tsd2ex5, internal = TRUE)


# Add data for extension enrichment-through weighting, how to do it internal?
library(readxl)
data_bsp <- Daten_Beispiel <- read_excel(paste0(getwd(),"/Daten_Beispiel.xlsx"))
library(tidyverse)
nsclc_data <- data_bsp %>%
  select(study, treament, control,response_treat, response_cont, n_treat, n_cont, cov) %>%
  rename(
    treatment_1 = "treament",
    treatment_2 = "control",
    event_1 = "response_treat",
    event_2 = "response_cont",
    n_1 = "n_treat",
    n_2 = "n_cont",
    x = "cov"
  ) %>%
  pivot_longer(c(2:7),
               names_to = c(".value", "set"),
               names_pattern = "(.+)_(.)")

save(nsclc_data, file="nsclc.rda")

