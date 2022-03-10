library(devtools)
library(readxl)
library(magrittr)
library(dplyr)

# dietfat <- read_excel("data-raw/rate_example.xlsx")
# dietfat <- as.data.frame(dietfat)
# use_data(dietfat, overwrite=TRUE) #rate

diabetes.sim <- read_excel("data-raw/rate2_example.xlsx")
diabetes.sim <- as.data.frame(diabetes.sim)
use_data(diabetes.sim, overwrite=TRUE) #rate2
diabetes <- diabetes.sim[,1:6]
use_data(diabetes, overwrite=TRUE)

library(gemtc)
data(thrombolytic)
thrombolytic <- thrombolytic$data.ab %>%
  rename(events=responders)
use_data(thrombolytic, overwrite=TRUE) #dichotomous

data(atrialFibrillation)
afib <- atrialFibrillation$data.ab %>%
  left_join(atrialFibrillation$studies, by="study") %>%
  rename(events=responders)
use_data(afib, overwrite=TRUE) #metareg


library(readxl)
data_bsp <- Daten_Beispiel <- read_excel(paste0(getwd(),"/data-raw/Daten_Beispiel.xlsx"))
library(tidyverse)
nsclc <- data_bsp %>%
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

use_data(nsclc, overwrite=TRUE)


