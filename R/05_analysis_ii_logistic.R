# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
gordon_clean_aug <- readRDS(file = "data/03_gordon_100.rds")


# Wrangle data ------------------------------------------------------------
gordon_clean_aug %>% ...


# Model data ------------------------------------------------------------
## Logistic regression model for each of the probes
gordon_logistic <- gordon_clean_aug %>% 
  mutate(mdl = map(data, ~ glm(outcome ~ value, 
                               data = .x,
                               family = binomial(link = "logit"))))

## Adding statistical variables for intercept and the estimated values for each model
## (std.error, statistic,  p.value, conf.low, conf.high)
gordon_logistic <- gordon_logistic %>% 
  mutate(mdl_tidy = map(mdl, tidy, conf.int = TRUE)) %>% 
  unnest(mdl_tidy)

## As we are not interested in the intercept for the values, we remove these rows
gordon_logistic <- gordon_logistic %>% 
  filter(term == "value")

## Identify significant and non-significant genes
gordon_logistic <- gordon_logistic %>% 
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   TRUE ~ "Non-significant"))


# Visualise data ----------------------------------------------------------
gordon_logistic %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)