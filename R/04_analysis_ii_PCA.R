# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
gordon_clean_aug <- readRDS(file = "data/03_gordon_100.rds")

# Model data  -------------------------------------------------------------
## Logistic regression model for each of the probes
gordon_clean_aug <- gordon_clean_aug %>% 
  mutate(mdl = map(data, ~ glm(outcome ~ value, 
                               data = .x,
                               family = binomial(link = "logit"))))

## Adding statistical variables for intercept and the estimated values for each model
## (std.error, statistic,  p.value, conf.low, conf.high)
gordon_clean_aug <- gordon_clean_aug %>% 
  mutate(mdl_tidy = map(mdl, tidy, conf.int = TRUE)) %>% 
  unnest(mdl_tidy)

## As we are not interested in the intercept for the values, we remove these rows
gordon_clean_aug <- gordon_clean_aug %>% 
  filter(term == "value")

## Identifying weather the genes are significant or not, based on the P-value
gordon_clean_aug <- gordon_clean_aug %>% 
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   TRUE ~ "Non-significant"))

# PCA analysis ---------------------------------------------------------------
## Selecting the same 100 genes as previous 
gordon_data_wide <- gordon_clean_aug %>% select(outcome, pull(gordon_100, probe))

gordon_data_wide <- gordon_data_wide %>% 
  mutate(outcome = as_factor(outcome)) #why is this necessary? 

pca_fit <- gordon_data_wide %>% 
  select(where(is.numeric)) %>% 
  prcomp(scale = TRUE)

pca_fit %>%
  augment(gordon_data_wide) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = outcome)) + 
  geom_point(size = 1.5) + 
  theme_minimal() 
