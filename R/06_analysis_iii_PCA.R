# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
gordon_clean_aug <- readRDS(file = "data/03_gordon_100.rds")


# Wrangle data  -------------------------------------------------------------
gordon_wide <- gordon_clean_aug %>% 
  select(outcome, pull(gordon_100, probe))

gordon_data_wide <- gordon_data_wide %>% 
  mutate(outcome = as_factor(outcome)) #why is this necessary? 


# PCA analysis ---------------------------------------------------------------
pca_fit <- gordon_data_wide %>% 
  select(where(is.numeric)) %>% 
  prcomp(scale = TRUE)

pca_fit %>%
  augment(gordon_data_wide) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = outcome)) + 
  geom_point(size = 1.5) + 
  theme_minimal() 
