# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
gordon_clean_aug <- read_tsv(file = "data/03_gordon_clean_aug.tsv.gz")
gordon_100 <- readRDS(file = "data/03_gordon_100.rds")


# Wrangle data  -------------------------------------------------------------
## Get the 100 random genes and make outcome a factor variable
gordon_wide <- gordon_clean_aug %>% 
  select(outcome, pull(gordon_100, probe)) %>% 
  mutate(outcome = as_factor(outcome)) 


# PCA analysis ---------------------------------------------------------------
pca_fit <- gordon_wide %>% 
  select(where(is.numeric)) %>% # only numeric columns
  prcomp(scale = TRUE) # PCA on scaled data


# Visualise data ----------------------------------------------------------

## Plot data in PC coordinates
pca_fit %>%
  augment(gordon_wide) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = outcome)) + 
  geom_point(size = 1.5, alpha()) + 
  theme_minimal() + 
  



# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)