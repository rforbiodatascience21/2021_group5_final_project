# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
gordon_clean <- read_tsv(file = "data/02_gordon_clean.tsv.gz")


# Wrangle data ------------------------------------------------------------
gordon_clean_aug <- gordon_clean %>%
  mutate(outcome = case_when(value == "mesothelioma" ~ 0,
                             value == "adenocarcinoma" ~ 1)) %>% 
  relocate(c(value, outcome)) %>% 
  rename(response = value)

# Subset data -------------------------------------------------------------
set.seed(1234)
gordon_100 <- gordon_clean_aug %>% 
  sample_n(100)


# Write data --------------------------------------------------------------
write_tsv(x = gordon_clean_aug,
          file = "data/03_gordon_clean_aug.tsv.gz")
saveRDS(object  = gordon_100,
          file = "data/03_gordon_100.rds")
