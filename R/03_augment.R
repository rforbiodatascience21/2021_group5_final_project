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
gordon_long_nested <- gordon_clean_aug %>%
  select(-response) %>% #can't combine <chr> and <dbl>
  pivot_longer(cols = -outcome, 
               names_to = "probe", 
               values_to = "value") %>%
  group_by(probe) %>%
  nest() %>%
  ungroup() #why do we ungroup? there is no difference in the data setup, from just nest()

set.seed(1234)
gordon_100 <- gordon_long_nested %>% 
  sample_n(100)

# Write data --------------------------------------------------------------
write_tsv(x = gordon_clean_aug,
          file = "data/03_gordon_clean_aug.tsv.gz")
saveRDS(object = gordon_long_nested,
          file = "data/03_gordon_long_nested.rds")
saveRDS(object  = gordon_100,
          file = "data/03_gordon_100.rds")
