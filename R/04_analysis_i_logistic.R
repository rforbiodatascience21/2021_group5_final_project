# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- readRDS(file = "data/gordon_100.rds")

# Investigating data  -----------------------------------------------------

gordon_clean_aug %>% group_by(response) %>% summarise(n = n())

# Wrangle data ------------------------------------------------------------
my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)