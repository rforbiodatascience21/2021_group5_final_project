# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
my_data_clean_aug <- readRDS(file = "data/gordon_100.rds")

# Model data  -------------------------------------------------------------
my_data_clean_aug %>% ...