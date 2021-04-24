# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
load(file = "data/_raw/gordon.RData")


# Wrangle data ------------------------------------------------------------
gordon_x <- gordon %>% 
  pluck("x") %>% 
  as_tibble

gordon_y <- gordon %>% 
  pluck("y") %>% 
  as_tibble


# Write data --------------------------------------------------------------
write_tsv(x = gordon_x,
          file = "data/01_gordon_x.tsv.gz")
write_tsv(x = gordon_y, 
          file = "data/01_gordon_y.tsv.gz")
