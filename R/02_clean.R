# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
gordon_x <- read_tsv(file = "data/01_gordon_x.tsv.gz")
gordon_y <- read_tsv(file = "data/01_gordon_y.tsv.gz")


# Wrangle data ------------------------------------------------------------
gordon_clean <- bind_cols(gordon_x, gordon_y)


# Write data --------------------------------------------------------------
write_tsv(x = gordon_clean,
          file = "data/02_gordon_clean.tsv.gz")