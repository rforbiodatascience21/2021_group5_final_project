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

## Extract rotation matrix
pca_fit %>%
  tidy(matrix = "rotation")

# Visualise data ----------------------------------------------------------

## Plot data in PC coordinates
plt1 <- pca_fit %>%
  augment(gordon_wide) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = outcome)) + 
  geom_point(size = 1.5, alpha = 0.4) + 
  theme_minimal()

# Define arrow style for plotting
arrow_style <- arrow(angle = 20, 
                     ends = "first", 
                     type = "closed", 
                     length = grid::unit(8, "pt"))

## Plot of rotation matrix  
plt2 <- pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", 
              names_prefix = "PC", 
              values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  xlim(-.3, .2) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme_minimal()

## Plot of variance explained by first 10 PCs
plt3 <- pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  filter(percent > 0.024) %>% 
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  )


# Write data --------------------------------------------------------------
write_tsv(x = gordon_wide,
          file = "data/06_gordon_wide.tsv.gz")
ggsave(filename = "results/06_plot_PCA_PCcoords.png",
       plot = plt1)
ggsave(filename = "results/06_plot_PCA_rotation.png",
       plot = plt2)
ggsave(filename = "results/06_plot_PCA_varExpl.png",
       plot = plt3)
