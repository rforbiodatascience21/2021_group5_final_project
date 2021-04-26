# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
gordon_clean_aug <- read_tsv(file = "data/03_gordon_clean_aug.tsv.gz")
gordon_100 <- readRDS(file = "data/03_gordon_100.rds")

# Wrangle data ------------------------------------------------------------
## Get the 100 random genes and make outcome a factor variable
gordon_wide <- gordon_clean_aug %>% 
  select(outcome, pull(gordon_100, probe)) %>% 
  mutate(outcome = as_factor(outcome)) 


# K-means clustering ------------------------------------------------------


pca_fit %>%
  augment(gordon_wide) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2)) + 
  geom_point(size = 1.5, alpha = 0.4) + 
  theme_minimal()

points <- pca_fit %>% augment(gordon_wide) %>% select(.fittedPC1, .fittedPC2)


kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )
kclusts

clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))



# Visualise data ----------------------------------------------------------
## Nine plots with varying number of clusters k=1..9
plt1 <- ggplot(assignments, aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)

## With centers of clusters - don't work for me yet
plt2 <- plt1 + geom_point(data = clusters, size = 10, shape = "x")

## Variance within clusters
ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)