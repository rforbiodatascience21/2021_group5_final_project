# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
gordon_long_nested <- readRDS(file = "data/03_gordon_100.rds")


# Wrangle data ------------------------------------------------------------


# Model data ------------------------------------------------------------
## Logistic regression model for each of the probes
gordon_logistic <- gordon_long_nested %>% 
  mutate(mdl = map(data, ~ glm(outcome ~ value, 
                               data = .x,
                               family = binomial(link = "logit"))))

## Adding statistical variables for intercept and the estimated values for each model
## (std.error, statistic,  p.value, conf.low, conf.high)
gordon_logistic <- gordon_logistic %>% 
  mutate(mdl_tidy = map(mdl, tidy, conf.int = TRUE)) %>% 
  unnest(mdl_tidy)

## As we are not interested in the intercept for the values, we remove these rows
gordon_logistic <- gordon_logistic %>% 
  filter(term == "value")

## Identify significant and non-significant genes
gordon_logistic <- gordon_logistic %>% 
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   TRUE ~ "Non-significant"))


# Visualise data ----------------------------------------------------------
## Make a Manhattan-plot for illustrating p-values for probe association
gordon_logistic <- gordon_logistic%>%
  mutate(neg_log10_p = -log10(p.value))

plot1 <- gordon_logistic %>% 
  ggplot(mapping = aes(x = fct_reorder(probe, neg_log10_p, .desc = TRUE), 
                       y = neg_log10_p,
                       color = identified_as)) +
  geom_point() + 
  geom_hline(yintercept = - log10(0.05), linetype = "dashed") +
  theme_classic(base_size = 8, base_family = "Avenir") +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45 , vjust = 1, hjust = 1)) +
  labs( x = "Probe", y = "Minus log10(p)")

## A confidence interval plot with effct directions
plot2 <- gordon_logistic %>% 
  ggplot(mapping = aes(x = estimate, y = fct_reorder(probe, estimate, .desc = TRUE),
                       color = identified_as)) + 
  geom_point() + 
  geom_vline(xintercept = 0, linetype = "dashed" ) + 
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     height = 0.1)) +
  theme_classic(base_size = 8, base_family = "Avenir") +
  theme(legend.position = "bottom" ) +
  labs( x = "Estimate", y = "Probe")

## A confidence interval plot only including genes with a significant association
plot3 <- gordon_logistic %>% 
  filter(identified_as == "Significant") %>% 
  ggplot(mapping = aes(x = estimate, y = fct_reorder(probe, estimate, .desc = TRUE))) + 
  geom_point() + 
  geom_vline(xintercept = 0, linetype = "dashed" ) + 
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     height = 0.1)) +
  theme_classic(base_size = 8, base_family = "Avenir") +
  theme(legend.position = "bottom" ) +
  labs( x = "Estimate", y = "Probe")


# Write data --------------------------------------------------------------
saveRDS(object  = gordon_logistic,
        file = "results/05_gordon_logistic.rds")
ggsave(filename = "results/05_logistic_manhattan.png", plot = plot1)
ggsave(filename = "results/05_logistic_interval.png", plot = plot2)
ggsave(filename = "results/05_logistic_significant.png", plot = plot3)