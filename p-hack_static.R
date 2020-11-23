# static plot ----

library(tidyverse)
library(scales)
library(here)

source(here("functions.R"))

# simulate data ----

# sample the data at various numbers of additional tests
# then run p-hacked tests
hacked_p <- tibble(
  additional_tests = 0:30, 
  p_vals = numeric(31)
)
n <- 1000

for(i in seq_along(hacked_p$additional_tests)) {
  ps <- replicate(n = n, p_hack(hacked_p$additional_tests[i]))
  hacked_p$p_vals[i] <- mean(ps < .05) 
}

# make plot ----

p_hack_rates <- ggplot(hacked_p, aes(x = additional_tests, y = p_vals)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.05), 
    labels = percent(seq(0, 1, 0.05))
  ) +
  labs(
    x = "Number of Additional Blocks of 10 Participants Tested",
    y = "Percentage of False Positives",
    title = "False Positive Rates from Uncorrected Sequential Testing.",
    subtitle = paste(n, "Iterations Per Test.")
  ) +
  theme_bw()

ggsave(
  here("output", "p_hack_rates.png"), 
  p_hack_rates, 
  height = 8,
  width = 12
)
