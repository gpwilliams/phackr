library(tidyverse)
library(here)
library(gganimate)
library(gifski)
library(scales)

source(here("functions.R"))

# simulate data ----

hacked_p <- tibble(
  additional_tests = 0:30, 
  p_vals = numeric(31)
)
n <- 1000

for(i in seq_along(hacked_p$additional_tests)) {
  ps <- replicate(n = n, p_hack(hacked_p$additional_tests[i]))
  hacked_p$p_vals[i] <- mean(ps < .05) 
}

# make animation ----

sim_animation <- 
  ggplot(hacked_p, aes(x = additional_tests, y = p_vals, group = 1)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.05), 
    labels = percent(seq(0, 1, 0.05))
  ) +
  geom_hline(yintercept = 0.05, linetype = 2, size = 1) +
  labs(
    title = "False Positive Rates from Uncorrected Sequential Testing.",
    subtitle = "Simulation Count: {frame}", 
    x = "Number of Additional Blocks of 10 Participants Tested", 
    y = "Percentage of False Positives"
  ) +
  transition_manual(additional_tests, cumulative = TRUE) +
  ease_aes("linear")

animated <- animate(sim_animation, end_pause = 15, rendered = gifski_renderer())
anim_save(here("output", "p_hack_rates.gif"), animation = animated)
