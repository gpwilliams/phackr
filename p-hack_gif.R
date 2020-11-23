library(tidyverse)
library(here)
library(gganimate)
library(gifski)

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

sim_contrasts <- sim_contrasts %>%
  group_by(contrast) %>%
  mutate(sig = cumsum(p.value < .05)/1000)

sim_animation <- 
  ggplot(hacked_p, aes(x = additional_tests, y = p_vals, group = 1)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  geom_hline(yintercept = 0.05, linetype = 2, size = 1) +
  labs(title = "Simulation Count: {frame}", x = "Additional Tests", y = "False Positive") +
  transition_manual(additional_tests, cumulative = TRUE) +
  ease_aes("linear")

animated <- animate(sim_animation, end_pause = 15, rendered = gifski_renderer())
anim_save(here("output", "p_hack_rates.gif"), animation = animated)
