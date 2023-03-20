library(tidyverse)

set.seed(58)
tbl_data = tibble(
  i = factor(1:1000),
) %>% 
  mutate(
    Ct_peak = rnorm(n(), 17.8, 2.2),
    down_slope = pmax(rnorm(n(), 1.7, 1.7/4), 0.01),
  ) %>% 
  expand_grid(
    t = 0:15,
  ) %>% 
  mutate(
    true_ct = Ct_peak + down_slope * t,
    obs_ct = rnorm(n(), true_ct, 3),
  ) %>% 
  group_by(i) %>% 
  # Keep up until first pos test
  filter(t <= min(c(t[obs_ct > 40], Inf))) %>% 
  ungroup()

tbl_data %>% 
  ggplot(aes(colour = i, fill = i)) +
  geom_line(aes(t, true_ct)) +
  geom_point(aes(t, obs_ct)) +
  theme(legend.position = "none")

tbl_data %>% 
  select(i, t, obs_ct) %>% 
  write_csv(here::here("data/simulated.csv"))
