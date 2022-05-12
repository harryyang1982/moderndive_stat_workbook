library(tidyverse)
library(moderndive)

# 7.1 Sampling bowl activity
# 7.1.2 Using the shovel once
data(tactile_prop_red)
tactile_prop_red

ggplot(tactile_prop_red, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red",
       title = "Distribution of 33 proportions red")
# 7.1.4 What did we do?

# 7.2 Virtual sampling
# 7.2.1 Using the virtual shovel once

bowl
virtual_shovel <- bowl %>% 
  rep_sample_n(size = 50)
virtual_shovel

virtual_shovel %>% 
  mutate(is_red = (color == "red")) %>% 
  summarize(num_red = sum(is_red))

virtual_shovel %>% 
  mutate(is_red = (color == "red")) %>% 
  summarize(num_red = mean(is_red))

virtual_shovel %>% 
  summarize(num_red = mean(color == "red"))

n_sim <- 33
s_dist <- vector(length=n_sim)
for (i in 1:n_sim) {
  s_dist[i] <- bowl %>% rep_sample_n(size=50) %>% 
    summarize(num_red = mean(color == "red")) %>% .[['num_red']]
}

mean(s_dist)

# 7.2.2 Using the virtual shovel 33 times
virtual_samples <- bowl %>% 
  rep_sample_n(size = 50, reps = 33)
virtual_samples

s_dist <- bowl %>% 
    rep_sample_n(size = 50, reps = 500000) %>% 
    group_by(factor(replicate)) %>% 
    summarize(num_red = mean(color == "red")) %>% .[['num_red']]
mean(s_dist)

virtual_prop_red <- virtual_samples %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == 'red'),
            prop_red = mean(color == 'red'))

virtual_prop_red

ggplot(virtual_prop_red, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red",
       title = "Distribution of 33 proportions red")

# 7.2.3 Using the virtual shovel 1000 times
virtual_samples <- bowl %>% 
  rep_sample_n(size = 50, reps = 1000)
virtual_samples
virtual_prop_red <- virtual_samples %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == "red"),
            prop_red = mean(color == "red"))
virtual_prop_red
mean(virtual_prop_red$prop_red)

ggplot(virtual_prop_red, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red",
       title = "Distribution of 1000 proportions red")

# 7.2.4 Using different shovels

# Segment 1: sample size = 25
# 1. a) Virtually use shovel 1000 times
virtual_samples_25 <- bowl %>% 
  rep_sample_n(size = 25, reps = 1000)

# 1. b) Compute resulting 1000 replicates of proportion red
virtual_prop_red_25 <- virtual_samples_25 %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == "red"),
            prop_red = mean(color == "red"))

# 1. c) Plot distribution via a histogram
g1 <- ggplot(virtual_prop_red_25, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 25 balls that were red", title = "25")

# Segment 2: sample size = 50
# 2. a) Virtually use shovel 1000 times
virtual_samples_50 <- bowl %>% 
  rep_sample_n(size = 50, reps = 1000)

# 2. b) Compute resulting 1000 replicates of proportion red
virtual_prop_red_50 <- virtual_samples_50 %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == "red"),
            prop_red = mean(color == "red"))

# 2. c) Plot distribution via a histogram
g2 <- ggplot(virtual_prop_red_50, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red", title = "50")

# Segment 3: sample size = 100
# 3. a) Virtually use shovel 1000 times
virtual_samples_100 <- bowl %>% 
  rep_sample_n(size = 100, reps = 1000)

# 3. b) Compute resulting 1000 replicates of proportion red
virtual_prop_red_100 <- virtual_samples_100 %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == "red"),
            prop_red = mean(color == "red"))

# 3. c) Plot distribution via a histogram
g3 <- ggplot(virtual_prop_red_100, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 100 balls that were red", title = "100")

library(gridExtra)

grid.arrange(g1, g2, g3, ncol = 3)

# n = 25
virtual_prop_red_25 %>% 
  summarize(sd = sd(prop_red))

# n = 50
virtual_prop_red_50 %>% 
  summarize(sd = sd(prop_red))

# n = 100
virtual_prop_red_100 %>% 
  summarize(sd = sd(prop_red))
