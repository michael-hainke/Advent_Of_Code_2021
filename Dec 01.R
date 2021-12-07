# Advent of Code - Dec 1

library(tidyverse)

# Part 1

df <- read.csv("day1_data.txt", sep='\n', header = FALSE) %>%
      rename(depth = V1) %>%
      mutate(change = case_when((depth-lag(depth)) > 0 ~ "Increasing",
                                depth-lag(depth) < 0 ~ "Decreasing",
                                depth-lag(depth) == 0 ~ "Same",
                                TRUE ~ "NA")) %>%
      group_by(change) %>%
      tally()
      
# Part 2

df <- read.csv("day1_data.txt", sep='\n', header = FALSE) %>%
      rename(depth = V1) %>%
      mutate(sum = depth + lag(depth) + lag(depth, n=2)) %>%
      mutate(change = case_when((sum-lag(sum)) > 0 ~ "Increasing",
                                 sum-lag(sum) < 0 ~ "Decreasing",
                                 sum-lag(sum) == 0 ~ "Same",
                                 TRUE ~ "NA")) %>%
      group_by(change) %>%
      tally()
