# Advent of Code - Dec 2

library(tidyverse)

# Part 1

df <- read.csv("day2_data.txt", sep=' ', header = FALSE) %>%
      rename(direction = V1, distance = V2)

x = 0
y = 0

for (i in 1:length(df$direction)) {
   x = case_when(df$direction[i] == 'down' ~ x + df$distance[i],
                 df$direction[i] == 'up' ~ x - df$distance[i],
                 TRUE ~ x)
   y = case_when(df$direction[i] == 'forward' ~ y + df$distance[i],
                 TRUE ~ y)
}

print(x*y)

# Part 2

df <- read.csv("day2_data.txt", sep=' ', header = FALSE) %>%
      rename(direction = V1, distance = V2)

x = 0
y = 0
aim = 0

for (i in 1:length(df$direction)) {
   aim = case_when(df$direction[i] == 'down' ~ aim + df$distance[i],
                   df$direction[i] == 'up' ~ aim - df$distance[i],
                   TRUE ~ aim)
   x = case_when(df$direction[i] == 'forward' ~ x + aim * df$distance[i],
                 TRUE ~ x)
   y = case_when(df$direction[i] == 'forward' ~ y + df$distance[i],
                 TRUE ~ y)

}

print(x*y)
