# Advent of Code - Dec 6

library(tidyverse)

# read data

read_data <- function() {
   all_data <- readLines("day6_data.txt")
   df <- as.numeric(strsplit(all_data[1],",")[[1]])   
   return(df)
}

new_day <- function(df) {
   df = df - 1
   new_fish = length(df[df==-1])
   df[df==-1] = 6
   df = c(df, rep(8, times=new_fish))
   return(df)
   
}

new_day2 <- function(df) {
   df$age = df$age - 1
   new_fish = df$n[df$age==-1]
   df$age[df$age==-1] = 6
   if (length(new_fish)>0) {
     df = rbind(df,c(8,new_fish))
   }
   df = df %>%
      group_by(age) %>%
      summarise(n = sum(n))
   return(df)
   
}

# Part 1

df <- read_data()

number_of_days = 80

for (i in 1:number_of_days) {
   df = new_day(df)
}

print(length(df))

# Part 2 (256 days - can't model each fish, change to cardinalities)

age <- read_data()

df <- as.data.frame(age) %>%
      group_by(age) %>%
      summarise(n = n())

number_of_days = 256

for (i in 1:number_of_days) {
   df = new_day2(df)
}

options("scipen"=100, "digits"=4)
print(sum(df$n))
