# Advent of Code - Dec 3

library(tidyverse)

# Part 1

df <- read.csv("day3_data.txt", sep=' ', header = FALSE, colClasses=c("character"))

len = nchar(df$V1[1])

gamma = ""
epsilon = ""

for (i in 1:len) {
   cnt = table(str_sub(df$V1, i, i))
   if (cnt[1][1] > length(df$V1)/2) {
      gamma = paste0(gamma,'0')
      epsilon = paste0(epsilon,'1')
   } else {
      gamma = paste0(gamma,'1')
      epsilon = paste0(epsilon,'0')
   }
}

print(strtoi(gamma, base = 2) * strtoi(epsilon, base = 2))

# Part 2

calc_rating <- function(values, rating) {
   i = 1
   len = nchar(values[1])
   while (length(values) > 1) {
      cnt = table(str_sub(values, i, i))
      if (rating == 'oxygen') {
         if (length(cnt) == 2 & cnt["1"] >= length(values)/2) {
            values =  values[str_sub(values, i, i)==1]
         } else if (length(cnt) == 2 & cnt["1"] < length(values)/2) {
            values = values[str_sub(values, i, i)==0]
         }         
      } else if (rating == 'co2') {
         if (length(cnt) == 2 & cnt["1"] >= length(values)/2) {
            values =  values[str_sub(values, i, i)==0]
         } else if (length(cnt) == 2 & cnt["1"] < length(values)/2) {
            values = values[str_sub(values, i, i)==1]
         }        
      } 
      i = i + 1
   }
   return(values)
}

df <- read.csv("day3_data.txt", sep=' ', header = FALSE, colClasses=c("character"))

print(strtoi(calc_rating(df$V1,'oxygen'), base = 2)*strtoi(calc_rating(df$V1,'co2'), base = 2))

