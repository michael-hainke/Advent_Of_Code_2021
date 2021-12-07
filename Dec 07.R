# Advent of Code - Dec 7

library(tidyverse)

# read data

read_data <- function() {
   all_data <- readLines("day7_data.txt")
   df <- as.numeric(strsplit(all_data[1],",")[[1]])   
   return(df)
}

calc_fuel <- function(df,pos,df_fuel) {
   df = data.frame(df, abs(df-pos))
   colnames(df) = c('pos','dist')
   df = left_join(df,df_fuel,by=c('dist'))
   return(sum(df$fuel))
}

df <- read_data()

# Part 1

df_fuel = data.frame(0,0)
colnames(df_fuel) = c('dist','fuel')

for (i in 1:(max(df)-min(df))) {
   df_fuel = rbind(df_fuel, c(i, i))
}

df_final = data.frame(min(df):max(df), 0)
colnames(df_final) <- c('pos','fuel')

for (i in 1:length(df_final$pos)) {
   df_final$fuel[i] = calc_fuel(df,df_final$pos[i],df_fuel)
}

print(min(df_final$fuel))

# Part 2

#df = c(16,1,2,0,4,2,7,1,2,14)

df_fuel = data.frame(0,0)
colnames(df_fuel) = c('dist','fuel')

for (i in 1:(max(df)-min(df))) {
   df_fuel = rbind(df_fuel, c(i, i+df_fuel$fuel[i]))
}

df_final = data.frame(min(df):max(df), 0)
colnames(df_final) <- c('pos','fuel')

for (i in 1:length(df_final$pos)) {
   df_final$fuel[i] = calc_fuel(df,df_final$pos[i],df_fuel)
}

print(min(df_final$fuel))
