# Advent of Code - Dec 9

library(tidyverse)

# read data

read_data <- function() {
   all_data <- readLines("day9_data.txt")
   df = data.frame(matrix(ncol=nchar(all_data[1]), nrow=0))
   
   for (i in 1:length(all_data)) {
      df = rbind(df, as.numeric(unlist(strsplit(all_data[i],""))))
   }

   return(df)
}

df <- read_data()

# Part 1

risk_levels = NULL
edge = max(df) + 1

for (x in 1:dim(df)[1]) {
   for (y in 1:dim(df)[2]) {
      if (df[x,y] < min(unlist(c(ifelse(x>1,df[x-1,y],edge),
                                 ifelse(x<dim(df)[1],df[x+1,y],edge),
                                 ifelse(y>1,df[x,y-1],edge),
                                 ifelse(y<dim(df)[2],df[x,y+1],edge))))) {
         risk_levels = c(risk_levels, df[x,y]+1)
      }   
   }
}

print(sum(risk_levels))

# Part 2

basin_id = 10
changes = 1

while (changes > 0) {
   changes = 0
   for (x in 1:dim(df)[1]) {
      for (y in 1:dim(df)[2]) {
         if (df[x,y] != 9) {
            adj = unlist(c(ifelse(x>1,df[x-1,y],9),
                           ifelse(x<dim(df)[1],df[x+1,y],9),
                           ifelse(y>1,df[x,y-1],9),
                           ifelse(y<dim(df)[2],df[x,y+1],9)))
            if (df[x,y] < 9 & max(adj) <= 9) { df[x,y] = basin_id
                                               basin_id = basin_id + 1
                                               changes = changes + 1
            } else if (df[x,y] != min(adj[adj>9])) { df[x,y] = min(adj[adj>9])
                                                   changes = changes + 1}
         }
      }
   }
   print(paste0("Number of Changes: ",changes))
}


summary = df %>%
          pivot_longer(cols = everything(), names_to = 'y', values_to = 'basin') %>%
          filter(basin != 9) %>%
          group_by(basin) %>%
          summarise(size=n()) %>%
          slice_max(size, n = 3)

print(summary$size[1]*summary$size[2]*summary$size[3])
