# Advent of Code - Dec 12

library(tidyverse)

read_data <- function() {
   all_data <- readLines("day12_data.txt")
   df = data.frame(matrix(ncol=2, nrow=0))
   
   for (i in 1:length(all_data)) {
      df = rbind(df, unlist(strsplit(all_data[i],"-")))
   }
   colnames(df) = c('from','to')
   
   # add the reverse paths
   reverse = df
   colnames(reverse) = c('to','from')
   df = rbind(df, reverse) %>% filter(to != 'start')
   
   return(df)
}

get_paths <- function(paths, lower, n) {
   END = FALSE
   num_paths = 0
   col_num = 1
   
   while (!END) {
      paths = left_join(paths, df, by=c("to"="from")) %>%
         rename(!!paste0('from',col_num):=to,
                to = to.y) 
      col_num = col_num + 1
      
      num_paths = num_paths + dim(paths %>% filter(to == 'end'))[1]
      paths = paths %>% filter(to != 'end')
      
      for (j in 1:dim(paths)[1]) {
         dups = as.data.frame(table(unlist(paths[j,]))) %>% filter(Var1 %in% lower)
         paths$dups[j] = case_when((length(dups$Freq[dups$Freq==2])>n | length(dups$Freq[dups$Freq==3])>0) ~ 1,
                                   TRUE ~ 0)
      }

      print(paste0("round: ",col_num," paths: ",num_paths))
      paths = paths %>% filter(dups == 0)

      if (dim(paths)[1] == 0) { END = TRUE }
   }
   print(num_paths)
}


df = read_data()

# get list of small caves
lower = unique(c(unique(df$from),unique(df$to)))
lower = lower[!lower %in% c('start','end') & lower %in% str_to_lower(lower)]

# get start paths
paths = df %>% filter(from == 'start') %>% mutate(dups = 0)

# Part 1 (number of times a small cave can be visited twice = 0)

get_paths(paths, lower, 0)

# Part 2 (number of times a small cave can be visited twice = 1)

get_paths(paths, lower, 1)
