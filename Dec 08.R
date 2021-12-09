# Advent of Code - Dec 8

library(tidyverse)

# read data

read_data <- function() {
   all_data <- readLines("day8_data.txt")
   df = data.frame(matrix(ncol=4, nrow=0))
   colnames(df) <- c('entry','signal','id','pattern')
   
   for (i in 1:length(all_data)) {
      a <- unlist(strsplit(all_data[i]," \\| "))
      input <- data.frame(rep(i,times=10),rep('input',times=10),1:10,unlist(strsplit(a[1]," ")))
      colnames(input) <- c('entry','signal','id','pattern')
      output <- data.frame(rep(i,times=4),rep('output',times=4),1:4,unlist(strsplit(a[2]," ")))
      colnames(output) <- c('entry','signal','id','pattern')
      df = rbind(df, input, output)
   }
   return(df)
}

df <- read_data()

# Part 1

df_output <- df %>% filter(signal == 'output', str_length(df$pattern) %in% c(2,3,4,7))
print(length(df_output$pattern))

# Part 2

output = NULL

for (i in unique(df$entry)) {

   input_df = df %>% filter(entry == i & signal == 'input')
   
   one = input_df[nchar(input_df$pattern)==2,'pattern']
   seven = input_df[nchar(input_df$pattern)==3,'pattern']
   four = input_df[nchar(input_df$pattern)==4,'pattern']
   eight = input_df[nchar(input_df$pattern)==7,'pattern']
   three = input_df[nchar(input_df$pattern)==5 &
                   str_detect(input_df$pattern,substr(seven,1,1)) &
                   str_detect(input_df$pattern,substr(seven,2,2)) &
                   str_detect(input_df$pattern,substr(seven,3,3)),'pattern']
   nine = input_df[nchar(input_df$pattern)==6 &
               str_detect(input_df$pattern,substr(three,1,1)) &
               str_detect(input_df$pattern,substr(three,2,2)) &
               str_detect(input_df$pattern,substr(three,3,3)) &
               str_detect(input_df$pattern,substr(three,4,4)) &
               str_detect(input_df$pattern,substr(three,5,5)),'pattern']
   zero = input_df[nchar(input_df$pattern)==6 &
                  !str_detect(input_df$pattern,nine) &   
                  str_detect(input_df$pattern,substr(one,1,1)) &
                  str_detect(input_df$pattern,substr(one,2,2)),'pattern']
   six = input_df[nchar(input_df$pattern)==6 &
                 !str_detect(input_df$pattern,nine) &
                 !str_detect(input_df$pattern,zero),'pattern']
   bot_right = unlist(str_split(one,""))[unlist(str_split(one,"")) %in% unlist(str_split(six,""))]
   five = input_df[nchar(input_df$pattern)==5 &
                  str_detect(input_df$pattern,bot_right) &
                  !str_detect(input_df$pattern,three),'pattern']
   two = input_df[nchar(input_df$pattern)==5 &
                 !str_detect(input_df$pattern,three) &
                 !str_detect(input_df$pattern,five),'pattern']

   input_df =  input_df %>%
               mutate(value = case_when(pattern == zero ~ 0,
                                        pattern == one ~ 1,
                                        pattern == two ~ 2,
                                        pattern == three ~ 3,
                                        pattern == four ~ 4,
                                        pattern == five ~ 5,
                                        pattern == six ~ 6,
                                        pattern == seven ~ 7,
                                        pattern == eight ~ 8,
                                        pattern == nine ~ 9,
                                        TRUE ~ -1))
   
   for (j in 1:length(input_df$pattern)) {
      input_df$pattern[j] = str_flatten(str_sort(unlist(str_split(input_df$pattern[j],""))))
   }
   
   output_df = df %>% filter(entry == i & signal == 'output')
     
   for (j in 1:length(output_df$pattern)) {
      output_df$pattern[j] = str_flatten(str_sort(unlist(str_split(output_df$pattern[j],""))))
   }
   
   output_df = output_df %>%
               left_join(input_df, by = c('pattern')) %>%
               select(value)
   
   output_val = as.numeric(paste0(output_df$value[1],output_df$value[2],
                                  output_df$value[3],output_df$value[4]))
   
   output = c(output,output_val)

}

print(sum(output))