# Advent of Code - Dec 5

library(tidyverse)

# read data

read_data <- function() {
   all_data <- readLines("day5_data.txt")

   df = data.frame(matrix(ncol=4, nrow=0))
   
   for (i in 1:length(all_data)) {
      st_end <- strsplit(all_data[i]," -> ")
      st <- strsplit(st_end[[1]][1],",")
      end <- strsplit(st_end[[1]][2],",")
      rw <- c(as.numeric(st[[1]]), as.numeric(end[[1]]))
      
      df = rbind(df, rw)
      
   }
   
   colnames(df) <- c('x1','y1','x2','y2')

   return(df)
}

danger_points <- function(df) {
   
   pts = data.frame(matrix(ncol=2, nrow=0))
   
   for (i in 1:dim(df)[1]) {
      rw = df[i,]
      new_pts = data.frame(rw$x1:rw$x2, rw$y1:rw$y2)
      
      pts = rbind(pts, new_pts)
      
   }
   
   colnames(pts) <- c('x','y')
   
   danger = pts %>%
      group_by(x,y) %>%
      summarise(n = n()) %>%
      filter(n > 1)
   
   print(dim(danger)[1])
   
}

# Part 1

df <- read_data() %>%
      filter(x1==x2 | y1==y2)

danger_points(df)

# Part 2 (identical to part 1 except don't filter only horz and vert lines)

df <- read_data()

danger_points(df)
