# Advent of Code - Dec 11

library(tidyverse)

read_data <- function() {
   all_data <- readLines("day11_data.txt")
   a = matrix(ncol=nchar(all_data[1]), nrow=length(all_data))
   
   for (i in 1:length(all_data)) {
      a[i,] <- as.numeric(str_split(all_data[i],"")[[1]])
   }

   return(a)
}

dumbo = read_data()

# Part 1 and Part 2

rounds = 1000
flashes = 0
all_flash_round = 0

for (i in 1:rounds) {
   dumbo = dumbo + 1
   repeat {
      tmp_flash = 0
      for (x in 1:dim(dumbo)[1]) {
         for (y in 1:dim(dumbo)[2]) {
            if (dumbo[x,y]>9) {
               if(x==dim(dumbo)[1]) {x2=x} else {x2=x+1}
               if(y==dim(dumbo)[2]) {y2=y} else {y2=y+1}
               if(x==1) {x1=x} else {x1=x-1}
               if(y==1) {y1=y} else {y1=y-1}
               dumbo[x1:x2,y1:y2] = dumbo[x1:x2,y1:y2] + 1
               dumbo[x,y] = -10000
               tmp_flash = tmp_flash + 1
            }
         }
      }
      if (tmp_flash > 0) { flashes = flashes + tmp_flash } else { break }
   }
   if (length(dumbo[dumbo<0])==100 & all_flash_round == 0) { all_flash_round = i }
   dumbo[dumbo<0] = 0
}

print(flashes)
print(all_flash_round)

