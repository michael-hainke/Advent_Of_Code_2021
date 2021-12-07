# Advent of Code - Dec 4

library(tidyverse)

# read data

read_data <- function() {
   all_data <- readLines("day4_data.txt")
   numbers <- as.numeric(strsplit(all_data[1],",")[[1]])   
   
   board = 1
   row = 1
   boards = data.frame(matrix(ncol=6, nrow=0))
   
   for (i in 3:length(all_data)) {
    rw = as.numeric(str_split(all_data[i]," ")[[1]])
    if (length(rw)==1) {
       board = board + 1
       row = 1
    } else {
       rw = rw[!is.na(rw)]
       rw = c(board, row, rw)
       boards = rbind(boards, rw)
       row = row + 1
    }
   }
   
   colnames(boards) <- c('board', 'row', 'b', 'i', 'n', 'g','o')

   return(boards)
}

# Part 1

boards <- read_data()

# loop through bingo numbers and determine winner
winner = FALSE
i = 1

while (!winner) {
   num = numbers[i]
   boards = boards %>%
            mutate(b = ifelse(b==num,-1,b),
                   i = ifelse(i==num,-1,i),
                   n = ifelse(n==num,-1,n),
                   g = ifelse(g==num,-1,g),
                   o = ifelse(o==num,-1,o))
   
   board_sum = boards %>%
               mutate(tot = b+i+n+g+o) %>%
               group_by(board) %>%
               summarise(min_val = min(sum(b),sum(i),sum(n),sum(g),sum(o),min(tot))) %>%
               filter(min_val == -5)
   
   if ( length(board_sum$board) > 0 ) {
      wb = boards %>%
           filter(board == board_sum$board[1])
      total = c(wb$b, wb$i, wb$n, wb$g, wb$o)
      total = (sum(total[total!=-1]) * num)
      print(total)
      winner = TRUE
   } else {
      i = i + 1
   }
   
}


# Part 2

boards <- read_data()

# loop through bingo numbers and determine last board to win
winner = FALSE
i = 1

while (!winner) {
   num = numbers[i]
   boards = boards %>%
      mutate(b = ifelse(b==num,-1,b),
             i = ifelse(i==num,-1,i),
             n = ifelse(n==num,-1,n),
             g = ifelse(g==num,-1,g),
             o = ifelse(o==num,-1,o))
   
   board_sum = boards %>%
      mutate(tot = b+i+n+g+o) %>%
      group_by(board) %>%
      summarise(min_val = min(sum(b),sum(i),sum(n),sum(g),sum(o),min(tot))) %>%
      filter(min_val == -5)
   
   winner = (length(boards$board[!boards$board %in% board_sum$board])==0)

   if (winner) {
      wb = boards %>%
         filter(board %in% board_sum$board)
      total = c(wb$b, wb$i, wb$n, wb$g, wb$o)
      total = (sum(total[total!=-1]) * num)
      print(total)
   } else {
      boards = filter(boards, !board %in% board_sum$board)
      i = i + 1
   }
   
}



