# Advent of Code - Dec 13

library(plot.matrix)

read_data <- function(filename) {
   all_data <- readLines(filename)
   mat = data.frame(matrix(ncol=2, nrow=0))
   
   for (i in 1:length(all_data)) {
      mat = rbind(mat, as.numeric(unlist(strsplit(all_data[i],","))))
   }
   colnames(mat) = c('x','y')
   
   df = matrix(0, ncol=max(mat$x)+1, nrow=max(mat$y)+1)
   for (i in 1:dim(mat)[1]) {
      df[mat$y[i]+1,mat$x[i]+1] = 1
   }
   
   return(df)
}

read_instructions <- function(filename) {
   all_data <- readLines(filename)
   df = data.frame(matrix(ncol=2, nrow=0))
   
   for (i in 1:length(all_data)) {
      ln = gsub('fold along ','',all_data[i])
      df = rbind(df, unlist(strsplit(ln,"=")))
   }
   colnames(df) = c('axis','dist')
   df$dist = as.numeric(df$dist)
   return(df)
}

fold_paper <- function(paper,axis,dist) {
   if (axis == "x")      { a = paper[,1:dist] + paper[,dim(paper)[2]:(dist+2)] }
   else if (axis == "y") { a = paper[1:dist,] + paper[dim(paper)[1]:(dist+2),] }
   else                  { a = 0 }
   a[a==2] = 1
   return(a)
}


pap = read_data("day13_data.txt")
cmds = read_instructions("day13_data_instructions.txt")

# Part 1

sum(fold_paper(pap, cmds$axis[1], cmds$dist[1]))

# Part 2

code = pap
for (i in 1:dim(cmds)[1]) {
   code = fold_paper(code, cmds$axis[i], cmds$dist[i])
}

plot(code==0, border=NA, col=c('red','light green'), key=NULL)

