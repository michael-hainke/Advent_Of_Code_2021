# Advent of Code - Dec 10

# read data
df = readLines("day10_data.txt")

# Part 1

score1 = 0
incomplete = NULL
for (i in 1:length(df)) {
   a = NULL
   for (j in 1:nchar(df[i])) {
       if (substr(df[i],j,j) %in% c('[','(','{','<')) { a = paste0(a,substr(df[i],j,j))}
       else if (substr(df[i],j,j) == ']' & substr(a,nchar(a),nchar(a)) == '[') { a = substr(a,1,nchar(a)-1) }
       else if (substr(df[i],j,j) == ')' & substr(a,nchar(a),nchar(a)) == '(') { a = substr(a,1,nchar(a)-1) }
       else if (substr(df[i],j,j) == '}' & substr(a,nchar(a),nchar(a)) == '{') { a = substr(a,1,nchar(a)-1) }
       else if (substr(df[i],j,j) == '>' & substr(a,nchar(a),nchar(a)) == '<') { a = substr(a,1,nchar(a)-1) }
       else { if (substr(df[i],j,j) == ')') { score1 = score1 + 3 }
              if (substr(df[i],j,j) == ']') { score1 = score1 + 57 }
              if (substr(df[i],j,j) == '}') { score1 = score1 + 1197 }
              if (substr(df[i],j,j) == '>') { score1 = score1 + 25137 }
              break }
   if (j == nchar(df[i])) { incomplete = c(incomplete, a) }
   }
}

print(score1)

# Part 2

scores2 = NULL
for (i in 1:length(incomplete)) {
   score2 = 0
   for (j in nchar(incomplete[i]):1) {
      if (substr(incomplete[i],j,j) == '(') { score2 = score2*5+1 }
      if (substr(incomplete[i],j,j) == '[') { score2 = score2*5+2 }
      if (substr(incomplete[i],j,j) == '{') { score2 = score2*5+3 }
      if (substr(incomplete[i],j,j) == '<') { score2 = score2*5+4 }
   } 
   scores2 = c(scores2, score2)
}

print(median(scores2))
