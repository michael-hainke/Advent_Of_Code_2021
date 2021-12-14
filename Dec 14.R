# Advent of Code - Dec 14

read_data <- function(filename) {
   all_data <- readLines(filename)
   df = data.frame(matrix(ncol=3, nrow=0))
   
   for (i in 1:length(all_data)) {
      rw = unlist(strsplit(all_data[i]," -> "))
      df = rbind(df, unlist(c(strsplit(rw[1],""), rw[2])))
   }
   colnames(df) = c('pre','post','insert')
   return(df)
}

df = read_data("day14_data.txt")

# Part 1 (Brute Force)

pattern = unlist(strsplit('CFFPOHBCVVNPHCNBKVNV',''))
rounds = 10

for (round in 1:rounds) {
   x = 1
   while (x < length(pattern)) {
    insert = df[df$pre==pattern[x]&df$post==pattern[x+1],"insert"]
    pattern = c(pattern[1:x], insert, pattern[(x+1):length(pattern)])
    x = x + 2
   }
print(round)
}

max(table(pattern)) - min(table(pattern))

# Part 2 (Brute Force Bad.  There is a better way)

pattern = unlist(strsplit('CFFPOHBCVVNPHCNBKVNV',''))
rounds = 40

pat = data.frame(pre = pattern, post = lead(pattern)) %>%
      group_by(pre,post) %>%
      summarise(n = n())

for (round in 1:rounds) {
   print(paste0("Round: ",round))
   pat = pat %>%
         left_join(df,by=c('pre','post')) %>%
         group_by(pre,post,insert) %>%
         summarise(n = sum(n))
   
   pat = rbind(select(pat, pre, post=insert, n), select(pat, pre=insert, post,n)) %>%
         filter(!is.na(pre)) %>%
         ungroup()
   
}

summary = pat %>%
          group_by(pre) %>%
          summarise(n = sum(n))

options("scipen"=100, "digits"=4)
max(summary$n) - min(summary$n)

