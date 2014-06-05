setwd("~/Dropbox/OI APB/OI Labs/lab8/data")
homes <- read.delim("~/Dropbox/OI APB/OI Labs/lab8/data/LAhomes.txt")
homes <- homes[,-8]
levels(homes$garage)[1] <- "0"
levels(homes$type)[1] <- NA

plot(homes)
hist(homes$price)



set.seed(552012)
A <- sample(letters, 100, replace = T)
a <- c(40, 18, 42, 99, 16, 45, 35, 52)
b <- c(50, 84, 18, 42)
d <- c(7, 92, 23, 18)
e <- c(45, 62, 17)
f <- c(2, 13, 47, 7, 70, 35)
g <- c(62, 99, 52, 18, 22, 45, 72, 42)
cat(A[a],".",A[b],".",A[d],".",A[e],"/",A[f],"/",A[g],sep = "")