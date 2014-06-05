counts <- as.character(iran2$Karrubi)
ss <- rep(0,366)
for(i in 1:366){
ss[i] <- as.integer(substr(counts[i],1,1))}
table(ss)
barplot(table(ss))

nor <- read.csv("~/My Dropbox/lab6/nor.csv")
komm <- levels(nor$KOMMNAVN)
party <- levels(nor$PARTIKODE)
nor$SUM <- as.numeric(nor$SUM)
n <- length(komm)
p <- length(party)
mat <- matrix(rep(0, n*p), n)
nor2 <- data.frame(mat)
nor2 <- data.frame(komm, nor2)
names(nor2) <- c("kommune", party)
vnames <- as.factor(party)

for(i in 1:dim(nor)[1]){
  nor2[which(nor2$komm == nor$KOMMNAVN[i]), which(c(FALSE, vnames == nor$PARTIKODE[i]))] <- nor$SUM[i]
}

ptot <- colSums(nor2[,2:211])
head(order(ptot, decreasing=T))