dat <- read.delim("C:/Users/Andrew/Documents/My Dropbox/OI APB/lab3/ca2010_1_csv_v3.txt", sep=",", header=T)
ela5 <- dat[dat$Test.Type=="C" & dat$Grade==5 & dat$Test.Id==7, 17] # ELA
math5 <- dat[dat$Test.Type=="C" & dat$Grade==5 & dat$Test.Id==8, 17] # Math
ela5 <- as.numeric(ela5)
math5 <- as.numeric(math5)
ela5 <- ela5[ela5 > 5]
math5 <- math5[math5 > 5]


par(mfrow=c(3,3))
qqnorm(d$Hgt)
qqline(d$Hgt)
for(i in 1:8){
  dat <- rnorm(length(d$Hgt), mean(d$Hgt), sd(d$Hgt))
  qqnorm(dat)
  qqline(dat)
}


par(mfrow=c(3,3))
qqnorm(d$Wgt)
qqline(d$Wgt)
for(i in 1:8){
  dat <- rnorm(length(d$Wgt), mean(d$Wgt), sd(d$Wgt))
  qqnorm(dat)
  qqline(dat)
}