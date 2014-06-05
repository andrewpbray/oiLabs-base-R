# http://www.amstat.org/publications/JSE/v11n2/datasets.johnson.html

## On Your Own plots
pdf("histQQmatch.pdf", width = 8.5, height = 11)
par(mfrow = c(4,2))

sdat <- (fdims$bii.di - mean(fdims$bii.di))/sd(fdims$bii.di)
hist(sdat, nclass = 15, main = "Histogram of female bii.di")

sdat <- (fdims$che.de - mean(fdims$che.de))/sd(fdims$che.de)
qqnorm(sdat, main = "Normal Q-Q Plot A")
qqline(sdat)

##
sdat <- (fdims$elb.di - mean(fdims$elb.di))/sd(fdims$elb.di)
hist(sdat, main = "Histogram of female elb.di")

sdat <- (fdims$bii.di - mean(fdims$bii.di))/sd(fdims$bii.di)
qqnorm(sdat, main = "Normal Q-Q Plot B")
qqline(sdat)

##
sdat <- (bdims$age - mean(bdims$age))/sd(bdims$age)
hist(sdat, main = "Histogram of general age")

sdat <- (fdims$elb.di - mean(fdims$elb.di))/sd(fdims$elb.di)
qqnorm(sdat, main = "Normal Q-Q Plot C")
qqline(sdat)

##
sdat <- (fdims$che.de - mean(fdims$che.de))/sd(fdims$che.de)
hist(sdat, main = "Histogram of female che.de")

sdat <- (bdims$age - mean(bdims$age))/sd(bdims$age)
qqnorm(sdat, main = "Normal Q-Q Plot D")
qqline(sdat)

dev.off()





# BiaD for males - left skewed

hist(d$BiaDi[d$Sex == 1])
qqnorm(d$BiaDi[d$Sex == 1])
qqline(d$BiaDi[d$Sex == 1])

# DiiDi for females - left skewed - more skewed than BiaD for males

hist(d$DiiDi[d$Sex == 0])
qqnorm(d$DiiDi[d$Sex == 0])
qqline(d$DiiDi[d$Sex == 0])

# CheDe overall - right skewed

hist(d$CheDe)

# CheDe for females - very right skewed - good example for rs qq plot

hist(d$CheDe[d$Sex == 0])
qqnorm(d$CheDe[d$Sex == 0])
qqline(d$CheDe[d$Sex == 0])

# CheDi for females - pretty right skewed - good example for rs qq plot

hist(d$CheDi[d$Sex == 0])
qqnorm(d$CheDi[d$Sex == 0])
qqline(d$CheDi[d$Sex == 0])

# ElbDi for females - only somewhat left skewed but good example for steps & rounding

hist(d$ElbDi[d$Sex == 0])
qqnorm(d$ElbDi[d$Sex == 0])
qqline(d$ElbDi[d$Sex == 0])

# KneDi for females - pretty right skewed

hist(d$KneDi[d$Sex == 0])
qqnorm(d$KneDi[d$Sex == 0])
qqline(d$KneDi[d$Sex == 0])

# AnkDi for females - left skewed

hist(d$AnkDi[d$Sex == 0])
qqnorm(d$AnkDi[d$Sex == 0])
qqline(d$AnkDi[d$Sex == 0])

# ShoGi for females - right skewed

hist(d$ShoGi[d$Sex == 0])
qqnorm(d$ShoGi[d$Sex == 0])
qqline(d$ShoGi[d$Sex == 0])

# WaiGi overall, males and females right skewed, but females most right skewed

hist(d$WaiGi[d$Sex == 0])
qqnorm(d$WaiGi[d$Sex == 0])
qqline(d$WaiGi[d$Sex == 0])

# ThiGi for females - right skewed

hist(d$ThiGi[d$Sex == 0])
qqnorm(d$ThiGi[d$Sex == 0])
qqline(d$ThiGi[d$Sex == 0])

# BicGi for females - right skewed

hist(d$BicGi[d$Sex == 0])
qqnorm(d$BicGi[d$Sex == 0])
qqline(d$BicGi[d$Sex == 0])

# Age ovarall - right skewed

hist(d$Age)
qqnorm(d$Age)
qqline(d$Age)



# data processing

bdims <- read.csv("~/My Dropbox/OI APB/OI Labs/lab3/data/body.csv")
names(bdims) <- c("bia.di", "bii.di", "bit.di", "che.de", "che.di", "elb.di", "wri.di", "kne.di", "ank.di", "sho.gi", "che.gi", "wai.gi", "nav.gi", "hip.gi", "thi.gi", "bic.gi", "for.gi", "kne.gi", "cal.gi", "ank.gi", "wri.gi", "age", "wgt", "hgt", "sex")
write.csv(bdims, "bdims", row.names=F)

