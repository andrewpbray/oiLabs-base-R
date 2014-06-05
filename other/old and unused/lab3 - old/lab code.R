setwd("/Users/mine/Dropbox/OI Labs/lab3")
load("star.RData")

###
# ELA scores
###

ela5 = star$score[star$testID == "ela" & star$grade == "5" & !is.na(star$score)]

hist(ela5)

qqnorm(ela5)
qqline(ela5)

ela5mean <- mean(ela5)
ela5sd <- sd(ela5)

hist(ela5, probability = TRUE)
lines(x = 0:3000, y = dnorm(x = 0:3000, mean = ela5mean, sd = ela5sd), col = "blue")


# check the 68-95-99.7 rule - fits pretty well
ela5mean - ela5sd
ela5mean + ela5sd
within1sd <- ela5 > (ela5mean - ela5sd) & ela5 < (ela5mean + ela5sd)

ela5n <- length(ela5)

sum(ela5 > (ela5mean - ela5sd) & ela5 < (ela5mean + ela5sd)) / ela5n
sum(ela5 > (ela5mean - 2*ela5sd) & ela5 < (ela5mean + 2*ela5sd)) / ela5n
sum(ela5 > (ela5mean - 3*ela5sd) & ela5 < (ela5mean + 3*ela5sd)) / ela5n


# calculate exact and estimated probabilities, see if they match

sum(ela5 < 1500) / (ela5n+1)
pnorm(q = 1500, mean = ela5mean, sd = ela5sd)





###
# MATH scores
###

math = star$score[star$testID == "math" & !is.na(star$score)]

summary(math)

hist(math)

qqnorm(math)
qqline(math)

mathMean = mean(math)
mathSD = sd(math)

hist(math, probability = TRUE)
lines(x = 0:3300, y = dnorm(x = 0:3300, mean = mathMean, sd = mathSD))


# check the 68-95-99.7 rule - fits pretty well
x = math
m = mathMean
s = mathSD
n = length(math)

sum(math > (mathMean - mathSD) & math < (mathMean + mathSD)) / elaN
sum(math > (mathMean - 2*mathSD) & math < (mathMean + 2*mathSD)) / elaN
sum(math > (mathMean - 3*mathSD) & math < (mathMean + 3*mathSD)) / elaN







# guess shapes based on normal probability plots

# right skewed: geometry

qqnorm(star$score[star$testID == "geometry"])
qqnorm(star$score[star$testID == "geometry"])
hist(star$score[star$testID == "geometry"])



# left skewed: sci 4

qqnorm(star$score[star$testID == "sci 4"])
qqnorm(star$score[star$testID == "sci 4"])
hist(star$score[star$testID == "sci 4"])
