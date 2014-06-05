setwd("/Users/mine/Desktop/Teaching/Sta 101 - F11/Labs/Lab 8/data")
nc = read.csv("ncbirths.csv", h = T, na.strings = "")

nc <- nc[,-9]

nc$marital <- nc$marital - 1

nc$whitemom <- rep(1,1000)
nc$whitemom[nc$racemom > 1] <- 0
nc$whitemom[nc$racemom == 0] <- NA

nc <- nc[,-8]

nc$marital[nc$marital == 1] <- "married"
nc$marital[nc$marital == 0] <- "not married"
nc$marital = as.factor(nc$marital)

nc$whitemom[nc$whitemom == 1] <- "white"
nc$whitemom[nc$whitemom == 0] <- "not white"
nc$whitemom = as.factor(nc$whitemom)

write.csv(nc, file = "nc.csv", row.names = FALSE)

nc = read.csv("nc.csv", h = T)

