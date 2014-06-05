# Data Cleaning
setwd("~/My Dropbox/OI APB/OI Labs/lab8/LAhomes")
homes <- read.delim("~/My Dropbox/OI APB/OI Labs/lab8/LAhomes/LAhomes.txt")
homes <- homes[,-8]
levels(homes$garage)[1] <- 0
levels(homes$type)[1] <- NA
levels(homes$pool)[1] <- "N"
write.csv(homes, "homes.txt", row.names = F)

