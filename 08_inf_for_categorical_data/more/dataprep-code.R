#------------ Table 6 -------------#
# Read and reformat table 6 from the Global Index
# of Religiosity and Atheism
d <- read.csv("data/table6.csv", header = F)
names(d) <- c("country", "sample_size", "religious", "non_religious", "atheist", "no_answer")

d$religious <- as.integer(substr(as.character(d$religious),1,2))
d$non_religious <- as.integer(substr(as.character(d$non_religious),1,2))
d$atheist <- as.integer(substr(as.character(d$atheist),1,2))
d$no_answer <- as.integer(substr(as.character(d$no_answer),1,2))

# Provisional naive approximation of original counts
d$religious <- round(d$sample_size*d$religious/100)
d$non_religious <- round(d$sample_size*d$non_religious/100)
d$atheist <- round(d$sample_size*d$atheist/100)
d$no_answer <- round(d$sample_size*d$no_answer/100)
d$sample_size <- rowSums(d[,3:6])

# Add back in 8 missing people by randomly choosing rows and seeing
# if an addition can be made without skewing rounded percentages
d[10,5] <- d[10,5] - 1
d[7,3] <- d[7,3] - 1
d[45,4] <- d[45,4] - 1
d[37,6] <- d[37,6] - 1
d[6,6] <- d[6,6] - 1
d[4,3] <- d[4,3] - 1
d[17,4] <- d[17,4] - 1
d[1,3] <- d[1,3] - 1
d$sample_size <- rowSums(d[,3:6])

# Expand out into new data table
col1 <- NULL
col2 <- NULL

for(i in 1:(dim(d)[1])){
  n <- d[i,2]
  n1 <- d[i,3]
  n2 <- d[i,4]
  n3 <- d[i,5]
  n4 <- d[i,6]
  nletters <- nchar(as.character(d[i,1]))
  nation <- substr(as.character(d[i,1]), 1, nletters-1)
  col1 <- c(col1, rep(nation, n))
  newCol2 <- rep(c("religious", "non-religious", "atheist", "no answer"), c(n1, n2, n3, n4))
  col2 <- c(col2, sample(newCol2)) # shuffle the order to replicate look of original data
}

col2[col2 == "religious"] = "non-atheist"
col2[col2 == "non-religious"] = "non-atheist"
col2[col2 == "no answer"] = "non-atheist"

atheism12 <- data.frame("nationality" = as.factor(col1), "response" = as.factor(col2), "year" = 2012)
# write.csv(atheism12, "data/atheism12.csv", row.names = F)


#------ Table 4 - Mine ------#
d2 <- read.csv("data/table4.csv", header = F)
names(d2) <- c("country", "atheist2005", "atheist2012", "atheistDiff")

d2$atheist2005 <- as.integer(gsub("%", "", substr(as.character(d2$atheist2005),1,2)))
d2$atheist2012 <- as.integer(gsub("%", "", substr(as.character(d2$atheist2012),1,2)))
d2$atheistDiff <- as.integer(gsub("%", "", substr(as.character(d2$atheistDiff),1,2)))

sampSize <- c(1200, 1000, 1671, 1524, 502, 505, 1003, 852, 1003, 1146, 507, 987, 1002, 1000, 984, 1085, 1002, 520, 200, 1000, 1013, 606, 504, 1091, 1207, 1037, 991, 2705, 400, 1000, 1025, 1050, 1209, 1049, 520, 1505, 500)

# Naive approximation of original counts
d2$atheist2005 <- round(sampSize*d2$atheist2005/100)
d2$atheist2012 <- round(sampSize*d2$atheist2012/100)

col1 <- NULL
col2 <- NULL
col3 <- NULL

for(i in 1:(dim(d2)[1])){
  n <- sampSize[i]
  n1 <- d2[i,2]
  n2 <- d2[i,3]
  nletters <- nchar(as.character(d2[i,1]))
  nation <- substr(as.character(d2[i,1]), 1, nletters-1)
  col1 <- c(col1, rep(nation, 2*n))
  col2 <- c(col2, c(sample(rep(c("atheist", "non-atheist"), c(n1, n - n1))), sample(rep(c("atheist", "non-atheist"), c(n2, n - n2)))))
  col3 <- c(col3, rep(c("2005", "2012"), c(n, n)))
}

atheism05_temp <- data.frame("nationality" = as.factor(col1), "response" = as.factor(col2), "year" = as.factor(col3))
atheism05 <- subset(atheism05_temp, atheism05_temp$year == "2005")
write.csv(atheism05, "data/atheism05.csv", row.names = F)

#----- Combine data from two tables ------#

atheism <- rbind(atheism12, atheism05)
write.csv(atheism, "data/atheism.csv", row.names = F)

#----- analysis ------#


us12 <- atheism[atheism$nationality == "United States" & atheism$year == "2012",]
prop.table(table(us12$response))

source("http://stat.duke.edu/courses/Fall12/sta101.001/labs/inference.R")
inference(us12$response, est = "proportion", type = "ci", method = "theoretical", success = "atheist")
inference(us12$response, est = "proportion", type = "ht", null = 0.03, alternative = "twosided", method = "theoretical", success = "atheist")

# Do some analysis
china <- atheism[atheism$nationality == "China",]
inference(china$response, est = "proportion", type = "ht", null = .7, alternative = "less", method = "theoretical", success = "atheist")

ecuador <- atheism[atheism$nationality == "Ecuador",]
inference(ecuador$response, est = "proportion", type = "ci", method = "theoretical", success = "atheist")


# Simulate over n and p
par(mfrow=c(2,2))

p <- 0.1
n <- 1040
p_hats <- rep(0, 5000)

for(i in 1:5000){
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats[i] <- sum(samp == "atheist")/n
}

hist(p_hats, main = "p = 0.1, n = 1040", xlim = c(0, 0.18))

p <- 0.1
n <- 400
p_hats <- rep(0, 5000)

for(i in 1:5000){
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats[i] <- sum(samp == "atheist")/n
}

hist(p_hats, main = "p = 0.1, n = 400", xlim = c(0, 0.18))

p <- 0.02
n <- 1040
p_hats <- rep(0, 5000)

for(i in 1:5000){
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats[i] <- sum(samp == "atheist")/n
}

hist(p_hats, main = "p = 0.02, n = 1040", xlim = c(0, 0.18))

p <- 0.02
n <- 400
p_hats <- rep(0, 5000)

for(i in 1:5000){
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats[i] <- sum(samp == "atheist")/n
}

hist(p_hats, main = "p = 0.02, n = 400", xlim = c(0, 0.18))



# #------ Table 4 - Andrew ------#
# d2 <- read.csv("data/table4.csv", header = F)
# names(d2) <- c("country", "atheist2005", "atheist2012", "atheistDiff")
# 
# d2$atheist2005 <- as.integer(gsub("%", "", substr(as.character(d2$atheist2005),1,2)))
# d2$atheist2012 <- as.integer(gsub("%", "", substr(as.character(d2$atheist2012),1,2)))
# d2$atheistDiff <- as.integer(gsub("%", "", substr(as.character(d2$atheistDiff),1,2)))
# 
# sampSize <- c(1200, 1000, 1671, 1524, 502, 505, 1003, 852, 1003, 1146, 507, 987, 1002, 1000, 984, 1085, 1002, 520, 200, 1000, 1013, 606, 504, 1091, 1207, 1037, 991, 2705, 400, 1000, 1025, 1050, 1209, 1049, 520, 1505, 500)
# 
# # Naive approximation of original counts
# d2$atheist2005 <- round(sampSize*d2$atheist2005/100)
# d2$atheist2012 <- round(sampSize*d2$atheist2012/100)
# 
# col1 <- NULL
# col2 <- NULL
# col3 <- NULL
# 
# for(i in 1:(dim(d2)[1])){
#   n <- sampSize[i]
#   n1 <- d2[i,2]
#   n2 <- d2[i,3]
#   nletters <- nchar(as.character(d2[i,1]))
#   nation <- substr(as.character(d2[i,1]), 1, nletters-1)
#   col1 <- c(col1, rep(nation, 2*n))
#   col2 <- c(col2, c(sample(rep(c("atheist", "non-atheist"), c(n1, n - n1))), sample(rep(c("atheist", "non-atheist"), c(n2, n - n2)))))
#   col3 <- c(col3, rep(c("2005", "2012"), c(n, n)))
# }
# 
# atheism <- data.frame("nationality" = as.factor(col1), "response" = as.factor(col2), "year" = as.factor(col3))
# write.csv(atheism, "data/atheism.csv", row.names = F)




# CODE FOR BENFORDS CHI SQUARE LAB #

# counts <- as.character(iran2$Karrubi)
# ss <- rep(0,366)
# for(i in 1:366){
# ss[i] <- as.integer(substr(counts[i],1,1))}
# table(ss)
# barplot(table(ss))
# 
# nor <- read.csv("~/My Dropbox/lab6/nor.csv")
# komm <- levels(nor$KOMMNAVN)
# party <- levels(nor$PARTIKODE)
# nor$SUM <- as.numeric(nor$SUM)
# n <- length(komm)
# p <- length(party)
# mat <- matrix(rep(0, n*p), n)
# nor2 <- data.frame(mat)
# nor2 <- data.frame(komm, nor2)
# names(nor2) <- c("kommune", party)
# vnames <- as.factor(party)
# 
# for(i in 1:dim(nor)[1]){
#   nor2[which(nor2$komm == nor$KOMMNAVN[i]), which(c(FALSE, vnames == nor$PARTIKODE[i]))] <- nor$SUM[i]
# }
# 
# ptot <- colSums(nor2[,2:211])
# head(order(ptot, decreasing=T))