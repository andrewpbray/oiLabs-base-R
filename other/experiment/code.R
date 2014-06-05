setwd("~/Desktop/Teaching/Sta 101 - F12/Labs/experiment")

d1 = read.csv("Lab survey - Week 1 - Sheet1.csv")
d2 = read.csv("Lab survey - Week 2 - Sheet1.csv")

# d1
names(d1) = c("timestamp","difficulty","identify_args","no_args","color","identifier")
levels(d1$color) = c("multiple","single")
dim(d1)

# compare difficulty
boxplot(d1$difficulty ~ d1$color, ylab = "difficulty, 1-very easy, 5-very hard")
t.test(d1$difficulty ~ d1$color)

# compare %correct on identify_args
d1$identify_args_score = NA
d1$identify_args_score[d1$identify_args == "function: names, argument: present"] = 1
d1$identify_args_score[d1$identify_args == "function: present, argument: names"] = 0
mosaicplot(table(d1$color,d1$identify_args_score))
chisq.test(table(d1$color,d1$identify_args_score))

# compare %correct on no_args
d1$no_args_score = NA
d1$no_args_score[d1$no_args == 3] = 1
d1$no_args_score[d1$no_args != 3] = 0
mosaicplot(table(d1$color,d1$no_args_score))
chisq.test(table(d1$color,d1$no_args_score))


# d2
names(d2) = c("timestamp","difficulty","whats_wrong1","whats_wrong2","compare_diff","rewrite", "rewrite_score","typos","color","identifier")
levels(d2$color) = c("multiple","single")
dim(d2)

# compare difficulty
boxplot(d2$difficulty ~ d2$color, ylab = "difficulty, 1-very easy, 5-very hard")
t.test(d2$difficulty ~ d2$color)

# compare %correct on whats_wrong1
d2$whats_wrong1_score = NA
d2$whats_wrong1_score[d2$whats_wrong1 == "30 should not be in quotation marks."] = 1
d2$whats_wrong1_score[d2$whats_wrong1 != "30 should not be in quotation marks."] = 0
mosaicplot(table(d2$color,d2$whats_wrong1_score))
chisq.test(table(d2$color,d2$whats_wrong1_score))

# compare %correct on whats_wrong2
d2$whats_wrong2_score = NA
d2$whats_wrong2_score[d2$whats_wrong2 == " = should instead be ==."] = 1
d2$whats_wrong2_score[d2$whats_wrong2 != " = should instead be ==."] = 0
mosaicplot(table(d2$color,d2$whats_wrong2_score))
chisq.test(table(d2$color,d2$whats_wrong2_score))

# compare %correct on rewrite
mosaicplot(table(d2$color,d2$rewrite_score))
chisq.test(table(d2$color,d2$rewrite_score))

# typos
mosaicplot(table(d2$color,d2$typos), las = 1)
chisq.test(table(d2$color,d2$typos))

# compare_diff
levels(d2$compare_diff) = c("this easier", "this more diff", "this same")
mosaicplot(table(d2$color,d2$compare_diff), las = 1)
chisq.test(table(d2$color,d2$compare_diff))

# compare d2 to d1

d = merge(d1,d2,by = c("identifier","identifier"))
d = d[d$identifier != "123",]  # 4 people chose 123!
dim(d)

# sanity check
table(d$color.x, d$color.y)

# change in perceived difficulty
d$diff_diff = d$difficulty.x - d$difficulty.y
boxplot(d$diff_diff~d$color.x, xlab = "color in week 1", ylab = "week 1 difficulty - week 2 difficulty")
boxplot(d$diff_diff~d$color.y, xlab = "color in week 2", ylab = "week 1 difficulty - week 2 difficulty")


