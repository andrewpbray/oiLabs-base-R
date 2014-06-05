setwd("/Users/mine/Dropbox/OI Labs/lab3")
d = read.csv("ca2010_1_csv_v3.txt", header = TRUE)

names(d)[1] = "county.code"
names(d)[2] = "district.code"
names(d)[3] = "school.code"
names(d)[4] = "charter.number"
names(d)[5] = "test.year"
names(d)[6] = "subgroup.id"
names(d)[7] = "test.type"
names(d)[8] = "capa.assessment.level"
names(d)[9] = "total.d.enrollment"
names(d)[10] = "total.tested.at.entity.level"
names(d)[11] = "total.tested.at.subgroup.level"
names(d)[12] = "grade"
names(d)[13] = "testID"
names(d)[14] = "star.reported.enrollment.capa.eligible"
names(d)[15] = "students.tested"
names(d)[16] = "percent.tested"
names(d)[17] = "score"
names(d)[18] = "perc.advanced"
names(d)[19] = "perc.proficient"
names(d)[20] = "perc.at.or.above.proficient"
names(d)[21] = "perc.basic"
names(d)[22] = "perc.below.basic"
names(d)[23] = "perc.far.below.basic"
names(d)[24] = "students.with.scores"
names(d)[25] = "cma.sts.average.percent.correct"

d$test.type= as.character(d$test.type)
d$test.type[d$test.type == "C"] = "cst"
d$test.type[d$test.type == "M"] = "cma"
d$test.type[d$test.type == "P"] = "capa"
d$test.type[d$test.type == "S"] = "sts"
d$test.type = as.factor(d$test.type)

d$score[d$score == ""] = NA
d$score[d$score == "*"] = NA

d$perc.advanced[d$perc.advanced == ""] = NA
d$perc.advanced[d$perc.advanced == "*"] = NA

d$perc.proficient[d$perc.proficient == ""] = NA
d$perc.proficient[d$perc.proficient == "*"] = NA

d$perc.at.or.above.proficient[d$perc.at.or.above.proficient == ""] = NA
d$perc.at.or.above.proficient[d$perc.at.or.above.proficient == "*"] = NA

d$perc.basic[d$perc.basic == ""] = NA
d$perc.basic[d$perc.basic == "*"] = NA

d$perc.below.basic[d$perc.below.basic == ""] = NA
d$perc.below.basic[d$perc.below.basic == "*"] = NA

d$perc.far.below.basic[d$perc.far.below.basic == ""] = NA
d$perc.far.below.basic[d$perc.far.below.basic == "*"] = NA

d$testID = as.character(d$testID)
d$testID[d$testID == "7"] = "ela"
d$testID[d$testID == "8"] = "math"
d$testID[d$testID == "9"] = "algebra I"
d$testID[d$testID == "10"] = "integ math 1"
d$testID[d$testID == "11"] = "geometry"
d$testID[d$testID == "12"] = "integ math 2"
d$testID[d$testID == "13"] = "algebra II"
d$testID[d$testID == "14"] = "integ math 3"
d$testID[d$testID == "15"] = "summative hs math"
d$testID[d$testID == "18"] = "world hist"
d$testID[d$testID == "19"] = "us hist"
d$testID[d$testID == "20"] = "bio"
d$testID[d$testID == "21"] = "chem"
d$testID[d$testID == "22"] = "earth sci"
d$testID[d$testID == "23"] = "physics"
d$testID[d$testID == "24"] = "sci 1"
d$testID[d$testID == "25"] = "sci 2"
d$testID[d$testID == "26"] = "sci 3"
d$testID[d$testID == "27"] = "sci 4"
d$testID[d$testID == "28"] = "gen math"
d$testID[d$testID == "29"] = "hist soc sci gr 8"
d$testID[d$testID == "30"] = "capa - ela"
d$testID[d$testID == "31"] = "capa - math"
d$testID[d$testID == "32"] = "life sci gr 5, 8, and 10"
d$testID[d$testID == "38"] = "sts - rla"
d$testID[d$testID == "39"] = "sts - math"
d$testID[d$testID == "43"] = "capa - sci"
d$testID[d$testID == "44"] = "cma - ela"
d$testID[d$testID == "45"] = "cma - ela"
d$testID[d$testID == "46"] = "cma - sci"
d$testID[d$testID == "47"] = "sts - algebra I"
d$testID[d$testID == "48"] = "sts - geometry"
d$testID[d$testID == "49"] = "cma - algebra II"
d$testID[d$testID == "50"] = "cma - life sci"
d$testID = as.factor(d$testID)

d$score = as.numeric(d$score)

dCST = d[d$test.type == "cst",]

dCST$grade = as.factor(dCST$grade)
levels(dCST$grade)[11] = "EOC"
star <- dCST[dCST$subgroup.id == 1,]

star = dCST[,c(12,13,17:23)]

star$testID = factor(star$testID)

save(star, file = "star.RData")