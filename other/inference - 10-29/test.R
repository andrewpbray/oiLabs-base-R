setwd("~/Desktop/Teaching/Sta 104 - Su13/Lab/inference")

source("inference.R")

mature = c(rep("mature mom", 132), rep("younger mom", 867))
married = c(rep("married", 25), rep("not married", 107), rep("married", 361), rep("not married", 506))
momdata = as.data.frame(cbind(mature,married))

inference(momdata$mature, momdata$married, type = "ci", method = "theoretical", 
          est = "proportion", success = "mature mom", null = 0.5, alternative = "less", 
          nsim = 100, eda_plot = FALSE, inf_plot = FALSE)





d = c(rep(rnorm(40,4)), rep(rnorm(45,4)), rep(rnorm(50,9)), rep(rnorm(55,4)))
gr = c(rep("gr1",40), rep("gr2",45), rep("gr3",50), rep("gr4",55))
da = data.frame(d,gr)
da$d = as.numeric(da$d)
da$gr = as.factor(da$gr)



inference(da$d, da$gr, type = "ht", method = "theoretical", est = "mean", nsim = 100,
          eda_plot = FALSE, inf_plot = TRUE)


inference(da$d, da$gr, type = "ht", method = "sim", est = "mean", nsim = 100)

dd = rnorm(100)
inference(dd, type = "ht", alternative = "less", null = 0, method = "simulation", est = "mean", inf_plot = TRUE, nsim = 1000)

gr = c(rep("gr1",40), rep("gr2",45))
inference(gr, type = "ci", success = "gr1", method = "sim", est = "proportion", nsim = 100)


source("inference.R")
d = c(rep(rnorm(40,4)), rep(rnorm(45,4.5)))
gr = c(rep("gr1",40), rep("gr2",45))
da = data.frame(d,gr)
da$d = as.numeric(da$d)
da$gr = as.factor(da$gr)

pdf("a.pdf")
ddd = inference(da$d, da$gr, type = "ci", method = "simulation", est = "mean", nsim = 100, simdist = TRUE)
dev.off()

source("inference.R")


d = c(rep(rnorm(40,4)), rep(rnorm(45,4)), rep(rnorm(50,4)), rep(rnorm(55,4)))
gr = c(rep("gr1",40), rep("gr2",45), rep("gr3",50), rep("gr4",55))
da = data.frame(d,gr)
da$d = as.numeric(da$d)
da$gr = as.factor(da$gr)

mature = c(rep("mature mom", 108), rep("younger mom", 867), rep("mom", 50))
married = c(rep("married", 1), rep("not married", 107), rep("married", 361), rep("not married", 506), rep("married",20), rep("not married", 30))
momdata = as.data.frame(cbind(mature,married))
source("inference.R")
inference(momdata$mature, momdata$married, type = "ht", method = "theoretical", alternative = "greater", est = 'proportion')

mature = c(rep("mature mom", 3), rep("younger mom", 2), rep("mom", 1))
married = c(rep("married", 1), rep("not married", 1), rep("married", 1), rep("not married", 1), rep("married",1), rep("not married", 1))
momdata = as.data.frame(cbind(mature,married))
source("inference.R")
inference(momdata$mature, momdata$married, type = "ht", method = "theoretical", alternative = "greater", est = 'proportion')

data = momdata$mature
group = momdata$married
type = "ht"
method = "theoretical"
est = "proportion"
null = NULL

inference(momdata$mature, momdata$married, type = "ht", method = "theoretical", est = "proportion", null = 3)

d = c(rep("suc",50), rep("fail",35))
gr = c(rep("gr1",40), rep("gr2",45))
da = data.frame(d,gr)
da$d = as.factor(da$d)
da$gr = as.factor(da$gr)

inference(da$d, da$gr, type = "ht", method = "simulation", est = "proportion", null = 0, alternative = "less", success = "suc")
data = da$d
group = da$gr

d = rnorm(50)
inference(d, type = "ci", method = "theoretical", est = "mean", null = -0.5, alternative = "twosided")

data = as.factor(c(rep("suc",50), rep("fail",35)))
inference(data = data, success = "suc", type = "ht", method = "simulation", est = "proportion", null = 0.5, alternative = "twosided", nsim = 100)

group = NULL
type = "ht"
method = "theoretical"
est = "mean"
null = -0.5
alternative = "twosided"
success = "suc"
