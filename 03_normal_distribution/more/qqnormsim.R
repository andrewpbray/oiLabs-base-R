qqnormsim <- function(dat){
  par(mfrow = c(3,3))
  qqnorm(dat, main = "Normal QQ Plot (Data)")
  qqline(dat)

  for(i in 1:8){
    simnorm <- rnorm(n = length(dat), mean = mean(dat), sd = sd(dat))
    qqnorm(simnorm,main = "Normal QQ Plot (Sim)")
    qqline(simnorm)
  }
  
  par(mfrow = c(1,1))
}