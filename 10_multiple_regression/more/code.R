evals <- read.csv("evals.csv")

multiLines <- function(model, ...){
  if(class(model)!="lm"){
    warning("Model must be the output of the function lm()")
  }
  
  if(length(model$xlevels)!=1){
    warning("Model must contain exactly one categorical predictor")
  }
  
  if(length(model$coef)-length(model$xlevels[[1]])!=1){
    warning("Model must contain exactly one non-categorical predictor")
  }
  
  palette <- c("#E69F00", "#56B4E9", "#D55E00", "#009E73", "#CC79A7", "#F0E442", "#0072B2")
  
  baseIntercept <- model$coef[1]
  nLines <- length(model$xlevels[[1]])
  intercepts <- c(baseIntercept, rep(0, nLines-1))
  indicatorInd <- c(1, rep(0, nLines)) # used to find slope parameter by process of elimination
  
  for(i in 1:(nLines-1)){
    indicatorName <- paste(names(model$contrasts),model$xlevels[[1]][1+i], sep = "")
    intercepts[i+1] <- baseIntercept + model$coef[names(model$coef)==indicatorName]
    indicatorInd <- indicatorInd + (names(model$coef)==indicatorName)
  }
  
  slope <- model$coef[!indicatorInd]
  
  num_pred = which(names(model$model[,-1]) != names(model$xlevels)) + 1
  cat_pred = which(names(model$model[,-1]) == names(model$xlevels)) + 1
  
  model$model$COL = NA
  model$model$PCH = NA
  for(i in 1:nLines){
    model$model$COL[model$model[,cat_pred] == levels(model$model[,cat_pred])[i]] = adjustcolor(palette[i],0.40)
    model$model$PCH[model$model[,cat_pred] == levels(model$model[,cat_pred])[i]] = i+14
  }
  
  plot(model$model[,1] ~ jitter(model$model[,num_pred]), col = model$model$COL, pch = model$model$PCH,
       ylab = names(model$model)[1],
       xlab = names(model$model)[num_pred])
    
  for(j in 1:nLines){
    abline(intercepts[j], slope, col = palette[j], lwd = 2, ...)
  }

  if(slope > 0){legend_pos = "bottomright"}
  if(slope < 0){legend_pos = "topleft"}  
  
  legend(legend_pos, col = palette[1:nLines], lty = 1, legend = levels(model$model[,cat_pred]), lwd = 2)
}

save(evals, multiLines, file="evals.RData")
