# inference
inference <- function(y, x = NULL, 
                      est = c("mean", "median", "proportion"), 
                      success = NULL, order = NULL, 
                      conflevel = 0.95, siglevel = 0.05,
                      null = NULL, 
                      alternative = c("less","greater","twosided"), 
                      type = c("ci","ht"), method = c("theoretical","simulation"),
                      simdist = FALSE, nsim = 10000, seed = NULL,
                      sum_stats = TRUE, eda_plot = TRUE, inf_plot = TRUE, inf_lines = TRUE) {
  # y: variable 1, can be numerical or categorical
  # x: variable 2, categorical (optional)
  # est: parameter to estimate, mean, median, or proportion
  # success: which level of the categorical variable to call "success", i.e. do inference on
  # order: when x is given, order of xs in which to subtract parameters
  # nsim: number of simulations
  # level: confidence level, value between 0 and 1
  
  # set mirror
  options(repos=structure(c(CRAN="http://cran.rstudio.com")))

  # load packages, if needed
  if (!("lmPerm" %in% names(installed.packages()[,"Package"]))) {install.packages("lmPerm")}
  suppressMessages(library(lmPerm, quietly = TRUE))

  if (!("openintro" %in% names(installed.packages()[,"Package"]))) {install.packages("openintro")}
  suppressMessages(library(openintro, quietly = TRUE))

  if (!("BHH2" %in% names(installed.packages()[,"Package"]))) {install.packages("BHH2")}
  suppressMessages(library(BHH2, quietly = TRUE))

  # names for plot labels
  y_name = deparse(substitute(y))
  x_name = deparse(substitute(x))

  # set seed
  if(!is.null(seed)){set.seed(seed)}
  
  # plotting settings
  if(eda_plot == TRUE & inf_plot == TRUE) {par(mfrow=c(1,2), mar = c(4,2,0.5,0.5))}
  if(any(c(inf_plot,eda_plot) == FALSE)) {par(mfrow=c(1,1), mar = c(4,2,0.5,0.5))}
  
  # error: weird y
  if (length(y) == 1) {stop("Sample size of y is 1.", call. = FALSE)}
  
  # handling of NAs: drop NAs, and if x is given, use pairwise complete
  if (is.null(x)) {if (sum(is.na(y)) > 0) {y = y[!is.na(y)]}}
  if (!is.null(x)) {
    if (sum(is.na(y)) > 0 | sum(is.na(x)) > 0) {
      y.temp = y[!is.na(y) & !is.na(x)]
      x.temp = x[!is.na(y) & !is.na(x)]
      y = y.temp
      x = x.temp
    }
  }
  
  # error: y or x is character, make factor
  if (is.character(y)) {y = as.factor(y)}
  if (is.character(x)) {x = as.factor(x)}
  
  # error: variables not of same length
  if (!is.null(x)) {
    if (length(y) != length(x)) {stop("The two variables must be of same length.", call. = FALSE)}
  }
  
  # set variable type for y: numerical or categorical
  y_type = "categorical"
  if (is.numeric(y)) {y_type = "numerical"}
  
  # set variable type for x: categorical, numerical (unused), or only1var
  if (!is.null(x)) {
    x_type = "categorical"
    if (is.numeric(x)) {x_type = "numerical"}
  }  
  if (is.null(x)) {x_type = "only1var"}
  
  # error: explanatory variable numerical, convert to categorical
  if(x_type == "numerical"){
    x = as.factor(x)
    x_type = "categorical"		
    warning("Explanatory variable was numerical, it has been converted to categorical. In order to avoid this warning, first convert your explanatory variable to a categorical variable using the as.factor() function.", call. = FALSE)
  }
  
  # print variable types
  if (x_type == "only1Var") {cat(paste("One", y_type, "variable", "\n"))}
  if (x_type == "categorical") {cat(paste("Response variable: ", y_type, ", Explanatory variable: ", x_type, "\n", sep = ""))}
  
  # set number of levels
  x_levels = 0  # numerical variable
  y_levels = 0   # numerical variable
  if (x_type == "categorical") {x_levels = length(levels(x))}
  if (y_type == "categorical") {y_levels = length(levels(y))}
  
  # error: missing type, method, est
  if (length(type) > 1) {stop("Missing type: ci (confidence interval) or ht (hypothesis test).", call. = FALSE)}
  if (length(method) > 1) {stop("Missing method: theoretical or simulation.", call. = FALSE)}
  if (length(est) > 1) {stop("Missing estimate: mean, median, or proportion.", call. = FALSE)}
  
  # error: method isn't theoretical or simulation
  method_list = c("theoretical", "simulation")
  method = tolower(gsub("\\s","", method))
  which_method = pmatch(method, method_list)
  if(is.na(which_method)){stop("Method should be 'theoretical' or 'simulation'.", call. = FALSE)}
  method = method_list[which_method]
  
  # error: type isn't ci or ht
  type_list = c("ci", "ht")
  type = tolower(gsub("\\s","", type))
  which_type = pmatch(type, type_list)
  if(is.na(which_type)){stop("Type should be 'ci' or 'ht'.", call. = FALSE)}
  type = type_list[which_type]
  
  # error: alternative isn't less, greater, or twosided
  if(type == "ht"){
    alternative_list = c("less","greater","twosided","notequal")
    alternative = tolower(gsub("\\s","", alternative))
    which_alternative = pmatch(alternative, alternative_list)
    if((length(which_alternative) == 1) & any(is.na(which_alternative))){
      stop("Alternative should be 'less', 'greater' or 'twosided'.", call. = FALSE)      
    }
    if(any(which_alternative == 4)) {which_alternative = 3}
    alternative = alternative_list[which_alternative] 
  }
  
  # error / warning: issues with null values
  if (type == "ht" & x_type == "only1Var" & is.null(null)) {stop("Missing null value.", call. = FALSE)}
  if (type == "ht" & (x_levels == 2 & y_levels <= 2) & is.null(null)) {
    null = 0
    warning("Missing null value, set to 0.", call. = FALSE)
  }
  if (type == "ht" & (x_levels > 2 | y_levels > 2) & !is.null(null)) {
    if(y_type == "numerical"){
      warning("Ignoring null value since it's undefined for ANOVA.", call. = FALSE)      
    }
    if(y_type == "categorical"){
      warning("Ignoring null value since it's undefined for chi-square test.", call. = FALSE)      
    }
  }
  
  # error / warning: issues with alternative  
  if (type == "ht" & length(alternative) > 1) {
    if(x_levels <= 2 & y_levels <= 2) {stop("Missing alternative: less, greater, twosided.", call. = FALSE)}
    if(x_levels > 2 | y_levels > 2) {
      if(y_type == "numerical") {
        alternative = "greater"
        warning("Use alternative = 'greater' for ANOVA.", call. = FALSE)
      }
      if(y_type == "categorical") {
        alternative = "greater"
        warning("Use alternative = 'greater' for chi-square test.", call. = FALSE)
      }
    }
  }
  
  # error: categorical variables have more than two levels, but type is ci
  if ((x_levels > 2 | y_levels > 2) & type == "ci") {
    if(y_type == "numerical"){
      stop("Categorical variable has more than 2 levels, confidence interval is undefined, use ANOVA to test for a difference between means.", call. = FALSE)
    }
    if(y_type == "categorical"){
      stop("Categorical variable has more than 2 levels, confidence interval is not defined, use chi-square test of independence.", call. = FALSE)
    }
  } 
  if ((x_levels > 2 | y_levels > 2) & est == "median") {
    if(y_type == "numerical"){
      stop("This function cannot be used to compare medians across more than 2 xs, use est = 'mean' for ANOVA.", call. = FALSE)
    }
  }
  
  # error: estimate isn't mean, median, or proportion
  if (est %in% c("mean", "median", "proportion") == FALSE) {
    stop("Estimate should be 'mean', 'median', or 'proportion'.", call. = FALSE)
  }
  
  # error: wrong estimate
  if (y_type == "numerical" & est == "proportion") {
    stop("Variable is numerical, sample statistic cannot be a proportion, choose either mean or median.", call. = FALSE)
  }  
  if (y_type == "categorical" & (est == "mean" | est == "median")) {
    stop("Variable is categorical, sample statistic cannot be a mean or a median, use proportion.", call. = FALSE)
  } 
  
  # error: x variable has more than two levels, but alternative is not defined properly (chi-square and ANOVA)
  if (x_type == "categorical" & x_levels > 2 & length(alternative) == 1) {
    if(alternative != "greater"){stop("Use alternative = 'greater' for ANOVA or chi-square test.", call. = FALSE)}
  }
  
  # errors about success
  if ((y_type == "categorical" & x_levels == 2 & y_levels == 2) | (y_type == "categorical" & is.null(x))) {
    # error: success not provided for categorical variable for 1 or 2 proportion ci or ht
    if (is.null(success)) {
      y_level_names = levels(y)
      stop(paste("Variable is categorical, specify which level to call success: ", y_level_names[1], " or ", y_level_names[2]), call. = FALSE)
    }
    # error: success provided is not a level of the categorical variable
    if (success %in% levels(y) == FALSE) {
      stop(paste(success,"is not a level of the success variable."), call. = FALSE)
    }
  }
  
  # warning: success provided for numerical variable
  if (y_type == "numerical" & !is.null(success)) {
    warning("Ignoring success since y are numerical.\n", call. = FALSE)
  }
  
  # warning: confidence level greater than 1
  if (conflevel > 1) {
    conflevel = conflevel / 100
    warning(paste("Confidence level converted to ", conflevel, ".", sep = ""), call. = FALSE)    
  }
  
  # warning: significance level greater than 1
  if (siglevel > 1) {
    siglevel = siglevel / 100
    warning(paste("Significance level converted to ", siglevel, ".", sep = ""), call. = FALSE)    
  }
  
  # define sample size
  n = length(y)
  
  # define sign of hypothesis test, for one and two means, medians, and proportions
  if (type == "ht") {
    if (alternative == "less") {sign = "<"}
    if (alternative == "greater") {sign = ">"}
    if (alternative == "twosided") {sign = "!="}		
  }
  
  # one variable
  if (x_type == "only1var") {
    
    # print what's going on 
    if(y_type == "numerical"){cat("Single", est, "\n")}
    if(y_type == "categorical"){cat("Single", est, "-- success:", success, "\n")}    
    
    # set statistic: mean, median, or proportion
    if (y_type == "numerical") {statistic = match.fun(est)}
    if (y_type == "categorical") {statistic = function(x) {sum(x == success)/length(x)}}
    
    actual = statistic(y)
    
    cat("Summary statistics: ")
    if(est == "mean"){
      if(eda_plot == TRUE){hist(y, main = "", cex.main = 0.75, xlab = y_name, col = COL[3,4], ylab = "")}
      cat(paste("mean =", round(actual,4), "; ","sd =", round(sd(y), 4), "; ", "n =", n), "\n")
    }
    if(est == "median"){
      if(eda_plot == TRUE){
        boxplot(y, main = "", main = "", xlab = y_name, col = COL[3,4], axes = FALSE)
        axis(2)
      }
      cat(paste("median =", round(actual,4), "; ", "n =", n), "\n")
    }
    if(est == "proportion"){
      if(eda_plot == TRUE){barplot(table(y), main = "", xlab = y_name, col = COL[3,4])}
      cat(paste("p_hat =", round(actual,4), "; ", "n =", n), "\n")
    }
    
    # simulation
    if (method == "simulation") {
      sim = matrix(NA, nrow = n, ncol = nsim)
      
      # bootstrap ci
      if (type == "ci") {
        for(i in 1:nsim) {sim[,i] = sample(y, n, replace = TRUE)}
        if (y_type == "categorical") {
          statistic = function(x) {
            which_success = which(levels(y) == success)
            sum(x == which_success)/length(x)
          }
        }				
        sim_dist = apply(sim, 2, statistic)
        
        ci = quantile(sim_dist, c( (1 - conflevel)/2 , ((1 - conflevel)/2)+conflevel ))
        
        if(inf_plot == TRUE){
          #if(y_type == "categorical"){xlim = c(min(sim_dist)-sd(sim_dist), max(sim_dist)+sd(sim_dist))}
          #if(y_type == "numerical"){xlim = c(min(sim_dist)*0.8, max(sim_dist)*1.2)}
          xlim = c(min(sim_dist)-0.8*sd(sim_dist), max(sim_dist)+0.8*sd(sim_dist))
          
          if (nsim > 500) {            
            counts = hist(sim_dist, plot = FALSE)$counts
            hist(sim_dist, xlab = "Bootstrap distribution", main = "", ylab = "", xlim = xlim, col = COL[1,2])
            if (inf_lines == TRUE) {
              for (i in 1:2) {
                segments(ci[i], 0, ci[i], 0.8 * max(counts), col=COL[4], lwd=2)
                text(round(ci[i],4), max(counts), pos=1, col=COL[4], round(ci[i],4))
              }
            }
          }
          if (nsim <= 500) {
            BHH2::dotPlot(sim_dist, xlim = xlim, pch=19, col=COL[1,2], cex = 0.8, axes = FALSE, xlab="Bootstrap distribution")
            axis(1)
            if(inf_lines == TRUE){
              for (i in 1:2) {
                segments(ci[i], 0, ci[i], 0.6, col=COL[4], lwd=2)
                text(round(ci[i],4), 0.7, pos=1, col=COL[4], round(ci[i],4))
              }
            }
          }          
        }
        
        cat(c(conflevel*100, "% Bootstrap interval = (", round(ci[1],4), ",", round(ci[2],4), ")\n"))		
      }
      
      # randomization test
      if (type == "ht") {
        if (y_type == "numerical") {
          for(i in 1:nsim) {sim[,i] = sample(y, n, replace = TRUE)}
          sim_dist_temp = apply(sim, 2, statistic)
          if (est == "mean") {
            # hypotheses
            cat(paste("H0: mu =", null, "\n"))
            cat(paste("HA: mu", sign, null, "\n"))
            sim_dist = sim_dist_temp - (mean(sim_dist_temp) - null)
          }
          
          if (est == "median") {
            cat(paste("H0: median =", null, "\n"))
            cat(paste("HA: median", sign, null, "\n"))
            sim_dist = sim_dist_temp - (median(sim_dist_temp) - null)
          }					
        }
        if (y_type == "categorical") {
          cat(paste("H0: p =", null, "\n"))
          cat(paste("HA: p", sign, null, "\n"))
          sim_dist = rbinom(nsim, n, prob = null) / n
        }
        
        smaller_tail = round(min(c(mean(sim_dist <= actual), mean(sim_dist >= actual))), 4)	
        
        if(inf_plot == TRUE){
          #if(y_type == "categorical"){xlim = c(min(sim_dist)-sd(sim_dist), max(sim_dist)+sd(sim_dist))}
          #if(y_type == "numerical"){xlim = c(min(sim_dist)*0.8, max(sim_dist)*1.2)}
          xlim = c(min(sim_dist)-0.8*sd(sim_dist), max(sim_dist)+0.8*sd(sim_dist))
          
          if (nsim > 500) {
            counts = hist(sim_dist, plot = FALSE)$counts  
            hist(sim_dist, xlab = "Randomization distribution", main = "", ylab = "", xlim = xlim, col = COL[1,2])
          }
          if (nsim <= 500) {
            BHH2::dotPlot(sim_dist, xlim = xlim, pch=19, col=COL[1,2], cex = 0.8, axes = FALSE, xlab="Randomization distribution")
            axis(1)
          }
        }          
        
        
        #alternative = match.arg(alternative)
        
        if (alternative == "less") {
          if (actual < null) {cat(paste("p-value = ", smaller_tail,"\n"))}  			
          if (actual > null) {cat(paste("p-value = ", 1 - smaller_tail,"\n"))}
          if (inf_lines == TRUE) {
            if(nsim > 500) {lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col=COL[4], lwd=2)}
            if(nsim <= 500) {lines(x = c(actual,actual), y = c(0,0.8), col=COL[4], lwd=2)}
          }
        }
        if (alternative == "greater") {
          if (actual < null) {cat(paste("p-value = ", 1 - smaller_tail,"\n"))}				
          if (actual > null) {cat(paste("p-value = ", smaller_tail,"\n"))}
          if (inf_lines == TRUE) {
            if(nsim > 500) {lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col=COL[4], lwd=2)}
            if(nsim <= 500) {lines(x = c(actual,actual), y = c(0,0.8), col=COL[4], lwd=2)}
          }
        }
        if (alternative == "twosided") {
          cat(paste("p-value = ", smaller_tail * 2,"\n"))
          lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col=COL[4], lwd=2)
          if (actual >= null) {
            temp = actual - null
            if (inf_lines == TRUE) {
              if(nsim > 500) {lines(x = c(null - temp,null - temp), y = c(0,1.1*max(counts)), col=COL[4], lwd=2)}
              if(nsim <= 500) {lines(x = c(null - temp,null - temp), y = c(0,0.8), col=COL[4], lwd=2)}
            }						
          }
          if (actual < null) {
            temp = null - actual
            if (inf_lines == TRUE) {
              if(nsim > 500) {lines(x = c(null + temp,null + temp), y = c(0,1.1*max(counts)), col=COL[4], lwd=2)}
              if(nsim <= 500) {lines(x = c(null + temp,null + temp), y = c(0,0.8), col=COL[4], lwd=2)}
            }						
          }		
        }
        if (inf_lines == TRUE) {
          if(nsim > 500) {text(x = actual, y = 1.2*max(counts), paste("observed\n", round(actual,4)), col=COL[4], cex = 0.8)}
          if(nsim <= 500) {text(x = actual, y = 0.9, paste("observed\n", round(actual,4)), col=COL[4], cex = 0.8)}          
        }	
      }
      
    }		
    
    
    # theoretical
    if (method == "theoretical") {
      
      # confidence interval
      if (type == "ci") {
        if (y_type == "numerical") {
          if (est == "median") {stop("Use simulation methods for inference for the median.", call. = FALSE)}
          if (est == "mean") {
            # calculate me and se
            se = sd(y) / sqrt(n)
            cat(paste("Standard error =", round(se, 4), "\n"))
            if (n >= 30) {critvalue = qnorm( (1 - conflevel)/2 + conflevel )}
            if (n < 30) {critvalue = qt( (1 - conflevel)/2 + conflevel , df = n - 1)}					
          }
        }
        if (y_type == "categorical") {
          # check conditions
          suc = round(n * actual, 2)
          fail = round(n * (1 - actual), 2)
          cat(paste("Check conditions: number of successes =", round(suc), ";", "number of failures =", round(fail)), "\n")	
          if (suc < 10 | fail < 10) {
            stop("There aren't at least 10 successes and 10 failures, use simulation methods instead.", call. = FALSE)
          }
          # calculate me and se
          se = sqrt(actual * (1-actual) / n)
          cat(paste("Standard error =", round(se, 4), "\n"))
          critvalue = qnorm( (1 - conflevel)/2 + conflevel )					
        }
        me = critvalue * se
        ci = c(actual - me , actual + me)
        cat(c(conflevel*100, "% Confidence interval = (", round(ci[1],4), ",", round(ci[2],4), ")\n"))	
      }
      
      # hypothesis test
      if (type == "ht") {
        if (y_type == "numerical") {
          if (est == "median") {stop("Use simulation methods for inference for the median.", call. = FALSE)}
          if (est == "mean") {
            # hypotheses
            cat(paste("H0: mu =", null, "\n"))
            cat(paste("HA: mu", sign, null, "\n"))
            
            # calculate test statistic and p-value component
            se = sd(y) / sqrt(n)
            cat("Standard error =", round(se,4), "\n")
            teststat = (actual - null)/se
            if (n >= 30) {
              cat(paste("Test statistic: Z =", round(teststat, 3),"\n"))
              smaller_tail = round(min(pnorm(teststat), pnorm(teststat, lower.tail = FALSE)), 4)
            }
            if (n < 30) {
              cat(paste("Test statistic: T =", round(teststat, 3),"\n"))
              cat(paste("Degrees of freedom: ", n - 1, "\n"))
              smaller_tail = round(min(pt(teststat, df = n - 1), pt(teststat, df = n - 1, lower.tail = FALSE)), 4)
            }		
          }
        }
        if (y_type == "categorical") {
          if (null < 0 | null > 1) {
            stop("Null value should be a proportion between 0 and 1.", call. = FALSE)
          }
          # hypotheses
          cat(paste("H0: p =", null, "\n"))
          cat(paste("HA: p", sign, null, "\n"))
          
          # check conditions
          exp_suc = round(n * null, 2)
          exp_fail = round(n * (1 - null), 2)
          cat(paste("Check conditions: number of expected successes =", round(exp_suc), ";", "number of expected failures =", round(exp_fail)), "\n")
          if (exp_suc < 10 | exp_fail < 10) {
            stop("There aren't at least 10 expected successes and 10 expected failures, use simulation methods instead.", call. = FALSE)
          }
          # calculate test statistic and p-value
          se = sqrt(null * (1 - null) / n)
          cat("Standard error =", round(se,4), "\n")
          teststat = (actual - null)/se
          cat(paste("Test statistic: Z = ", round(teststat, 3),"\n"))
          smaller_tail = round(min(pnorm(teststat), pnorm(teststat, lower.tail = FALSE)), 4)					
        }
        
        # alternative = less
        if (alternative == "less") {
          if (actual < null) {cat(paste("p-value = ", smaller_tail,"\n"))}				
          if (actual > null) {cat(paste("p-value = ", 1 - smaller_tail,"\n"))}
          normTail(L = teststat, axes = FALSE, col = COL[1,2])
          axis(1, at = c(-3, teststat, 0, 3), labels = c(NA, paste(round(actual,2)), paste(null), NA))
        }
        
        # alternative = greater
        if (alternative == "greater") {
          if (actual < null) {cat(paste("p-value = ", 1 - smaller_tail,"\n"))}				
          if (actual > null) {cat(paste("p-value = ", smaller_tail,"\n"))}
          if(inf_plot == TRUE){
            normTail(U = teststat, axes = FALSE, col = COL[1,2])
            axis(1, at = c(-3, 0, teststat, 3), labels = c(NA, paste(null), paste(round(actual,2)), NA))
          }
        }
        
        # alternative = twosided	
        if (alternative == "twosided") {
          cat(paste("p-value = ", smaller_tail * 2,"\n"))
          if (inf_lot == TRUE){
            if (actual < null) {
              normTail(L = teststat, U = -1*teststat, axes = FALSE, col = COL[1,2])
              axis(1, at = c(-3, teststat, 0, -1*teststat, 3), labels = c(NA, paste(round(actual,2)), paste(null), paste(round(null + (null - actual), 2)), NA))
            }
            if (actual > null) {
              normTail(L = -1*teststat, U = teststat, axes = FALSE, col = COL[1,2])
              axis(1, at = c(-3, -1*teststat, 0, teststat, 3), labels = c(NA, paste(round(null - (actual - null), 2)), paste(null), paste(round(actual,2)), NA))
            }
          }
        }
      }
    }	
  }
  
  # two variables
  if (x_type == "categorical") {
    
    # print what's going on
    if (y_type == "numerical") {
      if (x_levels == 2) {cat("Difference between two ", est, "s", "\n", sep = "")}
      if (x_levels > 3) {cat("ANOVA\n\n")}      
    }
    
    if (y_type == "categorical") {
      cat("Two categorical variables\n")
      if (x_levels == 2 & y_levels == 2) {cat("Difference between two ", est, "s -- success: ", success, "\n", sep = "")}
      if (x_levels > 2 | y_levels > 2) {cat("Chi-square test of independence\n")}
    }
    
    # x variable with 2 levels
    if (x_levels == 2 & (y_type == "numerical" | (y_type == "categorical" & y_levels == 2))) {
      # order
      if (is.null(order)) {order = levels(x)}
      if (length(order) == 1 & !is.na(order[1])) {
        stop("Order cannot be of length 1, list the order in which two levels of the xing variable should be subtracted.", call. = FALSE)
      }
      if (length(order) == 2) {
        if ( all(order %in% levels(x)) == FALSE) {
          str = paste(order[which(!(order %in% levels(x)))], collapse=" ")
          stop(paste(str,"is not a level of the explanatory variable.",sep = " "), call. = FALSE)
        }
        if ((sum(levels(x) == order) == 0) == TRUE) {
          x = relevel(x, ref = levels(as.factor(x))[2])
        }  
        if ((sum(levels(x) == order) == 0) == FALSE) {
          x = x
        }		
      }
      
      # calculate sample sizes
      n1 = sum(x==levels(as.factor(x))[1])
      n2 = sum(x==levels(as.factor(x))[2])
      
      # summary statistics and eda plots
      cat("Summary statistics:\n")
      if (est == "mean") {
        for(i in 1:x_levels) {
          cat("n_", names(by(y, x, length))[i], " = ", by(y, x, length)[i], ", ", sep="")
          cat("mean_", names(by(y, x, mean))[i], " = ", round(by(y, x, mean)[i],4), ", ", sep="")
          cat("sd_", names(by(y, x, sd))[i], " = ", round(by(y, x, sd)[i],4), "\n", sep="")
        }
        if(eda_plot == TRUE){boxplot(y ~ x, xlab = x_name, ylab = y_name, main = "", col = COL[3,4])}
      }
      if (est == "median") {
        for(i in 1:x_levels) {
          cat("n_", names(by(y, x, length))[i], " = ", by(y, x, length)[i], ", ", sep="")
          cat("median_", names(by(y, x, median))[i], " = ", round(by(y, x, median)[i],4), ", ", sep="")
        }
        if(eda_plot == TRUE){boxplot(y ~ x, xlab = x_name, ylab = y_name, main = "", col = COL[3,4])}
      }   
      if (est == "proportion") {
        y_table = table(y, x)
        print(addmargins(y_table))
        if(eda_plot == TRUE){mosaicplot(table(x, y), xlab = x_name, ylab = y_name, main = "", col = COL[3,4])}
      }
      
      # set statistic: difference between means, medians, or proportions
      if (y_type == "numerical") {
        statistic <- function(y, x) {	
          if (est == "mean") {
            stat = mean(y[x == levels(as.factor(x))[1]]) - mean(y[x == levels(as.factor(x))[2]])
          }
          if (est == "median") {
            stat = median(y[x == levels(as.factor(x))[1]]) - median(y[x == levels(as.factor(x))[2]])	
          }
          return(stat)
        }
      }
      if (y_type == "categorical") {
        statistic <- function(y, x) {	
          sum(y == success & x == levels(as.factor(x))[1])/n1 - sum(y == success & x == levels(as.factor(x))[2])/n2 
        }
      }
      
      # calculate and print actual
      actual = statistic(y, x)
      cat("Observed difference between ", est, "s (", levels(x)[1], "-", levels(x)[2] ,") = ", round(actual,4), "\n\n", sep = "")
      
      # save label
      label = paste("Difference in sample ", est, "s", ", ", levels(as.factor(x))[1],"-",levels(as.factor(x))[2], sep = "")
      
      # simulation
      if (method == "simulation") {
        n = length(y)
        sim = matrix(NA, nrow = n, ncol = nsim)
        
        # bootstrap ci
        if (type == "ci") {
          
          if (y_type == "numerical") {statistic = match.fun(est)}
          if (y_type == "categorical") {
            statistic = function(x) {
              which_success = which(levels(y) == success)
              sum(x == which_success)/length(x)
            }
          }
          
          sim1 = matrix(NA, nrow = n1, ncol = nsim)
          sim2 = matrix(NA, nrow = n2, ncol = nsim)
          
          for(i in 1:nsim) {sim1[,i] = sample(y[x == order[1]], n1, replace = TRUE)}
          for(i in 1:nsim) {sim2[,i] = sample(y[x == order[2]], n2, replace = TRUE)}
          
          sim_dist1 = apply(sim1, 2, statistic)
          sim_dist2 = apply(sim2, 2, statistic)
          
          sim_dist = sim_dist1 - sim_dist2
          
          ci = quantile(sim_dist, c( (1 - conflevel)/2 , ((1 - conflevel)/2)+conflevel ))
          
          counts = table(sim_dist)
          
          if(inf_plot == TRUE){
            #if(y_type == "categorical"){xlim = c(min(sim_dist)-sd(sim_dist), max(sim_dist)+sd(sim_dist))}
            #if(y_type == "numerical"){xlim = c(min(sim_dist)*0.8, max(sim_dist)*1.2)}
            xlim = c(min(sim_dist)-0.8*sd(sim_dist), max(sim_dist)+0.8*sd(sim_dist))
            
            if (nsim > 500) {
              counts = hist(sim_dist, plot = FALSE)$counts  
              hist(sim_dist, xlab = "Bootstrap distribution", main = "", ylab = "", xlim = xlim, col = COL[1,2])
              if (inf_lines == TRUE) {
                for (i in 1:2) {
                  segments(ci[i], 0, ci[i], 0.8 * max(counts), col=COL[4], lwd=2)
                  text(round(ci[i],4), max(counts), pos=1, col=COL[4], round(ci[i],4))
                }
              }
            }
            if (nsim <= 500) {
              BHH2::dotPlot(sim_dist, xlim = xlim, pch=19, col=COL[1,2], cex = 0.8, axes = FALSE, xlab="Bootstrap distribution")
              axis(1)
              if(inf_lines == TRUE){
                for (i in 1:2) {
                  segments(ci[i], 0, ci[i], 0.6, col=COL[4], lwd=2)
                  text(round(ci[i],4), 0.7, pos=1, col=COL[4], round(ci[i],4))
                }
              }
            }            
          }
          
          
          cat(c(conflevel*100, "% Bootstrap interval = (", round(ci[1],4), ",", round(ci[2],4), ")\n"))
        }
        
        # randomization test			
        if (type == "ht") {
          # hypotheses
          if (est == "mean") {
            mu1 = paste("mu_", order[1], sep = "")		
            mu2 = paste("mu_", order[2], sep = "")		
            cat(paste("H0:", mu1 , "-", mu2, "=", null, "\n"))
            cat(paste("HA:", mu1 , "-", mu2, sign, null, "\n"))
          }
          
          if (est == "median") {
            med1 = paste("median_", order[1], sep = "")		
            med2 = paste("median_", order[2], sep = "")		
            cat(paste("H0:", med1 , "-", med2, "=", null, "\n"))
            cat(paste("HA:", med1 , "-", med2, sign, null, "\n"))
          }
          
          if (est == "proportion") {
            p1 = paste("p_", order[1], sep = "")		
            p2 = paste("p_", order[2], sep = "")		
            cat(paste("H0:", p1 , "-", p2, "=", null, "\n"))
            cat(paste("HA:", p1 , "-", p2, sign, null, "\n"))
          }
          
          for(i in 1:nsim) {sim[,i] = sample(x, n, replace = FALSE)}
          sim_dist = apply(sim, 2, statistic, y = y)
          
          smaller_tail = round(min(c(mean(sim_dist <= actual), mean(sim_dist >= actual))), 4)
          
          xmin = min(c(-1.1*abs(actual), sim_dist))
          xmax = max(c(1.1*actual, sim_dist))
          
          if (inf_plot == TRUE){	
            if (nsim > 500) {
              counts = hist(sim_dist, plot = FALSE)$counts  
              hist(sim_dist, main = "Randomization distribution", xlab = "", ylim = c(0, 1.3 * max(counts)), xlim = c(xmin,xmax), cex.main = 0.75)  
            }
            if (nsim <= 500) {
              if (y_type == "numerical") {
                counts = BHH2::dotPlot(sim_dist, main = "Randomization distribution", xlab = "", pch = 20, cex = 0.8, xlim = c(xmin,xmax), cex.main = 0.75)$y							
              }					
              if (y_type == "categorical") {
                counts = table(sim_dist)
                plot(sim_dist, type = "n", ylim = c(0,max(counts)*1.3), axes = FALSE, xlim = c(0.9*min(sim_dist),1.1*max(sim_dist)),  main = "Randomization distribution", xlab = "", ylab = "", cex.main = 0.75)
                axis(1)
                axis(2)
                for(i in 1:length(sim_dist)) {
                  x   <- sim_dist[i]
                  rec <- sum(sim_dist == x)
                  points(rep(x, rec), 1:rec, pch=20, cex=0.8)					
                }							
              }
            }
          }
          
          #alternative = match.arg(alternative)
          
          if (alternative == "less") {
            if (actual < null) {cat(paste("p-value = ", smaller_tail,"\n"))}				
            if (actual > null) {cat(paste("p-value = ", 1 - smaller_tail,"\n"))}
            if (inf_lines == TRUE) {lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col="#569BBD", lwd=2)}
          }
          if (alternative == "greater") {
            if (actual < null) {cat(paste("p-value = ", 1 - smaller_tail,"\n"))}				
            if (actual > null) {cat(paste("p-value = ", smaller_tail,"\n"))}
            if (inf_lines == TRUE) {lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col="#569BBD", lwd=2)}
          }
          if (alternative == "twosided") {
            cat(paste("p-value = ", smaller_tail * 2,"\n"))
            if (inf_lines == TRUE) {lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col="#569BBD", lwd=2)}
            if (actual >= null) {
              temp = actual - null
              if (inf_lines == TRUE) {lines(x = c(null - temp,null - temp), y = c(0,1.1*max(counts)), col = COL[1,2], lwd=2)}						
            }
            if (actual < null) {
              temp = null - actual
              if (inf_lines == TRUE) {lines(x = c(null + temp,null + temp), y = c(0,1.1*max(counts)), col = COL[1,2], lwd=2)}						
            }		
          }
          
          if (inf_lines == TRUE) {text(x = actual, y = 1.25*max(counts), paste("observed\n", round(actual,4)), col = COL[1,2], cex = 0.8)}								
        }
        
      }
      
      # theoretical
      if (method == "theoretical") {
        
        # confidence interval
        if (type == "ci") {
          if (y_type == "numerical") {
            if (est == "median") {stop("Use simulation methods for inference for the median.", call. = FALSE)
            }
            if (est == "mean") {
              # calculate se and critvalue
              s1 = sd(y[x == levels(x)[1]])
              s2 = sd(y[x == levels(x)[2]])
              se = sqrt(s1^2/n1 + s2^2/n2)
              cat("Standard error =", round(se,4), "\n")
              if (n1 >= 30 & n2 >= 30) {critvalue = qnorm( (1 - conflevel)/2 + conflevel )}
              if (n1 < 30 | n2 < 30) {critvalue = qt( (1 - conflevel)/2 + conflevel , df = min(n1 - 1, n2 - 1))}						
            }
            
          }
          
          if (y_type == "categorical") {
            # check conditions
            cat("Check conditions:\n")
            suc1 = sum(y[x == levels(x)[1]] == success)
            fail1 = sum(y[x == levels(x)[1]] != success)
            cat(paste("  ", levels(x)[1], ": number of successes =", round(suc1), ";", "number of failures =", round(fail1)), "\n")	
            
            suc2 = sum(y[x == levels(x)[2]] == success)
            fail2 = sum(y[x == levels(x)[2]] != success)
            cat(paste("  ", levels(x)[2], ": number of successes =", round(suc2), ";", "number of failures =", round(fail2)), "\n")
            
            if (suc1 < 10 | fail1 < 10 | suc2 < 10 | fail2 < 10) {
              stop("There aren't at least 10 successes and 10 failures, use simulation methods instead.", call. = FALSE)
            }
            # calculate se and critvalue
            p1 = suc1 / n1
            p2 = suc2 / n2
            se = sqrt(p1 * (1-p1)/n1 + p2 * (1-p2)/n2)
            cat("Standard error =", round(se,4), "\n")
            critvalue = qnorm( (1 - conflevel)/2 + conflevel )					
            
          }
          
          # calculate ci
          me = critvalue * se
          ci = c(actual - me , actual + me)
          cat(c(conflevel*100, "% Confidence interval = (", round(ci[1],4), ",", round(ci[2],4), ")\n"))
        }
        
        # hypothesis test
        if (type == "ht") {
          if (y_type == "numerical") {
            if (est == "median") {stop("Use simulation methods for inference for the median.", call. = FALSE)
            }
            if (est == "mean") {
              # hypotheses
              mu1 = paste("mu_", order[1], sep = "")		
              mu2 = paste("mu_", order[2], sep = "")		
              cat(paste("H0:", mu1 , "-", mu2, "=", null, "\n"))
              cat(paste("HA:", mu1 , "-", mu2, sign, null, "\n"))
              # calculate test statistic and p-value component
              s1 = sd(y[x == levels(x)[1]])
              s2 = sd(y[x == levels(x)[2]])
              se = sqrt(s1^2/n1 + s2^2/n2)
              cat("Standard error =", round(se,3), "\n")
              teststat = (actual - null)/se
              if (n1 >= 30 & n2 >= 30) {
                cat(paste("Test statistic: Z = ", round(teststat, 3),"\n"))
                smaller_tail = round(min(pnorm(teststat), pnorm(teststat, lower.tail = FALSE)), 4)
              }
              if (n1 < 30 | n2 < 30) {
                cat(paste("Test statistic: T = ", round(teststat, 3),"\n"))
                cat(paste("Degrees of freedom: ", min(n1 - 1, n2 - 1), "\n"))
                smaller_tail = round(min(pt(teststat, df = min(n1 - 1, n2 - 1)), pt(teststat, df = min(n1 - 1, n2 - 1), lower.tail = FALSE)), 4)
              }						
            }	
          }
          if (y_type == "categorical") {
            if (null <= -1 | null >= 1) {
              stop("Null value should be a proportion between 0 and 1.", call. = FALSE)
            }
            # hypotheses
            p1 = paste("p_", order[1], sep = "")		
            p2 = paste("p_", order[2], sep = "")		
            cat(paste("H0:", p1 , "-", p2, "=", null, "\n"))
            cat(paste("HA:", p1 , "-", p2, sign, null, "\n"))
            
            # calculate p_pool
            suc1 = sum(y[x == levels(x)[1]] == success)
            fail1 = sum(y[x == levels(x)[1]] != success)
            suc2 = sum(y[x == levels(x)[2]] == success)
            fail2 = sum(y[x == levels(x)[2]] != success)
            p_pool =  (suc1 + suc2)/(n1 + n2)
            cat(paste("Pooled proportion =", round(p_pool, 4), "\n"))	
            
            # check conditions
            cat("Check conditions:\n")
            exp_suc1 = n1 * p_pool
            exp_fail1 = n1 * (1 - p_pool)
            cat(paste("  ", levels(x)[1], ": number of expected successes =", round(exp_suc1), ";", "number of expected failures =", round(exp_fail1)), "\n")
            exp_suc2 = n2 * p_pool
            exp_fail2 = n2 * (1 - p_pool)
            cat(paste("  ", levels(x)[2], ": number of expected successes =", round(exp_suc2), ";", "number of expected failures =", round(exp_fail2)), "\n")
            if (exp_suc1 < 10 | exp_fail1 < 10 | exp_suc2 < 10 | exp_fail2 < 10) {
              stop("There aren't at least 10 expected successes and 10 expected failures, use simulation methods instead.", call. = FALSE)
            }
            # calculate test statistic and p-value
            se = sqrt( p_pool * (1 - p_pool) / n1 + p_pool * (1 - p_pool) / n2 )
            cat("Standard error =", round(se,3), "\n")
            teststat = (actual - null) / se
            cat(paste("Test statistic: Z = ", round(teststat, 3),"\n"))
            smaller_tail = round(min(pnorm(teststat), pnorm(teststat, lower.tail = FALSE)), 4)								
          }
          # alternative = less
          if (alternative == "less") {
            if (actual < null) {cat(paste("p-value = ", smaller_tail,"\n"))}				
            if (actual > null) {cat(paste("p-value = ", 1 - smaller_tail,"\n"))}
            if (inf_plot == TRUE){
              normTail(L = teststat, axes = FALSE, col = COL[1,2])
              axis(1, at = c(-3, teststat, 0, 3), labels = c(NA, paste(round(actual,2)), paste(null), NA))
            }
          }
          # alternative = greater
          if (alternative == "greater") {
            if (actual < null) {cat(paste("p-value = ", 1 - smaller_tail,"\n"))}				
            if (actual > null) {cat(paste("p-value = ", smaller_tail,"\n"))}
            if (inf_plot == TRUE){
              normTail(U = teststat, axes = FALSE, col = COL[1,2])
              axis(1, at = c(-3, 0, teststat, 3), labels = c(NA, paste(null), paste(round(actual,2)), NA))
            }
          }				
          # alternative = twosided	
          if (alternative == "twosided") {
            cat(paste("p-value = ", smaller_tail * 2,"\n"))
            if (inf_plot == TRUE){
              if (actual < null) {
                normTail(L = teststat, U = -1*teststat, axes = FALSE, col = COL[1,2])
                axis(1, at = c(-3, teststat, 0, -1*teststat, 3), labels = c(NA, paste(round(actual,2)), paste(null), paste(round(null + (null - actual), 2)), NA))
              }
              if (actual > null) {
                normTail(L = -1*teststat, U = teststat, axes = FALSE, col = COL[1,2])
                axis(1, at = c(-3, -1*teststat, 0, teststat, 3), labels = c(NA, paste(round(null - (actual - null), 2)), paste(null), paste(round(actual,2)), NA))
              }
            }
          }
        }
        
        
      }
      
    } 
    
    # x variable with >2 levels, ANOVA
    if (x_levels > 2 & y_type == "numerical") {
      
      # summary statistics
      cat("Summary statistics:\n")
      for(i in 1:x_levels) {
        cat("n_", names(by(y, x, length))[i], " = ", by(y, x, length)[i], ", ", sep="")
        cat("mean_", names(by(y, x, mean))[i], " = ", round(by(y, x, mean)[i],4), ", ", sep="")
        cat("sd_", names(by(y, x, sd))[i], " = ", round(by(y, x, sd)[i],4), "\n", sep="")
      }
      cat("\n")
      if(eda_plot == TRUE){boxplot(y ~ x, xlab = x_name, ylab = y_name, main = "", col = COL[3,4])}
      
      # hypotheses
      cat("H_0: All means are equal.\n")
      cat("H_A: At least one mean is different.\n")
      
      if(method == "theoretical"){        
        # anova output
        anova_output = anova(lm(y~x))
        print(anova_output, signif.stars=F)
        
        # post-hoc pairwise tests, if ANOVA is significant
        if (anova_output$"Pr(>F)"[1] < siglevel) {
          cat("\nPairwise tests: ")
          pairwise = pairwise.t.test(y, x, p.adj = "none")
          cat(pairwise$method, "\n")
          print(round(pairwise$p.value,4))
        }      
        
        # plot
        F = anova_output[["F value"]][1]
        df_n = anova_output$Df[1]
        df_d = anova_output$Df[2]
        if(inf_plot == TRUE) {FTail(F,df_n,df_d, col = COL[1])}
      }
      
      if(method == "simulation"){
        # anova output
        anova_output = anova(lmp(y~x))
        print(anova_output, signif.stars=F)
        
      }
    }
    
    # x variable with >2 levels, chi-square
    if ((x_levels > 2 | y_levels > 2) & y_type == "categorical") {
      
      # summary statistics
      cat("\nSummary statistics:\n")
      y_table = table(y, x)
      print(addmargins(y_table))
      mosaicplot(table(x, y), main = "", col = COL[3,4])
      cat("\n")
      
      # hypotheses
      cat("H_0: Response and explanatory variable are independent.\n")
      cat("H_A: Response and explanatory variable are dependent.\n")
      
      if (method == "theoretical") {
        chisquare_output = chisq.test(y_table, correct = FALSE)
        
        # check conditions
        cat("Check conditions: expected counts\n")
        expected = round(chisquare_output$expected, 2)
        print(expected)
        if (any(expected < 5)) {
          stop("Expected count for at least one cell is less than 5, use method = 'simulation'.", call. = FALSE)
        }
        
        # chi-square output
        print(chisquare_output)
        
        # plot
        chi = chisquare_output$statistic
        df = chisquare_output$parameter
        if (inf_plot == TRUE){chiTail(chi,df)}       
      }
      
      if(method == "simulation"){
        # chi-square output
        chisquare_output = chisq.test(y_table, correct = FALSE, simulate.p.value == TRUE)
        print(chisquare_output)
      }
      
    }
    
  }
  
  # return simdist
  if (simdist == TRUE) {return(sim_dist)}
  
  # reset plotting window
  par(mfrow = c(1,1), mar = c(5,4,4,2)+0.1)
}