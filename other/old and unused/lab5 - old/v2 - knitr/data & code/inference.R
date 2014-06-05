# inference
inference <- function(data, group = NULL, est = c("mean", "median", "proportion"), success = NULL, order = NULL, nsim = 10000, conflevel = 0.95, null = NULL, alternative = c("less","greater","twosided"), type = c("ci","ht"), method = c("theoretical","simulation"), drawlines = "yes", simdist = FALSE){
	# data: variable 1, can be quantitative or categorical
	# group: variable 2, categorical (optional)
	# est: parameter to estimate, mean, median, or mode
	# success: which level of the categorical variable to call "success", i.e. do inference on
	# order: when group is given, order of groups in which to subtract parameters
	# nsim: number of simulations
	# level: confidence level, value between 0 and 1
	
	# load openintro package
	if(!("openintro" %in% names(installed.packages()[,"Package"]))){install.packages("openintro")}
	if(!("BHH2" %in% names(installed.packages()[,"Package"]))){install.packages("BHH2")}
	library(openintro)
	library(BHH2)
		
	# possible error: weird data
	if(length(data) == 1){stop("Sample size is only 1, check input data.")}
	
	# possible error: missing arguments
	if(length(est) > 1){stop("Missing estimate: mean, median, or proportion")}
	if(length(type) > 1){stop("Missing type: confidence interval or hypothesis test")}
	if(length(method) > 1){stop("Missing method: theoretical or simulation")}
	
	if(type == "ht" & is.null(null)){stop("Missing null value")}
	if(type == "ht" & length(alternative) > 1){stop("Missing alternative: less, greater, or twosided")}
	
	# possible error: method isn't theoretical or simulation
	if(method %in% c("theoretical", "simulation") == FALSE){
		stop("Method should be theoretical or simulation, check spelling and spaces.")
	}
	
	# possible error: type isn't ci or ht
	if(type %in% c("ci", "ht") == FALSE){
		stop("Type should be ci or ht, check spelling and spaces.")
	}
	
	# possible error: estimate isn't mean, median, or proportion
	if(est %in% c("mean", "median", "proportion") == FALSE){
		stop("Estimate should be mean, median, or proportion, check spelling and spaces.")
	}
	
	# possible error: variables not of same length
	if (!is.null(group)){
		if (length(data) != length(group)) {stop("The two variables must be of same length.")}
	}
	
	# possible error: confidence level greater than 1
	if(conflevel > 1){conflevel = conflevel / 100}
		
	# possible error: drop NAs, and if group is given, use pairwise complete
	if (is.null(group)) {
    	if (sum(is.na(data)) > 0) {data = data[!is.na(data)]}
  	}
	if (!is.null(group)) {
    	if (sum(is.na(data)) > 0 | sum(is.na(group)) > 0) {
      		data.temp = data[!is.na(data) & !is.na(group)]
      		group.temp = group[!is.na(data) & !is.na(group)]
      		data = data.temp
      		group = group.temp
    	}
  	}

	# if data or group is character, make factor
	if(is.character(data)){data = as.factor(data)}
	if(is.character(group)){group = as.factor(group)}

	# set variable type for data
	data_type = "categorical"
  	if (is.numeric(data)) {data_type = "quantitative"}
	
	# print variable types
	if (!is.null(group)) {
    	group_type = "categorical"
    	if(is.numeric(group)){
			group_type = "quantitative"
    		cat(paste("Variable 1:", data_type, ", Variable 2:", group_type, "\n"))
			}
  	}
	if (is.null(group)) {
    	group_type = "only1var"
    	cat(paste("One", data_type, "variable", "\n"))
  	}

	# the quantitative variable should be data if one categorical and one quantitative
  	if (data_type == "categorical" & group_type == "quantitative") {
    	data.temp = group
    	group.temp = data
    	data = data.temp
    	group = group.temp
    	data_type = "quantitative"
    	group_type = "categorical"
  	}

	# possible error: wrong estimate
	if(data_type == "quantitative" & est == "proportion"){
		stop("Variable is quantitative, sample statistic cannot be a proportion, choose either mean or median")
	}	
	if(data_type == "categorical" & est == "mean"){
		stop("Variable is categorical, sample statistic cannot be a mean or a median, use proportion")
	}
	if(data_type == "categorical" & est == "median"){
		stop("Variable is categorical, sample statistic cannot be a mean or a median, use proportion")
	}
	
	
	# possible error: group variable has more than two levels
	if(group_type == "categorical" & length(levels(group)) > 2){
		stop("Grouping variable (group) can only have 2 levels.")
	} 
	
	# possible errors about success
	if(data_type == "categorical"){
		# success not provided for categorical variable
		if(is.null(success)){
			stop("Variable is categorical, specify which level to call success.")
		}
		# possible error: success provided is not a level of the categorical variable
		if(success %in% levels(data) == FALSE){
			stop(paste(success,"is not a level of the success variable."))
		}
	}

	# possible warning: success provided for quantitative variable
	if(data_type == "quantitative" & !is.null(success)){
		warning("`success' is ignored for quantitative variable.\n")
	}
	
	# define sample size
	n = length(data)
	
	# define sign of hypothesis test
	if(type == "ht"){
		if(alternative == "less"){sign = "<"}
		if(alternative == "greater"){sign = ">"}
		if(alternative == "twosided"){sign = "!="}		
	}

	# one variable
	if(group_type == "only1var"){
		cat("Single", est, "\n")
		# set statistic: mean, median, or proportion
		if(data_type == "quantitative"){statistic = match.fun(est)}
		if(data_type == "categorical"){statistic = function(x){sum(x == success)/length(x)}}
	
		actual = statistic(data)
		cat(paste("Observed", est, "=", round(actual, 4)), "\n")
		
		# simulation
		if(method == "simulation"){
			sim = matrix(NA, nrow = n, ncol = nsim)
			
			# bootstrap ci
			if(type == "ci"){
				cat("Bootstrapping, please wait...", "\n")
				for(i in 1:nsim){sim[,i] = sample(data, n, replace = TRUE)}
				if(data_type == "categorical"){
					statistic = function(x){
						which_success = which(levels(data) == success)
						sum(x == which_success)/length(x)
						}
					}				
				sim_dist = apply(sim, 2, statistic)

				ci = quantile(sim_dist, c( (1 - conflevel)/2 , ((1 - conflevel)/2)+conflevel ))
				
				if(nsim > 500){
					counts = hist(sim_dist, plot = FALSE)$counts
					hist(sim_dist, main = "Bootstrap distribution", xlab = "bootstrap statistic")  
				}
				if(nsim <= 500){
					counts = table(sim_dist)
					plot(sim_dist, type = "n", ylim = c(0,max(counts)), axes = FALSE, xlim = c(0.9*min(sim_dist),1.1*max(sim_dist)), main = "Bootstrap distribution", xlab = "bootstrap statistic", ylab = "")
					axis(1)
					axis(2)
					for(i in 1:length(sim_dist)){
						x   <- sim_dist[i]
						rec <- sum(sim_dist == x)
						points(rep(x, rec), 1:rec, pch=20, cex=0.8)
					}
				}
				
				for (i in 1:2) {
			    	if(drawlines == "yes"){
						segments(ci[i], 0, ci[i], 0.8 * max(counts), col="#225588", lwd=2)
			    		text(round(ci[i],2), max(counts), pos=1, col="#225588", round(ci[i],2))
					}
			  	}

				cat(c(conflevel*100, "% Bootstrap interval = (", round(ci[1],2), ",", round(ci[2],2), ")\n"))		
			}
			
			# randomization test
			if(type == "ht"){
				cat("Randomizing, please wait...", "\n")
				if(data_type == "quantitative"){
					for(i in 1:nsim){sim[,i] = sample(data, n, replace = TRUE)}
					sim_dist_temp = apply(sim, 2, statistic)
					if(est == "mean"){
						# hypotheses
						cat(paste("H0: mu =", null, "\n"))
						cat(paste("HA: mu", sign, null, "\n"))
						sim_dist = sim_dist_temp - (mean(sim_dist_temp) - null)
						#cat(mean(sim_dist))
					}
										
					if(est == "median"){
						cat(paste("H0: median =", null, "\n"))
						cat(paste("HA: median", sign, null, "\n"))
						sim_dist = sim_dist_temp - (median(sim_dist_temp) - null)
						#cat(median(sim_dist))
					}					
				}
				if(data_type == "categorical"){
					cat(paste("H0: p =", null, "\n"))
					cat(paste("HA: p", sign, null, "\n"))
					sim_dist = rbinom(nsim, n, prob = null) / n
				}
				
				smaller.tail = round(min(c(mean(sim_dist <= actual), mean(sim_dist >= actual))), 4)	
								
				if(nsim > 500){
					counts = hist(sim_dist, plot = FALSE)$counts  
					hist(sim_dist, main = "Randomization distribution", xlab = "randomization statistic", ylim = c(0, 1.3 * max(counts)))  
				}
				if(nsim <= 500){
					counts = table(sim_dist)
					plot(sim_dist, type = "n", ylim = c(0,max(counts)), axes = FALSE, xlim = c(0.9*min(sim_dist),1.1*max(sim_dist)), main = "Randomization distribution", xlab = "randomization statistic", ylab = "")
					axis(1)
					axis(2)
					for(i in 1:length(sim_dist)){
						x   <- sim_dist[i]
						rec <- sum(sim_dist == x)
						points(rep(x, rec), 1:rec, pch=20, cex=0.8)					
					}				
				}
				
			    alternative = match.arg(alternative)

				if(alternative == "less"){
					if(actual < null){cat(paste("p-value: ", smaller.tail,"\n"))}				
					if(actual > null){cat(paste("p-value: ", 1 - smaller.tail,"\n"))}
					if(drawlines == "yes"){lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col="#225588", lwd=2)}
				}
				if(alternative == "greater"){
					if(actual < null){cat(paste("p-value: ", 1 - smaller.tail,"\n"))}				
					if(actual > null){cat(paste("p-value: ", smaller.tail,"\n"))}
					if(drawlines == "yes"){lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col="#225588", lwd=2)}
				}
				if(alternative == "twosided"){
					cat(paste("p-value: ", smaller.tail * 2,"\n"))
					lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col="#225588", lwd=2)
					if(actual >= null){
						temp = actual - null
						if(drawlines == "yes"){lines(x = c(null - temp,null - temp), y = c(0,1.1*max(counts)), col = "#225588", lwd=2)}						
					}
					if(actual < null){
						temp = null - actual
						if(drawlines == "yes"){lines(x = c(null + temp,null + temp), y = c(0,1.1*max(counts)), col = "#225588", lwd=2)}						
					}		
				}
			  	if(drawlines == "yes"){text(x = actual, y = 1.2*max(counts), paste("observed\n", round(actual,4)), col = "#225588", cex = 0.8)}								
			}		
		}
		
		# theoretical
		if(method == "theoretical"){
			
			# confidence interval
			if(type == "ci"){
				if(data_type == "quantitative"){
					if(est == "median"){stop("Use simulation methods for inference for the median.")}
					if(est == "mean"){
						# check conditions
						hist(data, main = "Histogram of sample data", xlab = "sample data")	
						#cat("Check histogram to verify that sample data do not come from an extremely skewed distribution.\n")
						# calculate me and se
						se = sd(data) / sqrt(n)
						cat(paste("Standard error =", round(se, 4), "\n"))
						if(n >= 50) {critvalue = qnorm( (1 - conflevel)/2 + conflevel )}
						if(n < 50) {critvalue = qt( (1 - conflevel)/2 + conflevel , df = n - 1)}					
					}
				}
				if(data_type == "categorical"){
					# check conditions
					suc = round(n * actual, 2)
					fail = round(n * (1 - actual), 2)
					cat(paste("Number of successes =", round(suc), ";", "Number of failures =", round(fail)), "\n")	
					if(suc < 10 | fail < 10){
						stop("There aren't at least 10 successes and 10 failures, use simulation methods instead.")
					}
					# calculate me and se
					se = sqrt(actual * (1-actual) / n)
					cat(paste("Standard error =", round(se, 4), "\n"))
					critvalue = qnorm( (1 - conflevel)/2 + conflevel )					
				}
				me = critvalue * se
				ci = c(actual - me , actual + me)
				cat(c(conflevel*100, "% Confidence interval = (", round(ci[1],2), ",", round(ci[2],2), ")\n"))	
			}
			
			# hypothesis test
			if(type == "ht"){
				if(data_type == "quantitative"){
					if(est == "median"){stop("Use simulation methods for inference for the median.")}
					if(est == "mean"){
						# hypotheses
						cat(paste("H0: mu =", null, "\n"))
						cat(paste("HA: mu", sign, null, "\n"))
						
						# check conditions
						par(mfrow = c(1,2))
						hist(data, main = "Histogram of sample data", xlab = "sample data")	
						#cat("Check histogram to verify that sample data do not come from an extremely skewed distribution.\n")
						# calculate test statistic and p-value component
						se = sd(data) / sqrt(n)
						cat("Standard error =", round(se,4), "\n")
						teststat = (actual - null)/se
						if(n >= 50){
							cat(paste("Test statistic: Z = ", round(teststat, 3),"\n"))
							smaller.tail = round(min(pnorm(teststat), pnorm(teststat, lower.tail = FALSE)), 4)
							}
						if(n < 50){
							cat(paste("Test statistic: T = ", round(teststat, 3),"\n"))
							cat(paste("Degrees of freedom: ", n - 1, "\n"))
							smaller.tail = round(min(pt(teststat, df = n - 1), pt(teststat, df = n - 1, lower.tail = FALSE)), 4)
							}		
					}
				}
				if(data_type == "categorical"){
					if(null < 0 | null > 1){
						stop("Null value should be a proportion between 0 and 1.")
					}
					# hypotheses
					cat(paste("H0: p =", null, "\n"))
					cat(paste("HA: p", sign, null, "\n"))
					
					
					# check conditions
					exp_suc = round(n * null, 2)
					exp_fail = round(n * (1 - null), 2)
					cat(paste("Number of expected successes =", round(exp_suc), ";", "Number of expected failures =", round(exp_fail)), "\n")
					if(exp_suc < 10 | exp_fail < 10){
						stop("There aren't at least 10 expected successes and 10 expected failures, use simulation methods instead.")
					}
					# calculate test statistic and p-value
					se = sqrt(null * (1 - null) / n)
					cat("Standard error =", round(se,4), "\n")
					teststat = (actual - null)/se
					cat(paste("Test statistic: Z = ", round(teststat, 3),"\n"))
					smaller.tail = round(min(pnorm(teststat), pnorm(teststat, lower.tail = FALSE)), 4)					
				}

				# alternative = less
				if(alternative == "less"){
					if(actual < null){cat(paste("p-value: ", smaller.tail,"\n"))}				
					if(actual > null){cat(paste("p-value: ", 1 - smaller.tail,"\n"))}
					normTail(L = teststat, axes = FALSE, col = "#22558833")
					axis(1, at = c(-3, teststat, 0, 3), labels = c(NA, paste(round(actual,2)), paste(null), NA))
				}

				# alternative = greater
				if(alternative == "greater"){
					if(actual < null){cat(paste("p-value: ", 1 - smaller.tail,"\n"))}				
					if(actual > null){cat(paste("p-value: ", smaller.tail,"\n"))}
					normTail(U = teststat, axes = FALSE, col = "#22558833")
					axis(1, at = c(-3, 0, teststat, 3), labels = c(NA, paste(null), paste(round(actual,2)), NA))
				}
				
				# alternative = twosided	
				if(alternative == "twosided"){
					cat(paste("p-value: ", smaller.tail * 2,"\n"))
					if(actual < null){
						normTail(L = teststat, U = -1*teststat, axes = FALSE, col = "#22558833")
						axis(1, at = c(-3, teststat, 0, -1*teststat, 3), labels = c(NA, paste(round(actual,2)), paste(null), paste(round(null + (null - actual), 2)), NA))
					}
					if(actual > null){
						normTail(L = -1*teststat, U = teststat, axes = FALSE, col = "#22558833")
						axis(1, at = c(-3, -1*teststat, 0, teststat, 3), labels = c(NA, paste(round(null - (actual - null), 2)), paste(null), paste(round(actual,2)), NA))
					}				
				}
				par(mfrow = c(1,1))
			}
		}
	}	
	
	# two variables
	if(group_type == "categorical"){

		# order
		if(is.null(order)){order = levels(group)}
		if(length(order) == 1 & !is.na(order[1])){
			stop("`order' cannot be of length 1, list the order in which two levels of the grouping variable should be subtracted.")
		}
		if(length(order) == 2){
			if( all(order %in% levels(group)) == FALSE){
				str = paste(order[which(!(order %in% levels(group)))], collapse=" ")
				stop(paste(str,"is not a level of the grouping variable.",sep = " "))
			}
			if((sum(levels(group) == order) == 0) == TRUE){
				group = relevel(group, ref = levels(as.factor(group))[2])
			}	
			if((sum(levels(group) == order) == 0) == FALSE){
				group = group
			}		
		}

		# print what's going on
		if(data_type == "quantitative"){
			cat("One quantitative and one categorical variable\n")
		}
		if(data_type == "categorical"){
			cat("Two categorical variables\n")
		}
		cat("Difference between two ", est, "s", "\n", sep = "")

		# calculate sample sizes
	  	n1 = sum(group==levels(as.factor(group))[1])
	  	n2 = sum(group==levels(as.factor(group))[2])
	
		# print sample sizes
		n1_print = paste("n_", order[1], sep = "")		
		n2_print = paste("n_", order[2], sep = "")		
		cat(paste(n1_print, "=", n1, ";", n2_print, "=", n2, "\n"))

		# set statistic: difference between means, medians, or proportions
		if(data_type == "quantitative"){
			statistic <- function(data, group){	
				if(est == "mean"){
					stat = mean(data[group == levels(as.factor(group))[1]]) - mean(data[group == levels(as.factor(group))[2]])					
				}
				if(est == "median"){
					stat = median(data[group == levels(as.factor(group))[1]]) - median(data[group == levels(as.factor(group))[2]])										
				}
				return(stat)
			}
		}
		if(data_type == "categorical"){
			statistic <- function(data, group){	
				sum(data == success & group == levels(as.factor(group))[1])/n1 - sum(data == success & group == levels(as.factor(group))[2])/n2 
			}
    	}

		# calculate and print actual
		actual = statistic(data, group)
		cat("Observed difference between ", est, "s = ", round(actual,4), "\n", sep = "")

		# save label
		label = paste("Difference in sample ", est, "s", ", ", levels(as.factor(group))[1],"-",levels(as.factor(group))[2], sep = "")
		
		# simulation
		if(method == "simulation"){
			n = length(data)
			sim = matrix(NA, nrow = n, ncol = nsim)
			
			# bootstrap ci
			if(type == "ci"){
				cat("Bootstrapping, please wait...", "\n")
				
				if(data_type == "quantitative"){statistic = match.fun(est)}
				if(data_type == "categorical"){
					statistic = function(x){
						which_success = which(levels(data) == success)
						sum(x == which_success)/length(x)
					}
				}
				
				sim1 = matrix(NA, nrow = n1, ncol = nsim)
				sim2 = matrix(NA, nrow = n2, ncol = nsim)

				for(i in 1:nsim){sim1[,i] = sample(data[group == order[1]], n1, replace = TRUE)}
				for(i in 1:nsim){sim2[,i] = sample(data[group == order[2]], n2, replace = TRUE)}
				
				sim_dist1 = apply(sim1, 2, statistic)
				sim_dist2 = apply(sim2, 2, statistic)
				
				sim_dist = sim_dist1 - sim_dist2
				
				ci = quantile(sim_dist, c( (1 - conflevel)/2 , ((1 - conflevel)/2)+conflevel ))
				
				counts = table(sim_dist)
				
				if(nsim > 500){
					counts = hist(sim_dist, plot = FALSE)$counts  
					hist(sim_dist, main = "Bootstrap distribution", xlab = "bootstrap statistic")  
				}
				if(nsim <= 500){						
						if(data_type == "quantitative"){
							counts = BHH2::dotPlot(sim_dist, main = "Bootstrap distribution", xlab = "bootstrap statistic", pch = 20, cex = 0.8)$y							
						}					
						if(data_type == "categorical"){
							counts = table(sim_dist)
							plot(sim_dist, type = "n", ylim = c(0,max(counts)), axes = FALSE, xlim = c(0.9*min(sim_dist),1.1*max(sim_dist)), main = "Bootstrap distribution", xlab = "bootstrap statistic", ylab = "")
							axis(1)
							axis(2)
							for(i in 1:length(sim_dist)){
								x   <- sim_dist[i]
								rec <- sum(sim_dist == x)
								points(rep(x, rec), 1:rec, pch=20, cex=0.8)					
							}							
						} 					
				}
				
				for (i in 1:2) {
			    	if(drawlines == "yes"){
						segments(ci[i], 0, ci[i], 0.8 * max(counts), col="#225588", lwd=2)
			    		text(round(ci[i],2), max(counts), pos=1, col="#225588", round(ci[i],2))
					}
			  	}

				cat(c(conflevel*100, "% Bootstrap interval = (", round(ci[1],2), ",", round(ci[2],2), ")\n"))
			}
			
			# randomization test			
			if(type == "ht"){
				# hypotheses
				if(est == "mean"){
					mu1 = paste("mu_", order[1], sep = "")		
					mu2 = paste("mu_", order[2], sep = "")		
					cat(paste("H0:", mu1 , "-", mu2, "=", null, "\n"))
					cat(paste("HA:", mu1 , "-", mu2, sign, null, "\n"))
				}
									
				if(est == "median"){
					med1 = paste("median_", order[1], sep = "")		
					med2 = paste("median_", order[2], sep = "")		
					cat(paste("H0:", med1 , "-", med2, "=", null, "\n"))
					cat(paste("HA:", med1 , "-", med2, sign, null, "\n"))
				}
				
				if(est == "proportion"){
					p1 = paste("p_", order[1], sep = "")		
					p2 = paste("p_", order[2], sep = "")		
					cat(paste("H0:", p1 , "-", p2, "=", null, "\n"))
					cat(paste("HA:", p1 , "-", p2, sign, null, "\n"))
				}
				
				cat("Randomizing, please wait...", "\n")
				
				for(i in 1:nsim){sim[,i] = sample(group, n, replace = FALSE)}
				sim_dist = apply(sim, 2, statistic, data = data)
				
				smaller.tail = round(min(c(mean(sim_dist <= actual), mean(sim_dist >= actual))), 4)
				
				xmin = min(c(-1.1*abs(actual), sim_dist))
			  	xmax = max(c(1.1*actual, sim_dist))
				
				if(nsim > 500){
					counts = hist(sim_dist, plot = FALSE)$counts  
					hist(sim_dist, main = "Randomization distribution", xlab = "randomization statistic", ylim = c(0, 1.3 * max(counts)), xlim = c(xmin,xmax))  
				}
				if(nsim <= 500){
					if(data_type == "quantitative"){
						counts = BHH2::dotPlot(sim_dist, main = "Randomization distribution", xlab = "randomization statistic", pch = 20, cex = 0.8, xlim = c(xmin,xmax))$y							
					}					
					if(data_type == "categorical"){
						counts = table(sim_dist)
						plot(sim_dist, type = "n", ylim = c(0,max(counts)), axes = FALSE, xlim = c(0.9*min(sim_dist),1.1*max(sim_dist)),  main = "Randomization distribution", xlab = "randomization statistic", ylab = "")
						axis(1)
						axis(2)
						for(i in 1:length(sim_dist)){
							x   <- sim_dist[i]
							rec <- sum(sim_dist == x)
							points(rep(x, rec), 1:rec, pch=20, cex=0.8)					
						}							
					}
				}
				
			    alternative = match.arg(alternative)

				if(alternative == "less"){
					if(actual < null){cat(paste("p-value: ", smaller.tail,"\n"))}				
					if(actual > null){cat(paste("p-value: ", 1 - smaller.tail,"\n"))}
					if(drawlines == "yes"){lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col="#225588", lwd=2)}
				}
				if(alternative == "greater"){
					if(actual < null){cat(paste("p-value: ", 1 - smaller.tail,"\n"))}				
					if(actual > null){cat(paste("p-value: ", smaller.tail,"\n"))}
					if(drawlines == "yes"){lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col="#225588", lwd=2)}
				}
				if(alternative == "twosided"){
					cat(paste("p-value: ", smaller.tail * 2,"\n"))
					if(drawlines == "yes"){lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col="#225588", lwd=2)}
					if(actual >= null){
						temp = actual - null
						if(drawlines == "yes"){lines(x = c(null - temp,null - temp), y = c(0,1.1*max(counts)), col = "#225588", lwd=2)}						
					}
					if(actual < null){
						temp = null - actual
						if(drawlines == "yes"){lines(x = c(null + temp,null + temp), y = c(0,1.1*max(counts)), col = "#225588", lwd=2)}						
					}		
				}
				
				if(drawlines == "yes"){text(x = actual, y = 1.25*max(counts), paste("observed\n", round(actual,4)), col = "#225588", cex = 0.8)}								
			}

		}
		
		# theoretical
		if(method == "theoretical"){
			
			# confidence interval
			if(type == "ci"){
				if(data_type == "quantitative"){
					if(est == "median"){stop("Use simulation methods for inference for the median.")
					}
					if(est == "mean"){
						# check conditions
						boxplot(data ~ group, main = "", xlab = "")
						#cat("Check boxplot to verify that sample data do not come from extremely skewed distributions.\n")
						# calculate se and critvalue
						s1 = sd(data[group == levels(group)[1]])
						s2 = sd(data[group == levels(group)[2]])
						se = sqrt(s1^2/n1 + s2^2/n2)
						cat("Standard error =", round(se,4), "\n")
						if(n1 >= 50 & n2 >= 50){critvalue = qnorm( (1 - conflevel)/2 + conflevel )}
						if(n1 < 50 | n2 < 50) {critvalue = qt( (1 - conflevel)/2 + conflevel , df = min(n1 - 1, n2 - 1))}						
					}
					
				}
			
				if(data_type == "categorical"){
					# check conditions
					suc1 = sum(data[group == levels(group)[1]] == success)
					fail1 = sum(data[group == levels(group)[1]] != success)
					cat(paste("Group 1: Number of successes =", round(suc1), ";", "Number of failures =", round(fail1)), "\n")	
					
					suc2 = sum(data[group == levels(group)[2]] == success)
					fail2 = sum(data[group == levels(group)[2]] != success)
					cat(paste("Group 2: Number of successes =", round(suc2), ";", "Number of failures =", round(fail2)), "\n")
					
					if(suc1 < 10 | fail1 < 10 | suc2 < 10 | fail2 < 10){
						stop("There aren't at least 10 successes and 10 failures, use simulation methods instead.")
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
				cat(c(conflevel*100, "% Confidence interval = (", round(ci[1],2), ",", round(ci[2],2), ")\n"))
			}
			
			# hypothesis test
			if(type == "ht"){
				if(data_type == "quantitative"){
					if(est == "median"){stop("Use simulation methods for inference for the median.")
					}
					if(est == "mean"){
						# hypotheses
						mu1 = paste("mu_", order[1], sep = "")		
						mu2 = paste("mu_", order[2], sep = "")		
						cat(paste("H0:", mu1 , "-", mu2, "=", null, "\n"))
						cat(paste("HA:", mu1 , "-", mu2, sign, null, "\n"))
						# check conditions
						par(mfrow = c(1,2))
						boxplot(data ~ group, main = "", xlab = "")
						#cat("Check boxplot to verify that sample data do not come from extremely skewed distributions.\n")
						# calculate test statistic and p-value component
						s1 = sd(data[group == levels(group)[1]])
						s2 = sd(data[group == levels(group)[2]])
						se = sqrt(s1^2/n1 + s2^2/n2)
						cat("Standard error =", round(se,3), "\n")
						teststat = (actual - null)/se
						if(n1 >= 50 & n2 >= 50){
							cat(paste("Test statistic: Z = ", round(teststat, 3),"\n"))
							smaller.tail = round(min(pnorm(teststat), pnorm(teststat, lower.tail = FALSE)), 4)
							}
						if(n1 < 50 | n2 < 50) {
							cat(paste("Test statistic: T = ", round(teststat, 3),"\n"))
							cat(paste("Degrees of freedom: ", min(n1 - 1, n2 - 1), "\n"))
							smaller.tail = round(min(pt(teststat, df = min(n1 - 1, n2 - 1)), pt(teststat, df = min(n1 - 1, n2 - 1), lower.tail = FALSE)), 4)
							}						
					}	
				}
				if(data_type == "categorical"){
					if(null <= -1 | null >= 1){
						stop("Null value should be a proportion between 0 and 1.")
					}
					# hypotheses
					p1 = paste("p_", order[1], sep = "")		
					p2 = paste("p_", order[2], sep = "")		
					cat(paste("H0:", p1 , "-", p2, "=", null, "\n"))
					cat(paste("HA:", p1 , "-", p2, sign, null, "\n"))

					# calculate p_pool
					suc1 = sum(data[group == levels(group)[1]] == success)
					fail1 = sum(data[group == levels(group)[1]] != success)
					suc2 = sum(data[group == levels(group)[2]] == success)
					fail2 = sum(data[group == levels(group)[2]] != success)
					p_pool =  (suc1 + suc2)/(n1 + n2)
					cat(paste("Pooled proportion =", round(p_pool, 4), "\n"))	
					
					# check conditions
					exp_suc1 = n1 * p_pool
					exp_fail1 = n1 * (1 - p_pool)
					cat(paste("Group 1: Number of expected successes =", round(exp_suc1), ";", "Number of expected failures =", round(exp_fail1)), "\n")
					exp_suc2 = n2 * p_pool
					exp_fail2 = n2 * (1 - p_pool)
					cat(paste("Group 2: Number of expected successes =", round(exp_suc2), ";", "Number of expected failures =", round(exp_fail2)), "\n")
					if(exp_suc1 < 10 | exp_fail1 < 10 | exp_suc2 < 10 | exp_fail2 < 10){
						stop("There aren't at least 10 expected successes and 10 expected failures, use simulation methods instead.")
					}
					# calculate test statistic and p-value
					se = sqrt( p_pool * (1 - p_pool) / n1 + p_pool * (1 - p_pool) / n2 )
					cat("Standard error =", round(se,3), "\n")
					teststat = (actual - null) / se
					cat(paste("Test statistic: Z = ", round(teststat, 3),"\n"))
					smaller.tail = round(min(pnorm(teststat), pnorm(teststat, lower.tail = FALSE)), 4)								
				}
				# alternative = less
				if(alternative == "less"){
					if(actual < null){cat(paste("p-value: ", smaller.tail,"\n"))}				
					if(actual > null){cat(paste("p-value: ", 1 - smaller.tail,"\n"))}
					normTail(L = teststat, axes = FALSE, col = "#22558833")
					axis(1, at = c(-3, teststat, 0, 3), labels = c(NA, paste(round(actual,2)), paste(null), NA))
				}
				# alternative = greater
				if(alternative == "greater"){
					if(actual < null){cat(paste("p-value: ", 1 - smaller.tail,"\n"))}				
					if(actual > null){cat(paste("p-value: ", smaller.tail,"\n"))}
					normTail(U = teststat, axes = FALSE, col = "#22558833")
					axis(1, at = c(-3, 0, teststat, 3), labels = c(NA, paste(null), paste(round(actual,2)), NA))
				}				
				# alternative = twosided	
				if(alternative == "twosided"){
					cat(paste("p-value: ", smaller.tail * 2,"\n"))
					if(actual < null){
						normTail(L = teststat, U = -1*teststat, axes = FALSE, col = "#22558833")
						axis(1, at = c(-3, teststat, 0, -1*teststat, 3), labels = c(NA, paste(round(actual,2)), paste(null), paste(round(null + (null - actual), 2)), NA))
					}
					if(actual > null){
						normTail(L = -1*teststat, U = teststat, axes = FALSE, col = "#22558833")
						axis(1, at = c(-3, -1*teststat, 0, teststat, 3), labels = c(NA, paste(round(null - (actual - null), 2)), paste(null), paste(round(actual,2)), NA))
					}				
				}
				par(mfrow = c(1,1))

			}
			
		}
	
	}
	if(simdist == TRUE){return(sim_dist)}
}