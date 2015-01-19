- arbuthnot:

Arbuthnot’s data on male and female birth ratios in London from 1629-1710. (From HistData package in R, http://cran.r-project.org/web/packages/HistData/HistData.pdf)

Description
John Arbuthnot (1710) used these time series data on the ratios of male to female births in London from 1629-1710 to carry out the first known significance test, comparing observed data to a null hypothesis. The data for these 81 years showed that in every year there were more male than female christenings.

On the assumption that male and female births were equally likely, he showed that the probability of observing 82 years with more males than females was vanishingly small (4.14x10^-25). He used this to argue that a nearly constant birth ratio > 1 could be interpreted to show the guiding hand of a devine being. The data set adds variables of deaths from the plague and total mortality obtained by Campbell and from Creighton (1965).

Format
A data frame with 82 observations on the following 3 variables.
	year: a numeric vector, 1629-1710
	boys: a numeric vector, number of male christenings
	girls: a numeric vector, number of female christenings