- cdc:

The Behavioral Risk Factor Surveillance System (BRFSS) is an annual telephone survey of 350,000 people in the United States collected by the Centers for Disease Control and Prevention (CDC). As its name implies, the BRFSS is designed to identify risk factors in the adult population and report emerging health trends. For example, respondents are asked about their diet and weekly physical activity, their HIV/AIDS status, possible tobacco use, and even their level of healthcare coverage. The BRFSS Web site (http://www.cdc.gov/brfss) contains a complete description of the survey, the questions that were asked and even research results that have been derived from the data.This data set is a random sample of 20,000 people from the BRFSS survey conducted in 2000. While there are over 200 questions or variables in this dataset, this data set only includes 9 variables.

Format
A data frame with 20,000 observations on the following 9 variables.
	genhlth: a categorical vector indicating general health, with categories excellent, very good, good, fair, and poor.
	exerany: a categorical vector, 1 if the respondent exercised in the past month and 0 otherwise
	hlthplan: a categorical vector, 1 if the respondent has some form of health coverage and 0 otherwise
	smoke100: a categorical vector, 1 if the respondent has smoked at least 100 cigarettes in their entire life and 0 otherwise
	height: a numerical vector, respondent's height in inches
	weight: a numerical vector, respondent's weight in pounds
	wtdesire: a numerical vector, respondent's desired weight in pounds
	age: a numerical vector, respondent's age in years
	gender: a numerical vector, respondent's gender