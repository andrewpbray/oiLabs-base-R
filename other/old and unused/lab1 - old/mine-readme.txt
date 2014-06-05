



-add video link for "how to format your labs"
-use subset for logicals or []?
-what's the easiest way to do side by side for wdiff by gender?
-ideas on ~ ?

some changes i(mine) made:
- exercise environment. i think by making these look like the exercises in the book the project looks more unified. i also think it might relieve some headache i'll get from "do we have to turn these in too…". i'm not holding my breath on that one.

- "six number summary" = "numerical summary"
since there is a widely used term called five number summary, i think this might be misleading the students to think "six number summary" is used as well. i don't think it is, right?

- weight is in the 6th column, not 8th, changed accordingly. possibly changed after you took out those two variables.

- "Both row-and-column notation and dollar-sign notation…" this sentence wasn't complete, I completed it as "… are widely used, which one you choose to use depends on your personal preference."

- changed the data set names for male and over 30 and make or over 30 from mdata to mOver30data and mOrOver30data respectively, to avoid overwriting mdata. not sure if these are the prettiest names but i think it's a good ideas to use different names. both to avoid overwriting and so they can see them separately in their workspace.

- changed ~ so it shows up funny but copies and pastes properly. also changed ``related to" to "versus". i think this definition will later be helpful in the linear regression chapter as well.

-the calculation of BMI is not correct, it should be divided by height^2. i fixed that and also added some language defining BMI and explaining where the 703 came from. unfortunately the ^2 looks bad on the pdf. i ended up having to put a space between ^ and 2 so it doesn't become 2-hat but maybe you have a better solution.

- "Notice that the first line above is just some arithmetic, but done on all 20,000 numbers in the cdc dataset." = "Notice that the first line above is just some arithmetic, but is applied to all 20,000 numbers in the \code{cdc} dataset."

- "divide by their height" =  "divide by their height-squared"

- question about describing distributions: added the sentence "Make sure to commend on any unusual observations."

- Question 4 on the on your own part was a bit vague. "Let's see if men tend to view this differently than women." = "Using numerical summaries and plots analyze if men tend to view this differently than women." Maybe this means we can get rid of the make side-by-side box plots? It's kinda implied

- still working on one more question. also will add a bit about making scatterplots. maybe between weight and height. since chp1 has those as well.