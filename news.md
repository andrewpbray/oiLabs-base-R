# OpenIntro Labs dplyr conversion

The following changes are being considered for incorporation into a new
version of the OpenIntro Labs.

#### Convert everything to data frames and remove square bracket notation

This would be a major rewrite, but (without looking through all the labs)
it seems like it'd be possible to remove *all* references to the vector
structure of R and just use a lot of `select()`. An alternative would be to
not go whole hog in the dataframe direction and leave some vectors in. But
the arguements for the full rewrite:

**Pros:**

- dplyr literate syntax only. So instead of dollar-sign, vector subsetting,
matrix subsetting, and the `subset()` function, there'd only be `filter()`
and `select()`.
- We could introduce chains from lab 1 as the new normal.
- The logic of dplyr is similar to that of ggplot, so it opens up more
powerful graphics options.
- Removal of the word vector, which may might be a bit daunting to 
many students.

**Cons:**

- I haven't gone through all the labs yet, so there might be some topics that
will require an ugly hack or be dropped altogether.
- For-loops will require reworking. I think our best option would likely be to
use the `do()` function in mosaic.
- This gets fairly far away from traditional R syntax, which will make googling
around more confusing for students.

One thing that I think we would need to add if we did this is a lab that *did*
focus on vectors, constructing data frames, and manipulating them using tidyr.


#### Convert all data files to RData

Looks like we've been sourcing .R files for the labs that don't have custom 
functions. Seems really awkward to have the data in an R script. The most
straightfoward fix would be to have them download and load an RData file
instead, which is what the latter labs do anyway. Here's an [example](https://github.com/andrewpbray/oiLabs-dplyr/commit/f8f0750f188865832dacccfde8b97982723e760c).

Even better is putting it all in the oiLabs package. Are there any arguments
in favor of keeping the data hosted on the openintro website instead of in
a package on github?



