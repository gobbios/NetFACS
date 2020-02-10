# netfacstestr


# workflow for development stages

`devtools::document()`

`devtools::build_vignettes()` (if necessary)

`devtools::check()`

`devtools::install()` or `devtools::install(build_vignettes = TRUE)`

So what are we going to with this? Also, there is another potential step in the development pipeline:

`devtools::test()`


############## 18/10/2019 v.0.0.0.901 Alex

Set up basic functions and uploaded a basic dataset; all functions come with basic documentation set up and parameters described

 - 'netfacstestdata' is a very standard FACS dataset with ~1500 rows signifying one independent facial expression each (so not frame-by-frame coding). We cannot use this for the final version, as the two underlying databases are not public, but for now it works

Some helper functions:

 - 'combine.columns' is a function that lets the user take two columns (ie two Action Units) and combine them (for example if 'looking left' and 'looking right' should be turned into 'looking sideways')
 - 'prepare.framewise' is a function that takes subsequent rows/events that have exactly the same information (i.e., same action units active) and aggregates them, turning e.g. 4 rows into one that then has a duration value attached
 - 'create.random.net' is the basic permutation function; the user defines the dataset, potential control variables, and whether permutations should keep rowSums and/or colSums constant, and how many iterations should be run; and the function returns a list of randomised permutations (can be parallelized)

And one actual statistical test:
 - 'element.descriptives' is a function that tests whether the observed sum/duration/frequency of activity of an Action Unit differs significantly from random using permutations of the element matrix that keep the number of elements per row constant but randomises how often each element/AU occurs



############### 13/12/19 v.0.0.0.902 Alex

There have been some pretty dramatic changes in all functions. Most importantly, rather than having to redo the bootstraps/permutations over and over again, one now creates an object first using the 'netfacs' function, and all other analyses go from there.

All functions now use that netfacs object. All functions are now effectively based on probabilities, because they are the most meaningful measure in this context. Most plots are ggplot or ggnet; some ar igraph.plot. 
All network objects can be created as 'igraph' or 'sna', so that all functions from those two packges (and the 'network' package) can be applied to these objects.

############### 04/02/2020 v.0.0.0.995 Alex

NetFACS has now been moved from 'netfacstestr' to a bespoke GitHub server that can be accessed and that we can work on from here. Tutorial should follow soon.

############### 10/02/2020

There are currently problems with 
 extract.netfacs function, which is turned into S3method rather than a function by the documentation; 
 the element.bayesian function, which has problems with the description vignette;
 the arules.return function, which does not seem to recognise the confidence and support values from the function calls;
neither of the last two happens when simply sourcing the two functions outside the package, so this must have something to the with how the documentation is created