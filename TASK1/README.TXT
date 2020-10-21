|---------------------------------------------------------|
|--------------------README TASK 1------------------------|
|---------------------------------------------------------|

MAIN CHANGES:

[1] Not using a function to obtain the number of rows of the file. Used command SAMPRATE in function SURVEYSELECT. 
SURVEYSELECT -> Is an equivelant function to R's sample(). It creates random samples for the data you want.
SAMPRATE -> by putting the value 1. We are telling the function SURVEYSELECT to create the sample size same as the original
dataset. 

[2] Did not use a loop, instead used the command rep to specify how many sample datasets we want the function SURVEYSELECT 
to create. This allowas us to create as many random datasets we want before we compute statistics on the data. Now we have 
all our data ready for computations. Each sample dataset simulation is is seperated by a column REPLICATE that holds an integer 
number that represents which simulation constructed it.

[3] Fitting a regression model only ones on all our simulation. Having the command "BY REPLICATE". This allowed the program
to fit a different linear regression model for each simulation dataset construnction. Since replicate value indicates the simulation it was 
created from. BY REPLICATE, groups all the records byt the simulation it was constructed and fits a different model for each simulated 
random dataset.

[4] We do not need to append results in a table for each simulation. Since we fitted all the models in one command. We have a table that holds
all of the fitted models estimates. We now only need to keep the columns of interests from the table that holds all the estimates value. The columns of interest
are Intecept Estimation and X-variable Estimation. 

---------------------------------------------------------
