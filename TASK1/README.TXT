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

TIMERS FOR DIFFERENT NUMBER OF LOOPS

TABLE
|   N    | Provided Code | Updated code | 
-----------------------------------------
|  20    |   0:00:00.59  |  0:00:00.04  | 
|  100   |   0:00:02.44  |  0:00:00.04  | 
|  500   |   0:00:11.71  |  0:00:00.09  | 
|  1000  |   0:00:23.78  |  0:00:00.11  | 
|  5000  |   0:01:59.56  |  0:00:00.41  | 
|  10000 |   0:04:01.78  |  0:00:00.79  | 
|  20000 |   0:08:42.88  |  0:00:01.53  | 
|  50000 |   0:23:31.36  |  0:00:03.69  | 
|  100000|   ----------  |  0:00:07.79  | 
