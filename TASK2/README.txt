|---------------------------------------------------------|
|--------------------README TASK 2------------------------|
|---------------------------------------------------------|

Macro for estimating standard error of a variable from jackknifing, applied to seal dataset. 

Calculated SE of Mean Seal Length: 0.5537712468cm

Includes timer for length of the jackknife, times ranging from 0.5-3s in testing.

Pseudocode for Macro Process

[1] Extract the sample mean and obtain a dataset with it repeated n times
[2] Acquire the size of the data, n
[3] Obtain n replications of the original dataset
[4] For each ith replication, leave out the ith observation
[5] Calculate the mean for each jackknifed sample
[6] Merge these n means with the sample mean repeated n times
[7] Extract the squared difference between each mean and the sample mean
[8] Sum the squared differences, apply required transformations, and store SE

