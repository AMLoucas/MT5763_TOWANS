/* MT5763 Group Project */
/* code for doing jackknife estimation */

%web_drop_table(WORK.SEALS);
FILENAME REFILE '/folders/myfolders/sasuser.v94/seals.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.SEALS;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.SEALS;
RUN;

%web_open_table(WORK.SEALS);

/*Jackknife Function
	INPUTS
	DataFile: the dataset to perform the analysis on
	X: the variable we want to analyze
	OUTPUTS
	estimate for standard error
	*/
%MACRO jackKnife(Datafile, X);

/*command for extracting the sample mean*/
PROC UNIVARIATE DATA=Vec noprint; 
VAR &X;
OUTPUT out=MEANX mean=sampmean;
RUN;

/*need to acquire size of the dataset (n) to know how many replicates will be needed*/
PROC SQL NOPRINT;
SELECT count(*) into :size from Vec;
QUIT;
    
/*obtain a dataset which is the sample mean repeated n times for later calculation*/
PROC SURVEYSELECT DATA=MEANX OUT=SAMPMEAN
method=srs samprate=1 rep=&SIZE. ;
RUN;
	
/*obtain n replications of the original data set*/
PROC SURVEYSELECT DATA=Vec OUT=VecLong
method=srs samprate=1 rep=&SIZE. ;
RUN;

/*delete sample i for each ith replication*/
DATA VecJack / VIEW = VecJack;
SET VecLong;
if replicate=mod(_n_,&SIZE.)+1 then delete;
RUN;
 
/*obtain the mean of each jackknifed sample*/
PROC UNIVARIATE data=VecJack noprint; 
VAR &X;
BY replicate;
OUTPUT out=jackMeans mean=mean;
RUN;
    
/*obtain the squared difference from the sample mean*/
DATA SquareDiffs;
MERGE jackMeans SampMean;
BY replicate;
SquareDiff = (mean - sampmean)**2;
RUN;
    
/*get the sum of the squared differences*/
PROC SUMMARY DATA=SquareDiffs;
VAR SquareDiff;
OUTPUT out=TotalDiffs sum=tot;
RUN;
	
/*calculate the standard error for storage*/
DATA Estimate;
SET TotalDiffs;
SE = SQRT((&SIZE. - 1) / &SIZE. * tot);
KEEP SE;
RUN;
 	
%MEND;

OPTIONS NONOTES;

/* Start the times, to count the function */
%let _timer_start = %sysfunc(datetime());

/* Calling function */
%jackKnife(WORK.SEALS, Lengths)
 	
/* Stop timer, obtain time taken to execute program */
data _null_;
  dur = datetime() - &_timer_start;
  put 30*'-' / ' TOTAL DURATION:' dur time13.2 / 30*'-';
run;
 
 	
 	

	
	