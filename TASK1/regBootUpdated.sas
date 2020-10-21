/* MT5763 GROUP PROJECT */
/* Updated bootstrap code to be faster */
FILENAME REFFILE '/folders/myfolders/sasuser.v94/GROUP-BootStrap/seals.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.SEALS_UPDATED;
	GETNAMES=YES;
	GUESSINGROWS=MAX;
RUN;

PROC CONTENTS DATA=WORK.SEALS_UPDATED; 
RUN;

/* Boostrap function */
%MACRO bootStrap(DataFile, X, Y, SampleSet);

/* Bootstrap loop for simulating data */
PROC SURVEYSELECT 
	data=&DataFile
	out=WORK.bootData seed=23434
	/* SAMPRATE = HELPS US NO NEED TO FIND THE OBSERVATION SETS SIZE. FINDS ITS FOR US */
	/* REP = IS THE NUMBER OF TIMES YOU WANT THE SIMULATION TO OCCUR */
	/* METHOS = IS TO CREATE THE SAMPLES IN RANDOM UNIFORM WAY */
	/* OUTHITS = ENSURES EACH RECORD IS SAVED, RATHER THAN JUST 1 SIMULATION */
	method=urs noprint SAMPRATE=1 outhits rep=&SampleSet;
RUN;


/* Create model for each loop/simulation */
PROC REG data=WORK.bootData 
	outest=WORK.ESTIMATES  noprint;
	Model &Y=&X;
	/* REPLICATE = VARIABLE THAT WORKS AS A SIMULATION INDEX. ALL RANDOM SAMPLES
		FROM THE SAME SIMULATION HOLD THE SAME REPLICATE VALUE */
	/* BY REPLICATE = MEANS A MODEL WILL BE FITTED FOR EACH SIMULATION THAT WAS APPLIED */
	BY Replicate;
RUN;
QUIT;

/*Extract just the columns for slope and intercept for storage */
DATA WORK.ESTIMATES;
	SET WORK.ESTIMATES;
	/* Keeping 2 columns of interest and renaming to appropriate names */
	KEEP Intercept &X;
	RENAME Intercept=RandomIntercept &X=RandomSlope;	
RUN;

%MEND;

OPTIONS NONOTES;

/* Start the times, to count the function */
%let _timer_start = %sysfunc(datetime());

/* Calling function */
%bootStrap(DataFile = WORK.SEALS_UPDATED, X = Lengths, Y = Testosterone, SampleSet = 50000);

/* Stop timer, obtain time taken to execute program */
data _null_;
  dur = datetime() - &_timer_start;
  put 30*'-' / ' TOTAL DURATION:' dur time13.2 / 30*'-';
run;


