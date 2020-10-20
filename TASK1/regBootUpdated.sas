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
%MACRO bootStrap(DataFile, X, Y);

/*Number of rows in my dataset*/
DATA _null_;
	set &DataFile NOBS=size;
	call symput("NROW",size);
	stop;
RUN;
 
/* Bootstrap loop for simulating data */
PROC SURVEYSELECT 
	data=&DataFile
	out=WORK.bootData seed=23434
	method=urs noprint sampsize=&NROW outhits rep=20;
RUN;

/* Create model for each loop/simulation */
PROC REG data=WORK.bootData 
	outest=WORK.ESTIMATES  noprint;
	Model &Y=&X;
	BY Replicate;
RUN;
QUIT;

/*Extract just the columns for slope and intercept for storage*/
DATA WORK.ESTIMATES;
	SET WORK.ESTIMATES;
	KEEP Intercept &X;
	RENAME Intercept=RandomIntercept &X=RandomSlope;	
RUN;
%MEND;


%bootStrap(DataFile = WORK.SEALS_UPDATED, X = Lengths, Y = Testosterone);
