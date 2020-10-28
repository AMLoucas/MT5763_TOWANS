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

	DATA Vec;
	SET &Datafile;
	KEEP &X;
	RUN;
	
	PROC UNIVARIATE DATA=Vec noprint; 
    VAR &X;
    OUTPUT out=MEANX mean=sampmean;
    RUN;
    
    PROC SQL NOPRINT;
 	SELECT count(*) into :size from Vec;
 	QUIT;
    
    PROC SURVEYSELECT DATA=MEANX OUT=SAMPMEAN
 	method=srs samprate=1 rep=&SIZE. ;
 	RUN;
	
	PROC SURVEYSELECT DATA=Vec OUT=VecLong
 	method=srs samprate=1 rep=&SIZE. ;
 	RUN;
 	
	DATA VecJack / VIEW=VecJack;
 	set VecLong;
 	if replicate=mod(_n_,&SIZE.)+1 then delete;
 	RUN;
 	
 	PROC UNIVARIATE data=VecJack noprint; 
    VAR &X;
    BY replicate;
    OUTPUT out=jackMeans mean=mean;
    RUN;
    
    DATA MeanDiffs;
    MERGE jackMeans SampMean;
    BY replicate;
    RUN;
    
    DATA SquareDiffs;
    SET MeanDiffs;
    SquareDiff = (mean - sampmean)**2;
    RUN;
    
    PROC SUMMARY DATA=SquareDiffs;
	var SquareDiff;
	output out=TotalDiffs sum=tot;
	RUN;
	
	DATA Estimate;
	SET TotalDiffs;
	SE = SQRT((&SIZE. - 1) / &SIZE. * tot);
	KEEP SE;
	RUN;
 	
 	%MEND;
 	
%jackKnife(WORK.SEALS, Lengths)
 	
 
 	
 	

	
	
