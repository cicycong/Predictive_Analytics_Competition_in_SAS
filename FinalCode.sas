Libname Game "/home/u48760567/5210Game";

* Macro variables for numeric variables and categorical variables;
%Global numvar;
%Global chavar;
%let numvar=Age AmtLastYear Frequency MaxGift MinGift NbActivities Recency Referrals
	Salary SeniorList Seniority TotalGift;
%let chavar=City Education GaveLastYear Woman;

/*
* Glance of previous donation situation;
Proc sql;
select count(*)
from(
select *
from Game.hist2
where AmtThisYear>25);
Quit;

Proc sql;
select count(*)
from(
select *
from Game.hist2
where AmtLastYear>25);
Quit;

proc means data=game.hist2;
	var AmtThisYear;
Run;

proc freq data=Game.hist2;
	table GaveThisYear GaveLastYear;
Run;
*/


data Game.Stage2clean(drop=Age);
	set Game.hist2;
	If Frequency=. Then Do;
		Frequency =0; M_Frequency=1;
	End;
	Else Do;
		M_Frequency=0;
	End;
	
	If MaxGift=. Then Do;
		MaxGift =0; M_MaxGift=1;
	End;
	Else Do;
		M_MaxGift=0;
	End;
	
	If MinGift=. Then Do;
		MinGift =0; M_MinGift=1;
	End;
	Else Do;
		M_MinGift=0;
	End;

	If TotalGift=. Then Do;
		TotalGift =0; M_TotalGift=1;
	End;
	Else Do;
		M_TotalGift=0;
	End;

	If Recency=. Then Do;
		Recency =10; M_Recency=1;
	End;
	Else Do;
		M_Recency=0;
	End;

	If Seniority=. Then Do;
		Seniority =10; M_Seniority=1;
	End;
	Else Do;
		M_Seniority=0;
	End;
	
	IF Age>=16 and Age <=25 Then Age_trans="16-25" ;
	IF Age>=26 and Age <=35 Then Age_trans="26-35" ;
	IF Age>=36 and Age <=45 Then Age_trans="36-45" ;
	IF Age>=46 and Age <=55 Then Age_trans="46-55" ;
	IF Age>=56 and Age <=65 Then Age_trans="56-65" ;
	If Age>=66 Then Age_trans=">65";
Run;


**************************************************************************************;
/*                             Data Splitting                                       */
**************************************************************************************;
**************************************************************************************;



/* prepare for target stratificatioon */
proc sort data=Game.Stage2clean out=develop; 
   by GaveThisYear; 
run;

/* set up data split indicator */
proc surveyselect noprint data=develop seed=11111 samprate=.6667 
		stratumseed=restore outall out=sample;
	strata GaveThisYear;
run;

/* verify target distribution split */
proc freq data=sample;
	tables GaveThisYear*selected;
run;

/* split data into training and validation */
data Game.hist2train(drop=selected SelectionProb SamplingWeight) 
     Game.hist2valid(drop=selected SelectionProb SamplingWeight);
	set sample;
	if selected=1 then output Game.hist2train;
	else output Game.hist2valid;
run;

*********************************************************************;
/*             Variable clustering        */
*********************************************************************;


proc contents data=Game.hist2train(drop=ID  AmtThisYear GaveThisYear) 
	out=temp(keep=name type) noprint;
run;

proc sql noprint;
	select name into: interval separated by " "
	from temp
	where type=1;
quit;

ods select clustersummary rsquare;

proc varclus data=Game.hist2train maxeigen=0.5 hi outtree=tree;
   var &interval;
run;

title1 'Dendrogram by Maximum Second Eigenvalue';
proc tree data=tree horizontal;
	height _MAXEIG_;
run;

/* selected VARCLUS variables (n=10) */
/* variables with the lowest 1– R2 ratio in each cluster
 are good representatives of their cluster*/

%let reduced= M_Frequency TotalGift Referrals  GaveLastYear AmtLastYear
				 Salary Contact  Woman SeniorList MinGift;


/*
             Variable Screening
*/


ods select none;
ods output spearmancorr=spearman
           hoeffdingcorr=hoeffding;

title1 'Spearman and Hoeffding Correlation';
proc corr data=Game.hist2train spearman hoeffding;
   var GaveThisYear;
   with &reduced;
run;

ods select all;

/* prepare for merge */
proc sort data=spearman;
    by variable;
run;

proc sort data=hoeffding;
    by variable;
run;

/* merge correlation files */
data temp;
   merge spearman(rename=(GaveThisYear=scorr pGaveThisYear=spvalue))
         hoeffding(rename=(GaveThisYear=hcorr pGaveThisYear=hpvalue));
   by variable;
   scorr_abs=abs(scorr);
   hcorr_abs=abs(hcorr);
run;

/* rank variables on Spearman's and Hoeffding correlations */
proc rank data=temp out=correlations descending;
    var scorr_abs hcorr_abs;
    ranks ranksp rankho;
run;

proc sort data=correlations;
   by ranksp;
run;

title1 "Rank of Spearman Correlations and Hoeffding Correlations";
proc print data=correlations label split='*';
   var variable ranksp rankho scorr spvalue hcorr hpvalue;
   label ranksp ='Spearman rank*of variables'
         scorr  ='Spearman Correlation'
         spvalue='Spearman p-value'
         rankho ='Hoeffding rank*of variables'
         hcorr  ='Hoeffding Correlation'
         hpvalue='Hoeffding p-value';  
run;

/* Find values for reference lines */
proc sql noprint;
   select min(ranksp) into :vref 
   from (select ranksp 
   from correlations );

   select min(rankho) into :href 
   from (select rankho
   from correlations);
quit;

/* Plot variable names, Hoeffding ranks, and Spearman ranks. */
title1 "Scatter Plot of the Ranks of Spearman vs. Hoeffding";
proc sgplot data=correlations;
   scatter y=ranksp x=rankho / datalabel=variable;
   yaxis label="Rank of Spearman";
   xaxis label="Rank of Hoeffding";
run;

title1 ;



%let screened= M_Frequency TotalGift Referrals  GaveLastYear AmtLastYear
			  Salary Contact  Woman SeniorList MinGift;




**************************************************************************************;
/*                       Logistic  Model Building                                    */
**************************************************************************************;
**************************************************************************************;

Data Game.hist2logtrian;
	Set Game.hist2train;
	Drop AmtThisYear;
Run;

Data Game.hist2logvalid;
	Set Game.hist2valid;
	Drop AmtThisYear;
Run;

******************************************************************************;
******************************************************************************;
/*                     Model With Variables Screened                        */
******************************************************************************;
******************************************************************************;  

title1 'Determine P-Value for Entry and Retention';
proc sql;
	select 1-probchi(log(sum(GaveThisYear ge 0)),1) into :sl
	from Game.hist2logtrian;
quit;

/*
title1 'Detect Interactions using Forward Selection';
proc logistic data=Game.hist2logtrian;
	class Age_trans(param=ref ref='16-25') City(param=ref ref='City') Education(param=ref ref='Elementary')  ;
	model GaveThisYear(event='1')= &screened Age_trans City Education/ 
			  clodds=pl selection=forward slentry=&sl;
run;
*/

%let simplelogistic= M_Frequency TotalGift Referrals  GaveLastYear 
			  Salary Contact  Woman SeniorList MinGift Age_trans City Education;


title1 'Fast Backward Selection';
proc logistic data=Game.hist2logtrian outmodel=stage2log;
	class Age_trans(param=ref ref='16-25') City(param=ref ref='City') Education(param=ref ref='Elementary')  ;
	model GaveThisYear(event='1')= &simplelogistic /
     clodds=pl selection=backward fast slstay=&sl hier=single;
    Score data=Game.hist2logvalid out = Game.hist2logvalid_predict fitstat;
    Store out=Game.hist2logstore;  
run;


*This model has a c stats of 0.71 at training data, and a AUC of 0.71 at validation data;	


******************************************************************************;
******************************************************************************;
/*                     Update Model With High Order Term                     */
******************************************************************************;
******************************************************************************;     


/*
title1 'Detect Interactions and HighOrder Term using Forward Selection';
proc logistic data=Game.hist2logtrian;
	class Age_trans(param=ref ref='16-25') City(param=ref ref='City') Education(param=ref ref='Elementary')  ;
	model GaveThisYear(event='1')= &screened Age_trans City Education
			     M_Frequency|TotalGift|Referrals|GaveLastYear|AmtLastYear|
				 Salary|Contact|Woman|SeniorList|MinGift|
				  Age_trans|City|Education @2
				  totalgift*totalgift referrals*referrals amtlastyear*amtlastyear
				  salary*salary seniorlist*seniorlist mingift*mingift/ 
			include=13 clodds=pl selection=forward slentry=&sl;
run;
*/

%let inter_highorder contact*age_trans contact*woman M_Frequency*contact contact*city 
				   referrals*referrals seniorlist*seniorlist contact*seniorlist gavelastyear*seniorlist
				   salary*contact totalgift*totalgift referrals*seniorlist city*education
				   mingift*mingift seniorlist*city M_frequency*seniorlist M_frequency*referrals
				   woman*age_trans referrals*contact contact*education referrals*gavelastyear
				   totalgift*referrals gavelastyear*contact gavelastyear*city;

title1 'Fast Backward Selection';
proc logistic data=Game.hist2logtrian outmodel=stage2log;
	class Age_trans(param=ref ref='16-25') City(param=ref ref='City') Education(param=ref ref='Elementary')  ;
	model GaveThisYear(event='1')= &screened Age_trans City Education &inter_highorder /
     clodds=pl selection=backward fast slstay=&sl hier=single;
    Score data=Game.hist2logvalid out = Game.hist2logvalid_predict fitstat;
    Store out=Game.hist2logstore;  
run;


*This model has a c stats of 0.721 at training data, and a AUC of 0.719 at validation data;
*Adding quadratic terms improved slightly from the interaction model.



******************************************************************************;
******************************************************************************;
/*                     Update Model With Interaction Term                     */
******************************************************************************;
******************************************************************************;     


	
title1 'Determine P-Value for Entry and Retention';
proc sql;
	select 1-probchi(log(sum(GaveThisYear ge 0)),1) into :sl
	from Game.hist2logtrian;
quit;

%let screened= M_Frequency TotalGift Referrals  GaveLastYear AmtLastYear
			  Salary Contact  Woman SeniorList MinGift;



/*
title1 'Detect Interactions using Forward Selection';
proc logistic data=Game.hist2logtrian;
	class Age_trans(param=ref ref='16-25') City(param=ref ref='City') Education(param=ref ref='Elementary')  ;
	model GaveThisYear(event='1')= &screened Age_trans City Education
			     M_Frequency|TotalGift|Referrals|GaveLastYear|AmtLastYear|
				 Salary|Contact|Woman|SeniorList|MinGift|
				  Age_trans|City|Education @2 / 
			include=13 clodds=pl selection=forward slentry=&sl;
run;
*/

%let interactions= contact*age_trans contact*woman M_Frequency*contact referrals*gavelastyear
				   contact*city contact*seniorlist salary*contact seniorlist*city M_frequency*referrals
				   M_frequency*seniorlist woman*age_trans gavelastyear*seniorlist referrals*contact
				   totalgift*referrals gavelastyear*contact city*education contact*education referrals*seniorlist
				   totalgift*salary totalgift*age_trans referrals*city;

title1 'Fast Backward Selection';
proc logistic data=Game.hist2logtrian outmodel=stage2log;
	class Age_trans(param=ref ref='16-25') City(param=ref ref='City') Education(param=ref ref='Elementary')  ;
	model GaveThisYear(event='1')= &screened Age_trans City Education &interactions /
     clodds=pl selection=backward fast slstay=&sl hier=single;
    Score data=Game.hist2logvalid out = Game.hist2logvalid_predict fitstat;
    Store out=Game.hist2logstore;  
run;

*This model has a c stats of 0.72 at training data, and a AUC of 0.718 at validation data;
*We could conclude that adding interaction terms improved the model.

* This model has been selected as our logistic model;




**************************************************************************************;
/*                                ROC Curve                                            */
**************************************************************************************;
%let simplelogistic= M_Frequency TotalGift Referrals  GaveLastYear 
			  Salary Contact  Woman SeniorList MinGift Age_trans City Education;
			  
title1 'Fast Backward Selection';
proc logistic data=Game.hist2logtrian outmodel=stage2log;
	class Age_trans(param=ref ref='16-25') City(param=ref ref='City') Education(param=ref ref='Elementary')  ;
	model GaveThisYear(event='1')= &simplelogistic /
     clodds=pl selection=backward fast slstay=&sl hier=single;
    Score data=Game.hist2logvalid out = Game.hist2logvalid_predict(rename=(p_1=p_11)) fitstat;
    Store out=Game.hist2logstore;  
run;


%let interactions= contact*age_trans contact*woman M_Frequency*contact referrals*gavelastyear
				   contact*city contact*seniorlist salary*contact seniorlist*city M_frequency*referrals
				   M_frequency*seniorlist woman*age_trans gavelastyear*seniorlist referrals*contact
				   totalgift*referrals gavelastyear*contact city*education contact*education referrals*seniorlist
				   totalgift*salary totalgift*age_trans referrals*city;


title1 'Fast Backward Selection';
proc logistic data=Game.hist2logtrian outmodel=stage2log;
	class Age_trans(param=ref ref='16-25') City(param=ref ref='City') Education(param=ref ref='Elementary')  ;
	model GaveThisYear(event='1')= &screened Age_trans City Education &interactions /
     clodds=pl selection=backward fast slstay=&sl hier=single;
    Score data=Game.hist2logvalid_predict out = Game.hist2logvalid_predict(rename=(p_1=p_12)) fitstat;
    Store out=Game.hist2logstore;  
run;


%let inter_highorder contact*age_trans contact*woman M_Frequency*contact contact*city 
				   referrals*referrals seniorlist*seniorlist contact*seniorlist gavelastyear*seniorlist
				   salary*contact totalgift*totalgift referrals*seniorlist city*education
				   mingift*mingift seniorlist*city M_frequency*seniorlist M_frequency*referrals
				   woman*age_trans referrals*contact contact*education referrals*gavelastyear
				   totalgift*referrals gavelastyear*contact gavelastyear*city;

title1 'Fast Backward Selection with Interaction and HighOrder';
proc logistic data=Game.hist2logtrian outmodel=stage2log;
	class Age_trans(param=ref ref='16-25') City(param=ref ref='City') Education(param=ref ref='Elementary')  ;
	model GaveThisYear(event='1')= &screened Age_trans City Education &inter_highorder /
     clodds=pl selection=backward fast slstay=&sl hier=single;
    Score data=Game.hist2logvalid_predict out = Game.hist2logvalid_predict(rename=(p_1=p_13)) fitstat;
    Store out=Game.hist2logstore;  
run;


ods select ROCOverlay ROCAssociation ROCContrastTest;
title1 "Validation Data Set Performance";
proc logistic data=Game.hist2logvalid_predict;
   model GaveThisYear(event='1')=p_11 p_12 p_13/nofit;
   roc "Simple Model" p_11;
   roc "Interaction Model" p_12;
   roc "Interaction & Quadratic Model" p_13;
   roccontrast "Comparing the Three Models";
run;





**************************************************************************************;
/*                Scoring on score2_contact/nocontact                               */
**************************************************************************************;
**************************************************************************************;

data Game.score2_contact_clean(drop=Age);
	set Game.score2_contact;
	If Frequency=. Then Do;
		Frequency =0; M_Frequency=1;
	End;
	Else Do;
		M_Frequency=0;
	End;
	
	If MaxGift=. Then Do;
		MaxGift =0; M_MaxGift=1;
	End;
	Else Do;
		M_MaxGift=0;
	End;
	
	If MinGift=. Then Do;
		MinGift =0; M_MinGift=1;
	End;
	Else Do;
		M_MinGift=0;
	End;

	If TotalGift=. Then Do;
		TotalGift =0; M_TotalGift=1;
	End;
	Else Do;
		M_TotalGift=0;
	End;

	If Recency=. Then Do;
		Recency =10; M_Recency=1;
	End;
	Else Do;
		M_Recency=0;
	End;

	If Seniority=. Then Do;
		Seniority =10; M_Seniority=1;
	End;
	Else Do;
		M_Seniority=0;
	End;
	
	IF Age>=16 and Age <=25 Then Age_trans="16-25" ;
	IF Age>=26 and Age <=35 Then Age_trans="26-35" ;
	IF Age>=36 and Age <=45 Then Age_trans="36-45" ;
	IF Age>=46 and Age <=55 Then Age_trans="46-55" ;
	IF Age>=56 and Age <=65 Then Age_trans="56-65" ;
	If Age>=66 Then Age_trans=">65";
Run;

data Game.score2_nocontact_clean(drop=Age);
	set Game.score2_nocontact;
	If Frequency=. Then Do;
		Frequency =0; M_Frequency=1;
	End;
	Else Do;
		M_Frequency=0;
	End;
	
	If MaxGift=. Then Do;
		MaxGift =0; M_MaxGift=1;
	End;
	Else Do;
		M_MaxGift=0;
	End;
	
	If MinGift=. Then Do;
		MinGift =0; M_MinGift=1;
	End;
	Else Do;
		M_MinGift=0;
	End;

	If TotalGift=. Then Do;
		TotalGift =0; M_TotalGift=1;
	End;
	Else Do;
		M_TotalGift=0;
	End;

	If Recency=. Then Do;
		Recency =10; M_Recency=1;
	End;
	Else Do;
		M_Recency=0;
	End;

	If Seniority=. Then Do;
		Seniority =10; M_Seniority=1;
	End;
	Else Do;
		M_Seniority=0;
	End;
	
	IF Age>=16 and Age <=25 Then Age_trans="16-25" ;
	IF Age>=26 and Age <=35 Then Age_trans="26-35" ;
	IF Age>=36 and Age <=45 Then Age_trans="36-45" ;
	IF Age>=46 and Age <=55 Then Age_trans="46-55" ;
	IF Age>=56 and Age <=65 Then Age_trans="56-65" ;
	If Age>=66 Then Age_trans=">65";
Run;

proc logistic inmodel=stage2log;
	score data=game.score2_contact_clean out=game.score2_contact_log;
Run;

proc logistic inmodel=stage2log;
	score data=game.score2_nocontact_clean out=game.score2_nocontact_log;
Run;




********************************************************************************************************************;
********************************************************************************************************************;
********************************************************************************************************************;

*select only the data that donate;

data Game.Stage2clean_donate;
	set Game.Stage2clean;
	where GaveThisYear=1;
Run;

proc means data=game.stage2clean_donate;
	var AmtThisYear;
Run;



**************************************************************************************;
/*                Correlation Detection                              */
**************************************************************************************;

/*
%Global numvar;
%Global chavar;
%let numvar=Age AmtLastYear Frequency MaxGift MinGift NbActivities Recency Referrals
	Salary SeniorList Seniority TotalGift M_Frequency;
%let chavar=City Education GaveLastYear Woman ;

ods graphics / reset=all imagemap;
 
title1 'Correlations and Scatter Plots with AmtThisYear';
proc corr data=Game.Stage2clean_donate rank;
	var AmtLastYear Frequency MaxGift MinGift NbActivities Recency Referrals
	Salary SeniorList Seniority TotalGift M_Frequency ;
	with AmtThisYear;
run;

* None of the variables have a very large linear correlation with AmtThisYear;

* SeniorList Seniority should be removed because of non linear relationship with amount donate.
*/

/*
title1 'Scatter Plot Matrix and Correlation of Numeric Variables';
proc corr data=Game.Stage2clean_donate rank;
	var  AmtLastYear Frequency  MinGift NbActivities Recency Referrals
	Salary  TotalGift M_Frequency ;
run;
*/
*Recency,M_Frequency are removed AmtLastYear   MinGift NbActivities  Referrals
	Salary  TotalGift  ;



**************************************************************************************;
/*                Linear Regression With Univariate Variables                              */
**************************************************************************************;

ods graphics;
proc glmselect data=Game.Stage2clean_donate plots=all seed=1234;
   class Age_trans(param=ref ref='16-25') City(param=ref ref='City') Education(param=ref ref='Elementary');
   model AmtThisYEar= Contact AmtLastYear Frequency  MinGift NbActivities  
   Referrals Salary  TotalGift Age_trans City Education/
               selection=backward select=sbc choose=validate;
   partition FRACTION (VALIDATE=0.3);
   store out=Game.hist2_reg_store;
   title "Selecting the Best Model using Honest Assessment";
run;

* Adjusted R2 0.008, F value 71.55, means we have found a significant model.;
* ASE of training and validation data greater than 50000;


**************************************************************************************;
/*           Linear With Interaction and Quadratic selected from correlation         */
**************************************************************************************;


ods graphics;
proc glmselect data=Game.Stage2clean_donate plots=all seed=1234;
   class Age_trans(param=ref ref='16-25') City(param=ref ref='City') Education(param=ref ref='Elementary');
   model AmtThisYEar= Contact AmtLastYear Frequency  MinGift NbActivities  
   Referrals Salary  TotalGift Age_trans City Education
   	Contact|AmtLastYear|Frequency|MinGift|NbActivities  
   Referrals|Salary|TotalGift|Age_trans|City|Education
   	AmtLastYear*AmtLastYear Frequency*Frequency MinGift*MinGift NbActivities*NbActivities
   	Referrals*Referrals Salary*Salary TotalGift*TotalGift/
               selection=backward select=sbc choose=validate;
   PARTITION FRACTION(VALIDATE=0.3 );
   store out=Game.hist2_reg_store;
   title "Selecting the Best Model using Honest Assessment";
run;

* Adjusted R2 0.014, ASE of training and validation data greater than 50000.;

* Due to the random splitting of train and valid dataset at a proportion of 7:3, 
the results generated could be different each time we run.;


**************************************************************************************;
/*               Regression With The Variable Screened from Logistics               */
**************************************************************************************;

%let screened= M_Frequency TotalGift Referrals  GaveLastYear AmtLastYear
			  Salary Contact  Woman SeniorList MinGift;


%let inter_highorder contact*age_trans contact*woman M_Frequency*contact contact*city 
				   referrals*referrals seniorlist*seniorlist contact*seniorlist gavelastyear*seniorlist
				   salary*contact totalgift*totalgift referrals*seniorlist city*education
				   mingift*mingift seniorlist*city M_frequency*seniorlist M_frequency*referrals
				   woman*age_trans referrals*contact contact*education referrals*gavelastyear
				   totalgift*referrals gavelastyear*contact gavelastyear*city;
				   
ods graphics;
proc glmselect data=Game.Stage2clean_donate plots=all seed=1234;
   class Age_trans(param=ref ref='16-25') City(param=ref ref='City') Education(param=ref ref='Elementary');
   model AmtThisYear= &screened Age_trans City Education &inter_highorder /
               selection=backward select=sbc choose=validate;
   PARTITION FRACTION(VALIDATE=0.3 );
   store out=Game.hist2_reg_store;
   title "Selecting the Best Model using Honest Assessment";
run;


**************************************************************************************;
/*   Regression With The Variable Screened from Logistics  (No Quadratic)            */
**************************************************************************************;
**************************************************************************************;
%let screened= M_Frequency TotalGift Referrals  GaveLastYear AmtLastYear
			  Salary Contact  Woman SeniorList MinGift;

%let interactions= contact*age_trans contact*woman M_Frequency*contact referrals*gavelastyear
				   contact*city contact*seniorlist salary*contact seniorlist*city M_frequency*referrals
				   M_frequency*seniorlist woman*age_trans gavelastyear*seniorlist referrals*contact
				   totalgift*referrals gavelastyear*contact city*education contact*education referrals*seniorlist
				   totalgift*salary totalgift*age_trans referrals*city;

ods graphics;
proc glmselect data=Game.Stage2clean_donate plots=all seed=1234;
   class Age_trans(param=ref ref='16-25') City(param=ref ref='City') Education(param=ref ref='Elementary');
   model AmtThisYear= &screened Age_trans City Education &interactions /
               selection=backward select=sbc choose=validate;
   PARTITION FRACTION(VALIDATE=0.3);
   store out=Game.hist2_reg_store;
   title "Selecting the Best Model using Honest Assessment";
run;


*Parameter of The Final Selected Linear Model;
proc glm data=Game.Stage2clean_donate plots=all;
   class Age_trans City Education;
   model AmtThisYear= &screened Age_trans City Education &interactions;
   title "Parameter Estimates of The Final Selected Linear Model";
run;
* Adjusted R2 0.013, ASE of training and validation data around 50000;

*This Model has been selected.



* predict on the score2 dataset;
proc plm restore=Game.hist2_reg_store;
   score data=game.score2_contact_log out=Game.score2_contact_log_reg;
   code file="/home/u48760567/5210Game/log_reg_scoring.sas";
run;

proc plm restore=Game.hist2_reg_store;
   score data=game.score2_nocontact_log out=Game.score2_nocontact_log_reg;
   code file="/home/u48760567/5210Game/log_reg_scoring_no.sas";
run;

* Adjusted R2 0.012, ASE of training and validation data around 52000;



**************************************************************************************;
/*                                 Merge Table                               */
**************************************************************************************;

* Contact Table;
PROC SORT DATA=Game.score2_contact_log_reg(keep=ID LastName I_GaveThisYear P_1 Predicted) 
	OUT=Game.contact_sort (rename=(I_GaveThisYear=ContactGave P_1=ContactProb Predicted=ContactAmt)); 
	BY ID;
RUN;

* NoContact Table;
PROC SORT DATA=Game.score2_nocontact_log_reg(keep=ID LastName I_GaveThisYear P_1 Predicted) 
	OUT=Game.nocontact_sort (rename=(I_GaveThisYear=NoContactGave P_1=NoContactProb Predicted=NoContactAmt)); 
	BY ID;
RUN;

data Game.Uplift;
	merge Game.contact_sort Game.nocontact_sort;
	by ID;
	EC= ContactProb*ContactAmt;
	ENC=NoContactProb*NoContactAmt;
	Uplift=EC-ENC;
Run;

Proc sql;
CREATE TABLE Game.upliftselected AS
select ID
from Game.Uplift
where Uplift>30
order by  Uplift desc;
Quit;



proc export data=Game.upliftselected 
outfile="/home/u48760567/5210Game/finaldecision.csv" dbms=csv
replace;
run;


**************************************************************************************;
/*                                Try Something Else                                */
**************************************************************************************;

/*
data Game.Stage2clean_try;
	set Game.Stage2clean;
	Amtlog=log(AmtThisYear);
	where GaveThisYear=1;
Run;

%let screened= M_Frequency TotalGift Referrals  GaveLastYear AmtLastYear
			  Salary Contact  Woman SeniorList MinGift;

%let interactions= contact*age_trans contact*woman M_Frequency*contact referrals*gavelastyear
				   contact*city contact*seniorlist salary*contact seniorlist*city M_frequency*referrals
				   M_frequency*seniorlist woman*age_trans gavelastyear*seniorlist referrals*contact
				   totalgift*referrals gavelastyear*contact city*education contact*education referrals*seniorlist
				   totalgift*salary totalgift*age_trans referrals*city;

ods graphics;
proc glmselect data=Game.Stage2clean_try plots=all;
   class Age_trans(param=ref ref='16-25') City(param=ref ref='City') Education(param=ref ref='Elementary');
   model Amtlog= &screened Age_trans City Education &interactions /
               selection=backward select=sbc choose=validate;
   PARTITION FRACTION(VALIDATE=0.3);
   store out=Game.hist2_reg_store;
   title "Selecting the Best Model using Honest Assessment";
run;




* predict on the score2 dataset;
proc plm restore=Game.hist2_reg_store;
   score data=game.score2_contact_log out=Game.score2_contact_log_reg;
   code file="/home/u48760567/5210Game/log_reg_scoring.sas";
run;

proc plm restore=Game.hist2_reg_store;
   score data=game.score2_nocontact_log out=Game.score2_nocontact_log_reg;
   code file="/home/u48760567/5210Game/log_reg_scoring_no.sas";
run;

data Game.score2_contact_log_reg;
	set Game.score2_contact_log_reg;
	Predicted=Exp(Predicted);
Run;
	
data Game.score2_nocontact_log_reg;
	set Game.score2_nocontact_log_reg;
	Predicted=Exp(Predicted);
Run;

/*

**************************************************************************************;
/*                                 Merge Table                               */
**************************************************************************************;
/*
* Contact Table;
PROC SORT DATA=Game.score2_contact_log_reg(keep=ID LastName I_GaveThisYear P_1 Predicted) 
	OUT=Game.contact_sort (rename=(I_GaveThisYear=ContactGave P_1=ContactProb Predicted=ContactAmt)); 
	BY ID;
RUN;

* NoContact Table;
PROC SORT DATA=Game.score2_nocontact_log_reg(keep=ID LastName I_GaveThisYear P_1 Predicted) 
	OUT=Game.nocontact_sort (rename=(I_GaveThisYear=NoContactGave P_1=NoContactProb Predicted=NoContactAmt)); 
	BY ID;
RUN;

data Game.Uplift;
	merge Game.contact_sort Game.nocontact_sort;
	by ID;
	EC= ContactProb*ContactAmt;
	ENC=NoContactProb*NoContactAmt;
	Uplift=EC-ENC;
Run;

Proc sql;
CREATE TABLE Game.upliftselected AS
select ID
from Game.Uplift
where Uplift>0
order by  Uplift desc;
Quit;

/*
