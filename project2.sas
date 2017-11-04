
LIBNAME Project '/folders/myfolders/data.codes.survival/';

*Importing the dataset;

proc import datafile="/folders/myfolders/data.codes.survival/FermaLogis_Event_Type.csv"
out=Project.mydata2 dbms=csv replace; 
run;

*Creating dummy variables;
data project.empdata2;
length turnoverType $25.;
set project.mydata2;
*To fix discrepancies in data;
IF Type = 0 THEN Turn_dum=0;
else Turn_dum=1;

*IF Turnover = 'Yes' THEN Turn_dum = 1
ELSE Turn_dum = 0;

IF Gender  = 'Male' THEN Gender_dum = 1;
ELSE Gender_dum = 0;

IF MaritalStatus = 'Single' THEN MaritalStatus_dum = 0;
ELSE IF MaritalStatus = 'Married' THEN MaritalStatus_dum  = 1;
ELSE MaritalStatus_dum=2;
       

IF OverTime = 'Yes' THEN OverTime_dum= 1;
ELSE OverTime_dum = 0;
	
IF BusinessTravel = 'Non-Travel' THEN BusinessTravel_dum = 0;
ELSE IF BusinessTravel  = 'Travel_Rarely' THEN BusinessTravel_dum = 1;
ELSE BusinessTravel_dum = 2;

IF Department = 'Human Resources' THEN Department_dum = 0;
ELSE IF Department = 'Research & Development' THEN Department_dum = 1;
ELSE Department_dum= 2;

IF EducationField = 'Human Resources' THEN EducationField_dum = 0;
ELSE IF EducationField = 'Life Sciences' THEN EducationField_dum = 1;
ELSE IF EducationField = 'Marketing' THEN EducationField_dum = 2;
ELSE IF EducationField = 'Medical' THEN EducationField_dum = 3;
ELSE IF EducationField = 'Other' THEN EducationField_dum = 4;
ELSE EducationField_dum = 5;
		
		
IF JobRole= 'Sales Executive' THEN JobRole_dum = 0;
ELSE IF JobRole = 'Research Scientist' THEN JobRole_dum = 1;
ELSE IF JobRole = 'Laboratory Technician' THEN JobRole_dum = 2;
ELSE IF JobRole = 'Manufacturing Director' THEN JobRole_dum = 3;
ELSE IF JobRole = 'Healthcare Representative' THEN JobRole_dum = 4;
ELSE IF JobRole = 'Manager' THEN JobRole_dum = 5;
ELSE IF JobRole = 'Sales Representative' THEN JobRole_dum = 6;
ELSE IF JobRole = 'Research Director' THEN JobRole_dum = 7;
ELSE IF JobRole_dum = 8;

IF STOCKOPTIONLEVEL=0 THEN STOCKS=0;
ELSE STOCKS=1;

*Stocks vs Turnover - answering professor's question from project 1 feedback;

proc lifetest data=project.empdata2 plots=(S H) method=LIFE;
	TIME YearsAtCompany*Turn_dum(0);
    strata stocks type;
    
	title "Survival curves of turnover with respect to stocks";
run;
*Hazard and Survival Curves rate by stratifying with Stock levels of an employee in fermalogis;
*Retiring employees;

proc lifetest data=project.empdata2 plots=(S H) method=LIFE;
	TIME YearsAtCompany*Type(0, 2, 3, 4);
	strata stocks;
	title "Retirement type with respect to stock";
run;

*Hazard and Survival Curves rate by stratifying with Stock;
*Voluntary Resignation;

proc lifetest data=project.empdata2 plots=(S H) method=LIFE;
	TIME YearsAtCompany*Type(0, 1, 3, 4);
	strata stocks;
	title "Voluntary Resignation type with respect to stock";
run;

*Hazard and Survival Curves rate by stratifying with Stock;
*Involuntary Resignation;

proc lifetest data=project.empdata2 plots=(S H) method=LIFE;
	TIME YearsAtCompany*Type(0, 1, 2, 4);
	strata stocks;
	title "Involuntary Resignation type with respect to stock";
run;

*Hazard and Survival Curves rate by stratifying with Stock;
*Job Termination;

proc lifetest data=project.empdata2 plots=(S H) method=LIFE;
	TIME YearsAtCompany*Type(0, 1, 2, 3);
	strata stocks;
	title "Job Termination type with respect to stock";
run;

*Frequency of occurence of each event type;
proc freq data=Project.empdata2;
	*where Turn_dum ne 0;
	tables Type*Turn_dum /chisq;
run;
 *Model 1;
PROC PHREG DATA=project.empdata2;
class Bonus_1;
   MODEL YearsAtCompany*Turn_dum(0)=  Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager  /TIES=EFRON;
RUN;

/* 	      Array Bonus_(*)  Bonus_1 - Bonus_40; */
/* 	      Bonus_var=Bonus_[Yearsatcompany]; */

/* Effect of cumulative bonus variable */
data temp;
set project.empdata2;
where yearsatcompany>1;
	      Array Bonus_(*)  Bonus_1 - Bonus_40;
	      Do i=1 to 40;
	      	if Yearsatcompany=i then bonus_var=bonus_[i];
	      end;
	      run;
data project.empdata_cumu;
set project.empdata2;
array bonus(*) bonus1-bonus40;
array bonus_(*) bonus_1-bonus_40;
array cum(*) cum1-cum40;
bonus1=input(tranwrd(tranwrd(compress(bonus_1), "", "0"), "NA", "0"),8.);
cum1=bonus1;
do i=2 to 40;
/* bonus(i)=tranwrd(compress(bonus_(i)), "", "0"); */
	bonus(i)=input(tranwrd(tranwrd(compress(bonus_(i)), "", "0"), "NA", "0"),8.);
	cum(i)=(cum(i-1)*(i-1) + bonus(i))/i;
	if cum(i)=. then cum(i)=0;
end;
run;

*Model 2 - best model;
PROC PHREG DATA=project.empdata_cumu;
*where Yearsatcompany>1; 
   MODEL YearsAtCompany*Turn_dum(0)=  Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager CumBonus_var/TIES=DISCRETE;
	      Array CumBonus(*)  cum1 - cum40; 
	      if yearsatcompany>0 then
 	      CumBonus_var=cumBonus[Yearsatcompany]; 
 	      else 
 	      CumBonus_var=0;
RUN;



*a---Rahul;

*b --- Jyothsna----two approaches 1. Use the model mentioned above to answer the question;
*For visualisations - you may want to look at what I did above with Stocks vs turnover ;
*You can change the turnover type using the where condition, an example is given below;
PROC PHREG DATA=project.empdata_cumu;
where Yearsatcompany>1 AND TYPE=2;
   MODEL YearsAtCompany*Turn_dum(0)=  Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager CumBonus_var/TIES=DISCRETE;
	      Array CumBonus(*)  cum1 - cum40; 
 	      CumBonus_var=cumBonus[Yearsatcompany]; 
RUN;

*c----Anita;
*Additional models which might help you - check and compare performance of Bonus variables, to reiterate why
we chose cumulative bonus in the final model;
/* Time dependent covariates  */
PROC PHREG DATA=project.empdata2;
   MODEL YearsAtCompany*Turn_dum(0)= td_var Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager/TIES=efron;
	      if TotalWorkingYears>YearsAtCompany then td_var=0;else td_var=1;
RUN;

/* Time dependent covariates measured at regular intervals */
PROC PHREG DATA=project.empdata2;
   MODEL YearsAtCompany*Turn_dum(0)=  Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager Bonus_var/TIES=EFRON;
	      Array Bonus_(*)  Bonus_0 - Bonus_40;
	      Bonus_var=Bonus_[Yearsatcompany];
/* 	      Bonus_0="0"; */
/* 	      if Yearsatcompany>0 then Bonus_var=Bonus_[Yearsatcompany]; else Bonus_var="0"; */
RUN;

*d--Apoorva;
*Pasting Anil's code- not sure what is right---re-edit using final model and work---change variables also;

PROC PHREG DATA=project2.empdata;
   *WHERE YearsAtCompany > 5;
   MODEL YearsAtCompany*Turn_dum(0)=Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager bonus_dum dailyrate_years Department_years MaritalStatus_years YearsWithCurrManager_years 
 		YearsInCurrentRole_years TotalWorkingYears_years  / TIES=EFRON;
   	dailyrate_years = DailyRate*YearsAtCompany;
   	Department_years = Department_dum*YearsAtCompany;
   	MaritalStatus_years = MaritalStatus_dum*YearsAtCompany;
   	YearsWithCurrManager_years = YearsWithCurrManager*YearsAtCompany;
   	YearsInCurrentRole_years = YearsInCurrentRole*YearsAtCompany;
   	TotalWorkingYears_years = TotalWorkingYears*YearsAtCompany;

RUN;

*Are all events the same;
DATA project2.retired; /*create constitutional exit data*/
  SET project2.empdata;
  event=(Type=1);  /*this is for censoring out other types, another way to write if statement*/
  type1='retired';
DATA project2.resigned;  /*create natural death data*/
  SET project2.empdata;
  event=(Type=2);
  type1='resigned';
DATA project2.involuntary; /*create nonconstitutional exit data*/
  SET project2.empdata;
  event=(Type=3);
  type1='involuntary';
DATA project2.jobterm; /*create nonconstitutional exit data*/
  SET project2.empdata;
  event=(Type=4);
  type1='jobterm';
DATA project2.combine; /*we combined the datasets to use them as strata in the graphical analysis*/
  SET project2.retired project2.resigned project2.involuntary project2.jobterm;
  
PROC LIFETEST DATA=project2.combine PLOTS=LLS;  /*LLS plot is requested*/
  TIME YearsAtCompany*Turn_dum(0);
  STRATA type /diff=all;
RUN;


*d;
PROC PHREG DATA=project.empdata_cumu;
   MODEL YearsAtCompany*Type(0, 1, 2) = Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager TimeInteract1 TimeInteract2 TimeInteract3 TimeInteract4
	      TimeInteract5 TimeInteract6 TimeInteract7 TimeInteract8 TimeInteract9 TimeInteract10
	      TimeInteract11 TimeInteract12 TimeInteract13 TimeInteract14 TimeInteract15 TimeInteract16 TimeInteract17
	      CumBonus_var/TIES=DISCRETE;
	    *TimeIntercatWorkingYears=YearsAtCompany*TotalWorkingYears;
		*TimeIntercatCurrentRole=YearsAtCompany*YearsInCurrentRole;
		*TimeIntercatNumCompaniesWorked=YearsAtCompany*NumCompaniesWorked;
         * TimeInteractBusinesstravel=YearsAtCompany*BusinessTravel_dum;
TimeInteract1=	YearsAtCompany*BusinessTravel_dum;
TimeInteract2=	YearsAtCompany*Department_dum;
TimeInteract3=	YearsAtCompany*DistanceFromHome;
TimeInteract4=	YearsAtCompany*EnvironmentSatisfaction;
TimeInteract5=	YearsAtCompany*Gender_dum;
TimeInteract6=	YearsAtCompany*JobInvolvement;
TimeInteract7=	YearsAtCompany*JobRole_dum;
TimeInteract8=	YearsAtCompany*JobSatisfaction;
TimeInteract9=	YearsAtCompany*MaritalStatus_dum;
TimeInteract10=	YearsAtCompany*NumCompaniesWorked;
TimeInteract11=	YearsAtCompany*Overtime_dum;
TimeInteract12=	YearsAtCompany*RelationshipSatisfaction;
TimeInteract13=	YearsAtCompany*TotalWorkingYears;
TimeInteract14=	YearsAtCompany*TrainingTimesLastYear;
TimeInteract15=	YearsAtCompany*WorkLifeBalance;
TimeInteract16=	YearsAtCompany*YearsInCurrentRole;
TimeInteract17=YearsAtCompany*YearswithCurrManager;
 
		title PHreg model for involuntry resignation and job termination/turnover;
	      Array CumBonus(*)  cum1 - cum40; 
	      if yearsatcompany>0 then
 	      CumBonus_var=cumBonus[Yearsatcompany]; 
 	      else 
 	      CumBonus_var=0;
RUN;
*Rahul;

*Implementing phreg using programming step for all the turnover types in one model;

PROC PHREG DATA=project.empdata_cumu;
   MODEL YearsAtCompany*Type(0) = Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager TimeIntercat1 TimeIntercat2 TimeIntercat3 TimeInteract4
	      CumBonus_var/TIES=DISCRETE;
	    TimeIntercat1=YearsAtCompany*TotalWorkingYears;
		TimeIntercat2=YearsAtCompany*YearsInCurrentRole;
		TimeIntercat3=YearsAtCompany*NumCompaniesWorked;
		TimeInteract4=YearsAtCompany*YearsWithCurrManager;
		
		title PHreg model for all types/ turnover;
	      Array CumBonus(*)  cum1 - cum40; 
	      if yearsatcompany>0 then
 	      CumBonus_var=cumBonus[Yearsatcompany]; 
 	      else 
 	      CumBonus_var=0;
RUN;

*Implementing phreg using programming step for the type Retirement;

PROC PHREG DATA=project.empdata_cumu;
   MODEL YearsAtCompany*Type(0,2,3,4) = Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager TimeIntercat1 TimeIntercat2 TimeIntercat3 TimeInteract4
	      CumBonus_var/TIES=DISCRETE;
        TimeIntercat1=YearsAtCompany*TotalWorkingYears;
		TimeIntercat2=YearsAtCompany*YearsInCurrentRole;
		TimeIntercat3=YearsAtCompany*NumCompaniesWorked;
		TimeInteract4=YearsAtCompany*YearsWithCurrManager;
		title PHreg model for retirement/ turnover;
	      Array CumBonus(*)  cum1 - cum40; 
	      if yearsatcompany>0 then
 	      CumBonus_var=cumBonus[Yearsatcompany]; 
 	      else 
 	      CumBonus_var=0;
Run;

*Implementing phreg using programming step for the type Voluntary Resignation/ Turnover;

PROC PHREG DATA=project.empdata_cumu;

   MODEL YearsAtCompany*Type(0, 1, 3, 4) = Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager TimeIntercat1 TimeIntercat2 TimeIntercat3 TimeInteract4
		  CumBonus_var/TIES=DISCRETE;
	    TimeIntercat1=YearsAtCompany*TotalWorkingYears;
		TimeIntercat2=YearsAtCompany*YearsInCurrentRole;
		TimeIntercat3=YearsAtCompany*NumCompaniesWorked;
		TimeInteract4=YearsAtCompany*YearsWithCurrManager;
		title PHreg model for Voluntary Resignation/ Turnover;
	      Array CumBonus(*)  cum1 - cum40; 
	      if yearsatcompany>0 then
 	      CumBonus_var=cumBonus[Yearsatcompany]; 
 	      else 
 	      CumBonus_var=0;
RUN;

*Implementing phreg using programming step for the type InVoluntary Resignation;

PROC PHREG DATA=project.empdata_cumu;

   MODEL YearsAtCompany*Type(0, 1, 2, 4) = Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
		  TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager TimeIntercat1 TimeIntercat2 TimeIntercat3 TimeInteract4
	      CumBonus_var/TIES=DISCRETE;
	    TimeIntercat1=YearsAtCompany*TotalWorkingYears;
		TimeIntercat2=YearsAtCompany*YearsInCurrentRole;
		TimeIntercat3=YearsAtCompany*NumCompaniesWorked;
		TimeInteract4=YearsAtCompany*YearsWithCurrManager;
		title PHreg model for InVoluntary Resignation/turnover;
	      Array CumBonus(*)  cum1 - cum40; 
	      if yearsatcompany>0 then
 	      CumBonus_var=cumBonus[Yearsatcompany]; 
 	      else 
 	      CumBonus_var=0;
RUN;

*Implementing phreg using programming step for the type Termination;

PROC PHREG DATA=project.empdata_cumu;

   MODEL YearsAtCompany*Type(0, 1, 2, 3) = Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager TimeIntercat1 TimeIntercat2 TimeIntercat3 TimeInteract4
	      CumBonus_var/TIES=DISCRETE;
	    TimeIntercat1=YearsAtCompany*TotalWorkingYears;
		TimeIntercat2=YearsAtCompany*YearsInCurrentRole;
		TimeIntercat3=YearsAtCompany*NumCompaniesWorked;
		TimeInteract4=YearsAtCompany*YearsWithCurrManager;
		title PHreg model for termination/turnover;
	      Array CumBonus(*)  cum1 - cum40; 
	      if yearsatcompany>0 then
 	      CumBonus_var=cumBonus[Yearsatcompany]; 
 	      else 
 	      CumBonus_var=0;
RUN;

/* comparison */
DATA LogRatioTest_PHregTime;
	Nested=1128.682;
	Retirement=97.732;
	Voluntary_Resignation=481.54;
	InVoluntary_Resignation=352.866;
	Termination=273.732;
	Total=Retirement+ Voluntary_Resignation+InVoluntary_Resignation+Termination;
	Diff= -Nested + Total;
	P_value= probchi(Diff,102);
RUN;

PROC PRINT DATA=LogRatioTest_PHregTime;
	FORMAT P_Value 5.3;
	title total nested vs individual hypothesis;
RUN;



/* *checking involuntry resignation and job termination; */
PROC PHREG DATA=project.empdata_cumu;
   MODEL YearsAtCompany*Type(0, 1, 2) = Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager TimeIntercat1 TimeIntercat2 TimeIntercat3 TimeInteract4
	      CumBonus_var/TIES=EFRON;
	    TimeIntercat1=YearsAtCompany*TotalWorkingYears;
		TimeIntercat2=YearsAtCompany*YearsInCurrentRole;
		TimeIntercat3=YearsAtCompany*NumCompaniesWorked;
		TimeInteract4=YearsAtCompany*YearsWithCurrManager;;
		title PHreg model for involuntry resignation and job termination/turnover;
	      Array CumBonus(*)  cum1 - cum40; 
	      if yearsatcompany>0 then
 	      CumBonus_var=cumBonus[Yearsatcompany]; 
 	      else 
 	      CumBonus_var=0;
RUN;

/* checking involuntary resignation and job termination */

DATA LogRatioTest_PHreg_IVJT;
	Nested=826.083;
    InVoluntary_Resignation=788.617;
	Termination=458.263;
	Total=InVoluntary_Resignation+Termination;
	Diff= -Nested + Total;
	P_value= 1-probchi(Diff, 30);
RUN;

/* checking involuntary  resignation and job termination */

PROC PRINT DATA=LogRatioTest_PHreg_IVJT;
	FORMAT P_Value 5.3;
	title nested(involuntry, termination) vs individual hypothesis;
RUN;


*d - Martingale Residuals to verify PH;
ODS GRAPHICS ON;
PROC PHREG DATA=project.empdata2;

   MODEL YearsAtCompany*Turn_dum(0) = Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager/TIES=efron;
		    
        ASSESS PH / RESAMPLE;
        RUN;
*ODS GRAPHICS OFF;

*d;
*Implementing phreg using programming step for all the turnover types in one model;
 DATA Project.empdata2;
	SET project.empdata_cumu;
	ARRAY bonus_(*) bonus_1-bonus_40;
	ARRAY cum(*) cum1-cum40;
	array bonus character;

	do i=1 to dim(bonus);

		if bonus(i)="NA" then
			bonus(i)="0";
	end;

	cum1=bonus_1;

	DO i=2 TO 40;
		cum(i)=cum(i-1)+bonus_(i);
	END;
run;
PROC PHREG DATA=project.empdata_cumu;
   MODEL YearsAtCompany*Type(0) = Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager TimeInteract1 TimeInteract2 
	     CumBonus_var/TIES=efron;
	    TimeInteract1=YearsAtCompany*Yearswithcurrmanager;
		TimeIntercat2=YearsAtCompany*YearsInCurrentRole;
		
		
		DATA Project.empdata2;
	SET project.empdata_cumu;
	ARRAY bonus_(*) bonus_1-bonus_40;
	ARRAY cum(*) cum1-cum40;
	array bonus character;

	do i=1 to dim(bonus);

		if bonus(i)="NA" then
			bonus(i)="0";
	end;

	cum1=bonus_1;

	DO i=2 TO 40;
		cum(i)=cum(i-1)+bonus_(i);
	END;
run;

*Implementing phreg using programming step for all the turnover types in one model;

PROC PHREG DATA=project.empdata_cumu;
   MODEL YearsAtCompany*Type(0) = Age BusinessTravel_dum DailyRate Department_dum DistanceFromHome	
	      Education EducationField_dum EmployeeCount EmployeeNumber EnvironmentSatisfaction Gender_dum	
	      HourlyRate JobInvolvement JobLevel JobRole_dum JobSatisfaction MaritalStatus_dum
	      MonthlyIncome MonthlyRate NumCompaniesWorked OverTime_dum PercentSalaryHike	
	      PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears	
	      TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion	
	      YearsWithCurrManager TimeIntercatWorkingYears TimeIntercatCurrentRole TimeIntercatNumCompaniesWorked 
	      CumBonus_var/TIES=efron;
	    TimeIntercatWorkingYears=YearsAtCompany*TotalWorkingYears;
		TimeIntercatCurrentRole=YearsAtCompany*YearsInCurrentRole;
		TimeIntercatNumCompaniesWorked=YearsAtCompany*NumCompaniesWorked;
		title PHreg model for all types/ turnover;
	      Array CumBonus(*)  cum1 - cum40; 
	      if yearsatcompany>1 then
 	      CumBonus_var=cumBonus[Yearsatcompany-1]; 
 	      else 
 	      CumBonus_var=bonus1;
RUN;




