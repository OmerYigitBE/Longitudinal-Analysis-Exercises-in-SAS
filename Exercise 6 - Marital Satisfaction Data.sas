/* EXERCISE 6: MARITAL SATISFACTION DATA*/

libname longit  '/home/u43398024/Longitudinal Exercises/';

* 1.1. Splitting in separate datasets for father and mother;

data father;
set marital;
where parent=0;
run;

data mother;
set marital;
where parent=1;
run;

* 1.2. Full model with unstructured mean and covariance structure (for both datasets);
proc mixed data=father method=ml;
class year status;
model sat = year child birth educ marr income status / solution noint;
repeated year / type=un subject=idnr;
run;

proc mixed data=mother method=ml;
class year status;
model sat = year child birth educ marr income status / solution noint;
repeated year / type=un subject=idnr;
run;

* 1.3. Final model after reducing mean structure (for both datasets);

proc mixed data=father method=ml;
class year status;
model sat = year / solution noint;
repeated year / type=un subject=idnr;
run;

proc mixed data=mother method=ml;
class year status;
model sat = year / solution noint;
repeated year / type=un subject=idnr;
run;

* 1.4. Reducing the covariance structure (for both datasets);

proc mixed data=father method=ml;
class year status;
model sat = year / solution noint;
repeated year / type=un(2) subject=idnr;
run;

proc mixed data=father method=ml;
class year status;
model sat = year / solution noint;
repeated year / type=arh(1) subject=idnr;
run;

proc mixed data=father method=ml;
class year status;
model sat = year / solution noint;
repeated year / type=csh subject=idnr;
run;

proc mixed data=father method=ml;
class year status;
model sat = year / solution noint;
repeated year / type=toeph subject=idnr;
run;

proc mixed data=father method=ml;
class year status;
model sat = year / solution noint;
repeated year / type=cs subject=idnr;
run;

proc mixed data=mother method=ml;
class year status;
model sat = year / solution noint;
repeated year / type=un(2) subject=idnr;
run;

proc mixed data=mother method=ml;
class year status;
model sat = year / solution noint;
repeated year / type=arh(1) subject=idnr;
run;

proc mixed data=mother method=ml;
class year status;
model sat = year / solution noint;
repeated year / type=csh subject=idnr;
run;

proc mixed data=mother method=ml;
class year status;
model sat = year / solution noint;
repeated year / type=toeph subject=idnr;
run;

proc mixed data=mother method=ml;
class year status;
model sat = year / solution noint;
repeated year / type=cs subject=idnr;
run;

proc mixed data=mother method=ml;
class year status;
model sat = year / solution noint;
repeated year / type=ar(1) subject=idnr;
run;

proc mixed data=mother method=ml;
class year status;
model sat = year / solution noint;
repeated year / type=simple subject=idnr;
run;

* 1.5. Modelling longitudinal data of both parents simultaneously;

proc mixed data=marital method=ml;
class year parent;
model sat = parent year parent*year / solution noint;
repeated parent*year / type=un subject=famnr r;
run;

proc mixed data=marital method=ml;
class year parent;
model sat = parent year parent*year / solution noint;
repeated parent year / type=un@un subject=famnr r;
run;

proc mixed data=marital method=ml;
class year parent;
model sat = parent year parent*year / solution noint;
repeated parent year / type=un@ar(1) subject=famnr r;
run;

proc mixed data=marital method=ml;
class year parent;
model sat = parent year parent*year / solution noint;
repeated parent year / type=un@cs subject=famnr r;
run;

* 2.1. Data Management;

proc import out = longit.maritcc datafile='/home/u43398024/Longitudinal Exercises/marsatCC.xls' 
            dbms=xls replace;
     getnames=yes;
run;

data maritcc;
set longit.maritcc;
YEARCLS = year;
PARENTCLS = parent;
YEARSMAR90 = 1990 - (1900+marr);
AGEBASE = 1990 - (1900+birth);
run;

proc print data=maritcc;
run;

* 2.2. Separate Analysis for Males & Females;

proc sort data=maritcc; by parent; run;

proc genmod data=maritcc descending;
title "Full Model by Parent: CC - GEE: Type=UN";
class yearcls idnr;
model bsat = yearsmar90 yearsmar90*year yearsmar90*agebase yearsmar90*educ
yearsmar90*agebase*year yearsmar90*educ*year yearsmar90*educ*agebase /
dist=binomial;
repeated subject=idnr / within=yearcls type=un modelse;
by parent;
run;

* males;
data males;
set maritcc;
where parent=0;
run;

proc genmod data=males descending;
title "Reduced Model for Males: CC - GEE: Type=UN";
class yearcls idnr;
model bsat = / dist=binomial;
repeated subject=idnr / within=yearcls type=un modelse;
run;

* females;
data females;
set maritcc;
where parent=1;
run;

proc genmod data=females descending;
title "Reduced Model for Females: CC - GEE: Type=UN";
class yearcls idnr;
model bsat = yearsmar90 yearsmar90*year yearsmar90*agebase yearsmar90*educ
yearsmar90*agebase*year yearsmar90*educ*agebase / noint dist=binomial ;
repeated subject=idnr / within=yearcls type=un modelse;
run;

* 2.3. Joint Analysis;

* full model;
proc genmod data=maritcc descending;
title "CC - GEE: Full Model: Type=UN";
class famnr yearcls parentcls;
model bsat = parent*yearsmar90 parent*yearsmar90*year
parent*yearsmar90*agebase parent*yearsmar90*educ
parent*yearsmar90*agebase*year parent*yearsmar90*educ*year
parent*yearsmar90*educ*agebase / dist=binomial;
repeated subject=famnr / within=yearcls(parentcls) type=un modelse corrw;
run;

* reduced model;
proc genmod data=maritcc descending;
title "CC - GEE: Reduced Model: Type=UN";
class famnr yearcls parentcls;
model bsat = parent*yearsmar90 parent*yearsmar90*year
parent*yearsmar90*agebase parent*yearsmar90*educ
parent*yearsmar90*agebase*year / dist=binomial ;
repeated subject=famnr / within=yearcls(parentcls) type=un modelse corrw;
run;

* 3.1. Data Management;

proc import out = longit.marsat datafile='/home/u43398024/Longitudinal Exercises/marsatIC.xls' 
            dbms=xls replace;
     getnames=yes;
run;

data dat;
set longit.marsat (drop = SAT PCOM BPCOM DCOM BDCOM birth educ status income);
yearsmarr = year - (1900 + marr);
drop marr;
run;

proc sort data = dat;
by idnr year famnr;
run;

proc print data=dat;
run;

* 3.2. Procedure GLIMMIX - nested random effects;

proc glimmix data = dat method = RSPL NOCLPRINT NOITPRINT;
title "Nested RE - PQL";
class famnr parent year;
model bsat(event = "1") = year parent parent*year child yearsmarr/ dist = binary solution;
random intercept / subject = famnr;
random intercept / subject = parent(famnr);
run;

proc glimmix data = dat method = RMPL NOCLPRINT NOITPRINT;
title "Nested RE - MQL";
class famnr parent year;
model bsat(event = "1") = year parent parent*year child yearsmarr/ dist = binary solution;
random intercept / subject = famnr;
random intercept / subject = parent(famnr);
run;

* 3.3. Procedure NLMIXED - nested random effects;

proc nlmixed data = dat points = 10;
	title "Nested RE - Adaptive GH (q = 10)";
	parms b0 = 2.8528 b1 = -0.5461 b2 = 0.3003 b3 = 0.1212 b4 = -0.02857
		  b5 = -0.2657 b6 = 0.06638 b7 = -0.03084
		  V1 = 2.3957;
	eta = b0 + b1 * (year = 1990) + b2 * (year = 1995) +
		  b3 * (parent = 0) + b4 * (parent = 0) * (year = 1990) +
		  b5 * (parent = 0) * (year = 1995) + b6 * child +
		  b7 * yearsmarr +
		  g1 * (parent = 0) + g2 * (parent = 1);
	expeta = exp(eta);
	p = expeta / (1 + expeta);
	model bsat ~ binary(p);
	random g1 g2 ~ normal([0, 0], [V1 + V2,
								   V2, V1 + V2]) subject = famnr;
run;

* 3.4. Test for the parent random effect;

proc nlmixed data = dat points = 10;
title "Adaptive GH (q = 10) - No parent RE";
parms b0 = 2.8528 b1 = -0.5461 b2 = 0.3003 b3 = 0.1212 b4 = -0.02857
b5 = -0.2657 b6 = 0.06638 b7 = -0.03084
V1 = 2.3957;
eta = b0 + b1 * (year = 1990) + b2 * (year = 1995) +
b3 * (parent = 0) + b4 * (parent = 0) * (year = 1990) +
b5 * (parent = 0) * (year = 1995) + b6 * child +
b7 * yearsmarr + g1;
expeta = exp(eta);
p = expeta / (1 + expeta);
model bsat ~ binary(p);
random g1 ~ normal(0, V1**2) subject = famnr;
run;

data LRT;
logL0 = -1203.2/2;
logL1 = -1199.6/2;
LRT = -2 * (logL0 - logL1);
df = 1;
pval = 1 - probchi(LRT, df);
run;

proc print data = LRT;
run;

* 3.5. Procedure GLIMMIX for PQL and MQL - random intercepts;

proc glimmix data = dat method = RSPL NOCLPRINT NOITPRINT IC=PQ or;
title "PQL (REML)";
class famnr parent year;
nloptions maxit = 50 technique = newrap;
model bsat(event = "1") = year parent parent*year child
yearsmarr/ dist = binary solution;
random intercept / subject = famnr;
run;

proc glimmix data = dat method = MSPL NOCLPRINT NOITPRINT IC=PQ or;
title "PQL (ML)";
class famnr parent year;
nloptions maxit = 50 technique = newrap;
model bsat(event = "1") = year parent parent*year child
yearsmarr/ dist = binary solution;
random intercept / subject = famnr;
run;

proc glimmix data = dat method = RMPL NOCLPRINT NOITPRINT;
title "MQL (REML)";
class famnr parent year;
nloptions maxit = 50 technique = newrap;
model bsat(event = "1") = year parent parent*year child
yearsmarr/ dist = binary solution;
random intercept / subject = famnr;
run;

proc glimmix data = dat method = MMPL NOCLPRINT NOITPRINT;
title "MQL (ML)";
class famnr parent year;
nloptions maxit = 50 technique = newrap;
model bsat(event = "1") = year parent parent*year child
yearsmarr/ dist = binary solution;
random intercept / subject = famnr;
run;

* 3.6. Procedure NLMIXED for Gaussian quadrature - random intercepts;

proc nlmixed data = dat noad points = 20;
title "GH (q = 20)";
parms b0 = 2.8528 b1 = -0.5465 b2 = 0.3001 b3 = 0.1213 b4 = -0.02867
b5 = -0.2656 b6 = 0.06617 b7 = -0.03079
V1 = 2.3662;
eta = b0 + b1 * (year = 1990) + b2 * (year = 1995) +
b3 * (parent = 0) + b4 * (parent = 0) * (year = 1990) +
b5 * (parent = 0) * (year = 1995) + b6 * child +
b7 * yearsmarr + g1;
expeta = exp(eta);
p = expeta / (1 + expeta);
model bsat ~ binary(p);
random g1 ~ normal(0, V1**2) subject = famnr;
run;

* 3.7. Procedure NLMIXED for adaptive Gaussian quadrature - random intercepts;

proc nlmixed data = dat points = 20;
title "Adaptive GH (q = 20)";
parms b0 = 2.8528 b1 = -0.5465 b2 = 0.3001 b3 = 0.1213 b4 = -0.02867
b5 = -0.2656 b6 = 0.06617 b7 = -0.03079
V1 = 2.3662;
eta = b0 + b1 * (year = 1990) + b2 * (year = 1995) +
b3 * (parent = 0) + b4 * (parent = 0) * (year = 1990) +
b5 * (parent = 0) * (year = 1995) + b6 * child +
b7 * yearsmarr + g1;
expeta = exp(eta);
p = expeta / (1 + expeta);
model bsat ~ binary(p);
random g1 ~ normal(0, V1**2) subject = famnr;
estimate "V1^2" V1*V1;
run;

* 3.8. Procedure NLMIXED for Laplace approximation;

proc nlmixed data = dat points = 1;
title "Laplace approximation";
parms b0 = 2.8528 b1 = -0.5465 b2 = 0.3001 b3 = 0.1213 b4 = -0.02867
b5 = -0.2656 b6 = 0.06617 b7 = -0.03079
V1 = 2.3662;
eta = b0 + b1 * (year = 1990) + b2 * (year = 1995) +
b3 * (parent = 0) + b4 * (parent = 0) * (year = 1990) +
b5 * (parent = 0) * (year = 1995) + b6 * child +
b7 * yearsmarr + g1;
expeta = exp(eta);
p = expeta / (1 + expeta);
model bsat ~ binary(p);
random g1 ~ normal(0, V1**2) subject = famnr;
estimate "V1^2" V1*V1;
run;

* 3.9. Marginal evolutions;
proc univariate data=dat;
var child yearsmarr;
run;

data Simulate;
do parent=0 to 1 by 1;
	do subject=1 to 1000 by 1;
		b1=rannor(-1);
		b1=2.1366*b1;
		do t=1 to 3 by 1;
			output;
		end;
	end;
end;
run;

proc sort data=Simulate;
by t parent;
run;

data Simulate;
set Simulate;
child = 2;
marr = 22;
year90 = 0;
year95 = 0;
parent0 = 0;
if t=1 then year90=1;
if t=2 then year95=1;
if parent=0 then parent0=1;
if parent=0 then
	y=1/(1+exp(-(4.3383+b1-0.7549*year90+0.4583*year95+0.1511*parent0
		   -0.02017*parent0*year90-0.3720*parent0*year95+
		   .1531*child-0.04538*marr)));
else
	y=1/(1+exp(-(4.3383+b1-0.7549*year90+0.4583*year95+
		   0.1531*child-0.04538*marr)));
run;

proc means data=Simulate;
var y;
by t parent;
output out=out;
run;

proc gplot data=out;
plot y*t=parent / haxis=axis1 vaxis=axis2 legend=legend1;
axis1 label=(h=2 "Time Points") value=(h=1.5) minor=none;
axis2 label=(h=2 A=90 "P(Y=1)") value=(h=1.5) order=(0.84 to 0.95 by 0.02) minor=none;
legend1 label=(h=1.5 "Parent: ") value=(h=1.5 "Husband" "Wife");
title h=2.5 "Marginal average evolutions (GLMM)";
symbol1 c=black i=join w=5 l=1 mode=include;
symbol2 c=black i=join w=5 l=2 mode=include;
where _stat_="MEAN";
run;
quit;
run;

* 3.10. Evolutions for the median individual;

proc univariate data=dat;
var child yearsmarr;
run;

data MedianPlot;
do parent=0 to 1 by 1;
b1=0;
b1=2.1366*b1;
do t=1 to 3 by 1;
output;
end;
end;
run;

proc sort data=MedianPlot;
by t parent;
run;

data MedianPlot;
set MedianPlot;
child = 2;
marr = 22;
year90 = 0;
year95 = 0;
parent0 = 0;
if t=1 then year90=1;
if t=2 then year95=1;
if parent=0 then parent0=1;
if parent=0 then
y=1/(1+exp(-(4.3383+b1-0.7549*year90+0.4583*year95+0.1511*parent0
		-0.02017*parent0*year90-0.3720*parent0*year95+
		0.1531*child-0.04538*marr)));
else
	y=1/(1+exp(-(4.3383+b1-0.7549*year90+0.4583*year95+
		   0.1531*child-0.04538*marr)));
run;

proc gplot data=MedianPlot;
plot y*t=parent / haxis=axis1 vaxis=axis2 legend=legend1;
axis1 label=(h=2 "Time Points") value=(h=1.5) minor=none;
axis2 label=(h=2 A=90 "P(Y=1)") value=(h=1.5) order=(0.92 to 1.0 by 0.02) minor=none;
legend1 label=(h=1.5 "Parent: ") value=(h=1.5 "Husband" "Wife");
title h=2.5 "Evolutions for the median parent";
symbol1 c=black i=join w=5 l=1 mode=include;
symbol2 c=black i=join w=5 l=2 mode=include;
run;
quit;
run;

* 3.11. Test for the parent effect;
proc nlmixed data = dat points = 20;
title "Adaptive GH (q = 20) - No parent effect";
parms b0 = 2.8528 b1 = -0.5465 b2 = 0.3001 b3 = 0.1213 b4 = -0.02867
V1 = 2.3662;
eta = b0 + b1 * (year = 1990) + b2 * (year = 1995) + b3 * child +
b4 * yearsmarr + g1;
expeta = exp(eta);
p = expeta / (1 + expeta);
model bsat ~ binary(p);
random g1 ~ normal(0, V1**2) subject = famnr;
run;

data LRT;
logL0 = -1203.8/2;
logL1 = -1203.0/2;
LRT = -2 * (logL0 - logL1);
df = 3;
pval = 1 - probchi(LRT, df);
run;

proc print data = LRT;
run;

* 4.1 Data Management;

/* IC */
PROC IMPORT OUT= WORK.MSDIC
DATAFILE= "C:\QMSS Workshop\Data\MSD\data3_IC.xls"
DBMS=EXCEL REPLACE;
SHEET="Sheet1$";
GETNAMES=YES;
MIXED=NO;
SCANTEXT=YES;
USEDATE=YES;
SCANTIME=YES;
RUN;

data msdic;
set msdic;
yearcls=year;
parentcls=parent;
drop pcom bpcom dcom bdcom;
run;

proc print data=msdic;
run;

/* CC */
PROC IMPORT OUT= WORK.MSDCC
DATAFILE= "C:\QMSS Workshop\Data\MSD\data3_CC.xls"
DBMS=EXCEL REPLACE;
SHEET="Sheet1$";
GETNAMES=YES;
MIXED=NO;
SCANTEXT=YES;
USEDATE=YES;
SCANTIME=YES;
RUN;

data msdcc;
set msdcc;
drop pcom bpcom dcom bdcom;
run;

proc print data=msdcc;
run;

* 4.2. Exploring Missing Data Patterns;

data msd1;
set msdic;
if year=1990 then sat1=sat;
if year>1990 then delete;
keep famnr parent sat1;
run;

data msd2;
set msdic;
if year=1995 then sat2=sat;
if year>1995|year<1995 then delete;
keep famnr parent sat2;
run;

data msd3;
set msdic;
if year=2000 then sat3=sat;
if year<2000 then delete;
keep famnr parent sat3;
run;

data misexp;
merge msd1 msd2 msd3;
by famnr parent;
run;

proc mi data=misexp;
var sat1 sat2 sat3;
run;

* 4.3. Continuous Response: Procedure MIXED: CC;
proc mixed data=msdcc method=ml;
title "CONT MIXED, CC";
class famnr idnr year status parent;
model sat = year parent year*parent / s noint;
repeated parent year / type=un@CS subject=famnr r rcorr;
run;

* 4.4. Continuous Response: Procedure MIXED: Direct Likelihood;
proc mixed data=msdic method=ml;
title "CONT MIXED Direct Likelihood";
class famnr idnr year status parent;
model sat = year parent year*parent / s noint;
repeated parent year / type=un@cs subject=famnr r rcorr;
run;

* 4.5. Continuous Response: Procedure MIXED: Multiple Imputation;
proc MI data = msdic out = msdiccomp NOPRINT;
mcmc initial=em (bootstrap = 0.85)
prior = JEFFREYS
chain = multiple;
run;

/* Transforming the data for Mixed model (Creating dummies) */
data msdiccomp;
title "CONT MIXED MULTIPLE IMPUTATION";
set msdiccomp;
year1990 = 0;
year1995 = 0;
year2000 = 0;
if year = 1990 then year1990 = 1;
if year = 1995 then year1995 = 1;
if year = 2000 then year2000 = 1;
father = 0;
mother = 0;
if Parent = 0 then father = 1;
if Parent = 1 then mother = 1;
father1990 = 0;
father1995 = 0;
father2000 = 0;
mother1990 = 0;
mother1995 = 0;
mother2000 = 0;
if (father = 1) and (year = 1990) then father1990 = 1;
if (father = 1) and (year = 1995) then father1995 = 1;
if (father = 1) and (year = 2000) then father2000 = 1;
if (mother = 1) and (year = 1990) then mother1990 = 1;
if (mother = 1) and (year = 1995) then mother1995 = 1;
if (mother = 1) and (year = 2000) then mother2000 = 1;
run;

/* Analysing 5 Completed Datasets */
proc mixed data=msdiccomp method = ml asycov covtest;
title2 "MIXED MODEL ANALYSIS PER IMPUTATION";
class famnr year parent ;
model sat = year1990 year1995 year2000 father father1990 father1995 / s noint covb;
repeated year parent / subject=famnr type=un@cs rcorr;
ods output solutionf = solution covb = covb covparms = covparms asycov = asycov;
by _imputation_;
data solution0;
set solution;
data covb0;
set covb;
data covparms0;
set covparms;
if CovParm="YEAR UN(1,1)" then effect = "YEARUN11";
if CovParm=" UN(2,1)" then effect = "YEARUN21";
if CovParm=" UN(2,2)" then effect = "YEARUN22";
if CovParm=" UN(3,1)" then effect = "YEARUN31";
if CovParm=" UN(3,2)" then effect = "YEARUN32";
if CovParm=" UN(3,3)" then effect = "YEARUN33";
if CovParm="PARENT Corr" then effect = "PARENTCORR" ;
drop covparm;
data asycov0;
set asycov;
Col1=CovP1;
Col2=CovP2;
Col3=CovP3;
Col4=CovP4;
Col5=CovP5;
Col6=CovP6;
Col7=CovP7;
if CovParm="YEAR UN(1,1)" then effect = "YEARUN11";
if CovParm=" UN(2,1)" then effect = "YEARUN21";
if CovParm=" UN(2,2)" then effect = "YEARUN22";
if CovParm=" UN(3,1)" then effect = "YEARUN31";
if CovParm=" UN(3,2)" then effect = "YEARUN32";
if CovParm=" UN(3,3)" then effect = "YEARUN33";
if CovParm="PARENT Corr" then effect = "PARENTCORR" ;
drop CovP1 CovP2 CovP3 CovP4 CovP5 CovP6 CovP7 covparm;
run;

/* Combining 5 Separate Analyses (mean structure) */
proc mianalyze parms=solution0 covb(effectvar=rowcol)=covb0;
title2 "COMBINING 5 MIXED MODEL ANALYSES (MEAN STRUCTURE)";
modeleffects year1990 year1995 year2000 father father1990 father1995;
run;
/* Combining 5 Separate Analyses (covariance structure) */
proc mianalyze parms=covparms0 covb(effectvar=rowcol)=asycov0;
title2 "COMBINING 5 MIXED MODEL ANALYSES (COVARIANCE STRUCTURE)";
modeleffects YEARUN11 YEARUN21 YEARUN22 YEARUN31 YEARUN32 YEARUN33 PARENTCO;
run;

* 4.6. Binary Response: Procedure GEE: CC;
proc genmod data=msdcc descending;
title "BIN GEE CC";
class famnr year parent;
model bsat = parent year birth parent*year parent*birth birth*year parent*birth*year
/dist=binomial type3;
repeated subject=famnr / within=year(parent) type=un modelse corrw;
run;

* 4.7. Binary Response: Procedure GEE: WGEE;

/* Use the WGEE MACRO FOR CREATING VARIABLES "DROPOUT" AND "PREV" */
%dropout(data=msdic,id=famnr,time=year,response=bsat,out=msdicwgee);

proc print data=msdicwgee;
title ’BIN WGEE’;
run;

proc genmod data=msdicwgee descending;
title ’BIN WGEE’;
title2 ’Dropout Model’;
class famnr year status prev;
model dropout = prev parent year birth/ pred dist=b;
ods output obstats=pred;
ods listing exclude obstats;
run;

data pred;
set pred;
keep observation pred;
run;

data msdicwgee1;
merge pred msdicwgee;
run;

/* Use the WGEE MACRO TO CREATE THE WEIGHTING VARIABLE */
%dropwgt(data=msdicwgee1,id=famnr,time=year,pred=pred,dropout=dropout,out=msdicwgee2);

proc print data=msdicwgee2;
var famnr year bsat dropout prev pred wi;
run;

/* WGEE model */
proc genmod data=msdicwgee2 descending;
title ’BIN WGEE’;
scwgt wi;
class famnr year parent ;
model bsat = parent year birth parent*year parent*birth birth*year parent*birth*year
/dist=binomial type3;
repeated subject=famnr / within=year(parent) type=un modelse corrw;
run;

* 4.8. Binary Response: Procedure GEE: Available Cases;
proc genmod data=msdic descending;
title ’BIN GEE Available Cases’;
class famnr year parent ;
model bsat = parent year birth parent*year parent*birth year*birth parent*year*birth
/ dist=binomial type3;
repeated subject=famnr / within=year(parent) type=un modelse corrw;
run;

* 4.9. Binary Response: Procedure GEE: Multiple Imputation;
proc MI data = msdic out = msdiccomp NOPRINT;
mcmc initial=em (bootstrap = 0.85)
prior = JEFFREYS
chain = multiple;
run;

/* Transforming the data for MI GEE (Creating dummies) */
data msdiccomp;
title1 ’BIN GEE MULTIPLE IMPUTATION’;
set msdiccomp;
if SAT <= 4 then BSAT = 0;
if SAT > 4 then BSAT = 1;
year1990 = 0;
year1995 = 0;
year2000 = 0;
if year = 1990 then year1990 = 1;
if year = 1995 then year1995 = 1;
if year = 2000 then year2000 = 1;
father = 0;
mother = 0;
if Parent = 0 then father = 1;
if Parent = 1 then mother = 1;
father1990 = 0;
father1995 = 0;
father2000 = 0;
mother1990 = 0;
mother1995 = 0;
mother2000 = 0;
if (father = 1) and (year = 1990) then father1990 = 1;
if (father = 1) and (year = 1995) then father1995 = 1;
if (father = 1) and (year = 2000) then father2000 = 1;
if (mother = 1) and (year = 1990) then mother1990 = 1;
if (mother = 1) and (year = 1995) then mother1995 = 1;
if (mother = 1) and (year = 2000) then mother2000 = 1;
fatherbirth = 0;
motherbirth = 0;
if father = 1 then fatherbirth = birth;
if mother = 1 then motherbirth = birth;
birth1990 = 0;
birth1995 = 0;
birth2000 = 0;
if year = 1990 then birth1990 = birth;
if year = 1995 then birth1995 = birth;
if year = 2000 then birth2000 = birth;
father1990birth = 0;
father1995birth = 0;
father2000birth = 0;
mother1990birth = 0;
mother1995birth = 0;
mother2000birth = 0;
if (father = 1) and (year = 1990) then father1990birth = birth;
if (father = 1) and (year = 1995) then father1995birth = birth;
if (father = 1) and (year = 2000) then father2000birth = birth;
if (mother = 1) and (year = 1990) then mother1990birth = birth;
if (mother = 1) and (year = 1995) then mother1995birth = birth;
if (mother = 1) and (year = 2000) then mother2000birth = birth;
run;

/* Analysing 5 Completed Datasets */
proc genmod data=msdiccomp descending;
title2 ’GEE ANALYSIS PER IMPUTATION’;
class famnr year parent ;
model bsat = father year1990 year1995 BIRTH father1990 father1995 fatherbirth
birth1990 birth1995 father1990birth father1995birth
/ dist=binomial type3 covb;
repeated subject=famnr / within=year(parent) type=un modelse corrw;
ods output parameterestimates = gmparms parminfo = gmpinfo covb = gmcovb;
by _imputation_;
data gmparms;
set gmparms;
if parameter in (’Scale’) then delete;
data gmcovb;
set gmcovb;
data gmpinfo;
set gmpinfo;
run;

/* Combining 5 Separate Analyses */
proc mianalyze parms = gmparms covb = gmcovb parminfo = gmpinfo;
title2 ’COMBINING 5 GEE ANALYSES’;
modeleffects intercept father year1990 year1995 BIRTH father1990 father1995 fatherbirth
birth1990 birth1995 father1990birth father1995birth;
run;

* 4.10. Binary Response: Procedure NLMIXED: CC;
proc logistic data=msdcc;
title ’Determining the starting values for the CC GLMM’;
class year parent;
model bsat=parent year birth parent*year parent*birth birth*year parent*birth*year;
run;

proc nlmixed data = msdcc points = 10;
title ’CC GLMM’;
parms b0 = -3 b1 = -1.5 b2 = -0.9 b3 = 0.1 b4 = 0.008
b5 = 1.1 b6 = 3.1 b7 = 0.002 b8 = 0.04 b9 = 0.01 b10 = -0.02 b11 = -0.065
V1 = 2.3662;
eta = b0 + b1 * (year = 1990) + b2 * (year = 1995) + b3 * (parent = 0) + b4 * birth +
b5 * (parent = 0) * (year = 1990) + b6 * (parent = 0) * (year = 1995) +
b7 * (parent = 0) * birth + b8 * birth * (year = 1990) +
b9 * birth * (year = 1995) + b10 * birth * (year = 1990) * (parent = 0) +
b11 * birth * (year = 1995) * (parent = 0) + g1;
expeta = exp(eta);
p = expeta / (1 + expeta);
model bsat ~ binary(p);
random g1 ~ normal(0, V1**2) subject = famnr;
run;

* 4.11. Binary Response: Procedure NLMIXED: Direct Likelihood;
proc nlmixed data = msdic points = 10;
title ’Direct Likelihood GLMM’;
parms b0 = -3 b1 = -1.5 b2 = -0.9 b3 = 0.1 b4 = 0.008
b5 = 1.1 b6 = 3.1 b7 = 0.002 b8 = 0.04 b9 = 0.01 b10 = -0.02 b11 = -0.065
V1 = 2.3662;
eta = b0 + b1 * (year = 1990) + b2 * (year = 1995) + b3 * (parent = 0) + b4 * birth +
b5 * (parent = 0) * (year = 1990) + b6 * (parent = 0) * (year = 1995) +
b7 * (parent = 0) * birth + b8 * birth * (year = 1990) +
b9 * birth * (year = 1995) + b10 * birth * (year = 1990) * (parent = 0) +
b11 * birth * (year = 1995) * (parent = 0) + g1;
expeta = exp(eta);
p = expeta / (1 + expeta);
model bsat ~ binary(p);
random g1 ~ normal(0, V1**2) subject = famnr;
run;