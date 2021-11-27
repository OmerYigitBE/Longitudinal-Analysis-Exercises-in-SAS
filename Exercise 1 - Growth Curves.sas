/* EXERCISE 1: GROWTH CURVES*/

libname longit  '/home/u43398024/Longitudinal Exercises/';

data growthgv;
	set longit.growthgv;
run;

proc print data=growthgv;
run;

* 1. Full multivariate model;

proc mixed data=growthgv method=ml;
class child group age;
model height = group age group*age / solution;
repeated age / type=un subject=child r rcorr;
run;

proc mixed data=growthgv method=ml;
class child group age;
model height = group*age / noint solution;
repeated age / type=un subject=child r rcorr;
run;

* 2. Simplified mean structure;

data test;
set growthgv;
age = age-6;
ageclss = age;
run;

proc print data=test;
run;

proc mixed data=test method=ml ;
class child group ageclss ;
model height = group group*age / solution noint;
repeated ageclss / type=un subject=child r rcorr;
run;

* 3. F-test for interaction;

proc mixed data = test method=ml;
class child group ageclss ;
model height = group group*age / solution noint;
repeated ageclss / type=un subject=child r rcorr;
contrast ’interactie’ group*age 1 -1 0, group*age 1 0 -1;
run;

proc mixed data = test method=ml;
class child group ageclss ;
model height = group age group*age / solution;
repeated ageclss / type=un subject=child r rcorr;
run;

* Likelihood ratio test, using ML;

proc mixed data = test method=ml;
class child group ageclss ;
model height = group age / solution;
repeated ageclss / type=un subject=child r rcorr;
run;

* 4. Estimation of pairwise differences of average slopes;

proc mixed data = test method=ml;
class child group ageclss ;
model height = group group*age / solution noint;
repeated ageclss / type=un subject=child r rcorr;
estimate ’small-medium’ group*age 1 -1 0;
estimate ’small-tall’ group*age 1 0 -1;
estimate ’medium-tall’ group*age 0 1 -1;
run;

* 5. Fitting the linear mixed model;

proc mixed data = test;
class child group;
model height = group group*age / noint solution;
random intercept age / type = un subject=child g;
run;

* 6. Comparison with model with unstructured covariance matrix;

proc mixed data = test ic;
class child group;
model height = group group*age / noint solution;
random intercept age / type = un subject=child g;
run;

proc mixed data = test ic;
class child group ageclss ;
model height = group group*age / solution noint;
repeated ageclss / type=un subject=child r rcorr;
run;

* 7. Robust inference;

proc mixed data = test empirical;
class child group;
model height = group group*age / noint solution;
random intercept age / type = un subject=child g;
run;