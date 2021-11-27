/* EXERCISE 3: MASTITIS IN DAIRY CATTLE*/

libname longit  '/home/u43398024/Longitudinal Exercises/';

* Datasets;
*MAST01.SAS7BDAT: Original data;
*MASTCC.SAS7BDAT: Complete cases only;
*MASTLOCF.SAS7BDAT: Last observation carried forward.DATASET 3. MASTITIS IN DAIRY CATTLE 33;
*MASTMEAN.SAS7BDAT: Unconditional mean imputation;
*MASTCOND.SAS7BDAT: Conditional mean imputation;

* 1. Data Manipulation;
* Gauss codes for constructing datasets;

data1='\\home\\u43398024\\Longitudinal Exercises\\mast01';

open handle=^data1 for read;
mast01=readr(handle,1000);
close(handle);

ind=mast01[.,1];
tijd=mast01[.,2];
resp=mast01[.,3];

n=rows(resp)/2;

resp=ind[seqa(1,2,n)]~reshape(resp,n,2);

respcc=packr(resp);

resplocf=resp;
test=missrv(resplocf[.,3],10000);
resplocf[.,3]=(test.*(test./=10000))+(resplocf[.,2].*(test.==10000));

hulp=meanc(respcc[.,3]);
respmean=resp;
respmean[.,3]=missrv(respmean[.,3],hulp);

respcond=resp;
hulp=6.4435+0.6479*inv(0.9115)*(resp[.,2]-6.4435+0.7359);
respcond[.,3]=(test.*(test./=10000))+(hulp.*(test.==10000));

h1=(respcc[.,1].*.ones(2,1));
h2=ones(80,1).*.(0|1);
h3=vec(respcc[.,2 3]’);
let naam=ident ti yy;
dataset='\\home\\u43398024\\Longitudinal Exercises\\mastcc';
create handle=^dataset with ^naam,0,4;
writer(handle,h1~h2~h3);
close(handle);

h1=(resplocf[.,1].*.ones(2,1));
h2=ones(107,1).*.(0|1);
h3=vec(resplocf[.,2 3]’);
let naam=ident ti yy;
dataset='\\home\\u43398024\\Longitudinal Exercises\\mastlocf';
create handle=^dataset with ^naam,0,4;
writer(handle,h1~h2~h3);
close(handle);

h1=(respmean[.,1].*.ones(2,1));
h2=ones(107,1).*.(0|1);
h3=vec(respmean[.,2 3]’);
let naam=ident ti yy;
dataset='\\home\\u43398024\\Longitudinal Exercises\\mastmean';
create handle=^dataset with ^naam,0,4;
writer(handle,h1~h2~h3);
close(handle);

h1=(respcond[.,1].*.ones(2,1));
h2=ones(107,1).*.(0|1);
h3=vec(respcond[.,2 3]’);
let naam=ident ti yy;
dataset='\\home\\u43398024\\Longitudinal Exercises\\mastcond';
create handle=^dataset with ^naam,0,4;
writer(handle,h1~h2~h3);
close(handle);

* 2. Model Formulation;

proc mixed data=mastcc method=ml covtest;
title ’Complete Case Analysis’;
class ti;
model yy=ti / s;
repeated ti / type=csh subject=ident r;
run;

proc mixed data=m.mastcc method=ml covtest;
title ’Complete Case Analysis - Random Effects Version’;
class ti;
model yy=ti / s;
repeated ti / type=un(1) subject=ident r;
random intercept / subject=ident v;
id ident ti;
run;

proc mixed data=m.mastlocf method=ml covtest;
title ’Last Observation Carried Forward’;
class ti;
model yy=ti / s;
repeated ti / type=csh subject=ident r;
run;

proc mixed data=m.mastmean method=ml covtest;
title ’Unconditional Mean Imputation’;
class ti;
model yy=ti / s;
repeated ti / type=csh subject=ident r;
run;

proc mixed data=m.mastcond method=ml covtest;
title ’Conditional Mean Imputation’;
class ti;
model yy=ti / s;
repeated ti / type=csh subject=ident r;
run;

proc mixed data=m.mast01 method=ml covtest;
title ’Available Case/Ignorable Analysis’;
class ti;
model yy=ti / s;
repeated ti / type=csh subject=ident r;
run;

* 3. Intraclass Correlations;

proc mixed data=m.mastcc method=ml covtest;
title ’Complete Case Analysis’;
class ti;
model yy=ti / s;
repeated ti / type=ar(1) subject=ident r;
run;

proc mixed data=m.mastlocf method=ml covtest;
title ’Last Observation Carried Forward’;
class ti;
model yy=ti / s;
repeated ti / type=ar(1) subject=ident r;
run;

proc mixed data=m.mastmean method=ml covtest;
title ’Unconditional Mean Imputation’;
class ti;
model yy=ti / s;
repeated ti / type=ar(1) subject=ident r;
run;

proc mixed data=m.mastcond method=ml covtest;
title ’Conditional Mean Imputation’;
class ti;
model yy=ti / s;
repeated ti / type=ar(1) subject=ident r;
run;

proc mixed data=m.mast01 method=ml covtest;
title ’Available Case/Ignorable Analysis’;
class ti;
model yy=ti / s;
repeated ti / type=ar(1) subject=ident r;
run;

