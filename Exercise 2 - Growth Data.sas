/* EXERCISE 2: GROWTH DATA*/

libname longit  '/home/u43398024/Longitudinal Exercises/';

data growthcc;
	set longit.growthcc;
run;

proc print data=growthcc;
run;

************;

proc mixed data=growthcc method=ml;
title ’Growth Data (Complete Cases), Model 6’;
title2 ’Untransformed age’;
class sex idnr;
model measure=sex age*sex / s;
random intercept age / type=un subject=idnr g v vcorr;
run;

proc mixed data=growthcc method=ml;
title ’Growth Data (Complete Cases), Model 6’;
title2 ’Untransformed age - Nobound’;
class sex idnr;
model measure=sex age*sex / s;
parms / nobound;DATASET 2. GROWTH DATA 21
random intercept age / type=un subject=idnr g v vcorr;
run;

data hulp;
set growthcc;
age2=age-11;
age2=age2/3;
run;

proc mixed data=hulp method=ml;
title ’Growth Data (Complete Cases), Model 6’;
title2 ’Transformed age: (age-11)/3’;
class sex idnr;
model measure=sex age2*sex / s;
random intercept age2 / type=un subject=idnr g v vcorr;
run;

proc mixed data=hulp method=ml;
title ’Growth Data (Complete Cases), Model 6’;
title2 ’Transformed age: (age-11)/3 - Nobound’;
class sex idnr;
model measure=sex age2*sex / s;
parms / nobound;
random intercept age2 / type=un subject=idnr g v vcorr;
run;