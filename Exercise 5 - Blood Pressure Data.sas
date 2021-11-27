/* EXERCISE 5: BLOOD PRESSURE DATA*/

libname longit  '/home/u43398024/Longitudinal Exercises/';

* 1. Basic program;

data blood1;
	set longit.blood;
	slope = (time = 'after');
	intsys = (meas = 'systolic');
run;

proc print data=blood1;
title 'Blood Pressure Data';
run;

proc mixed data=blood1 covtest;
title 'Model from Verbeke-Molenberghs';
class time meas id;
model bp = meas*time / noint s;
random intercept intsys slope / type = un(1) subject = id v;
estimate 'trt_sys' meas*time 0 -1 0 1 / cl alpha = 0.05;
estimate 'trt_dia' meas*time -1 0 1 0 / cl alpha = 0.05;
run;

proc mixed data = blood1 covtest;
title "Model 1: 4x4 unstructured covariance matrix";
class time meas id;
model bp = meas*time / noint s;
repeated / type=un subject = id r;
estimate "trt_sys" meas*time 0 -1 0 1 / cl alpha = 0.05;
estimate "trt_dia" meas*time -1 0 1 0 / cl alpha = 0.05;
run;

proc mixed data = blood1 covtest;
title "Model 2: unstructured-by-unstructured";
class time meas id;
model bp = meas*time / noint s;
repeated meas time / type=un@un subject = id r;
estimate "trt_sys" meas*time 0 -1 0 1 / cl alpha = 0.05;
estimate "trt_dia" meas*time -1 0 1 0 / cl alpha = 0.05;
run;

proc mixed data = blood1 covtest;
title "Model 3: unstructured-by-compound symmetry";
class time meas id;
model bp = meas*time / noint s;
repeated meas time / type=un@cs subject = id r;
estimate "trt_sys" meas*time 0 -1 0 1 / cl alpha = 0.05;
estimate "trt_dia" meas*time -1 0 1 0 / cl alpha = 0.05;
run;
