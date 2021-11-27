/* EXERCISE 4: AGE-RELATED MACULAR DEGENERATION STUDY*/

libname longit  '/home/u43398024/Longitudinal Exercises/';

data allarmd;
	set longit.armd;
	keep crf trt visual0 visual4 visual12 visual24 visual52 lesion;
run;

proc print data=allarmd;
run;

* 1. Exploration of Missing Data Mechanisms;

data armd14;
set allarmd;
diff4=visual4-visual0;
diff12=visual12-visual0;
diff24=visual24-visual0;
diff52=visual52-visual0;
bindif4=0; if diff4 <= 0 then bindif4=1;
bindif12=0;if diff12 <= 0 then bindif12=1;
bindif24=0;if diff24 <= 0 then bindif24=1;
bindif52=0;if diff52 <= 0 then bindif52=1;
if diff4=. then bindif4=.;
if diff12=. then bindif12=.;
if diff24=. then bindif24=.;
if diff52=. then bindif52=.;
if trt=1 then treat=1;
if trt=4 then treat=2;
subject=_n_;
run;

proc sort data=armd14;
by treat;
run;

proc mi data=armd14 seed=675938 simple nimpute=0;
title ’standard EM’;
em itprint outem=growthem1;
var diff4 diff12 diff24 diff52;
by treat;
run;

proc mi data=armd14 seed=675938 simple nimpute=0;
title ’EM with CC initial values’;
em itprint outem=growthem1 initial=cc;
var diff4 diff12 diff24 diff52;
by treat;
run;

* 2. Preparation of CC, LOCF and observed data datasets.
*	 Preparation of WGEE analysis;

* We create a longitudinal dataset with four binary outcomes - complete & incomplete;

data armd11;
set allarmd;
diff4=visual4-visual0;
diff12=visual12-visual0;
diff24=visual24-visual0;
diff52=visual52-visual0;
bindif4=0; if diff4 <= 0 then bindif4=1;
bindif12=0;if diff12 <= 0 then bindif12=1;
bindif24=0;if diff24 <= 0 then bindif24=1;
bindif52=0;if diff52 <= 0 then bindif52=1;
if diff4=. then bindif4=.;
if diff12=. then bindif12=.;
if diff24=. then bindif24=.;
if diff52=. then bindif52=.;
if trt=1 then treat=1;
if trt=4 then treat=2;
run;

proc print data=armd11;
run;

* switch from horizontal to vertical dataset;
data armd111;
set armd11;
array x (4) bindif4 bindif12 bindif24 bindif52;
do j=1 to 4;
	bindif=x(j);
	time=j;
	subject=_n_;
	output;
end;
run;

proc print data=armd111;
var subject bindif4 bindif12 bindif24 bindif52 treat time bindif;
run;

%macro cc(data=,id=,time=,response=,out=);
%if %bquote(&data)= %then %let data=&syslast;
proc freq data=&data noprint;
tables &id /out=freqsub;
tables &time / out=freqtime;
run;

proc iml;
use freqsub;
read all var {&id,count};
nsub = nrow(&id);
use freqtime;
read all var {&time,count};
ntime = nrow(&time);
use &data;
read all var {&id,&time,&response};
n = nrow(&response);
complete = j(n,1,1);
ind = 1;
do while (ind <= nsub);
	j = 1;
	do while (j <= ntime);
		if (&response[(ind-1)*ntime+j]=.) then
		complete[(ind-1)*ntime+1:(ind-1)*ntime+ntime]=0;
		j = j+1;
	end;
	ind = ind+1;
end;
create help var {&id &time &response complete};
append;
quit;
data &out;
merge &data help;
if complete=0 then delete;
drop complete;
run;
%mend;

%cc(data=armd111,id=subject,time=time,response=bindif,out=armdcc);

proc print data=armdcc;
run;

%macro locf(data=,id=,time=,response=,out=);
%if %bquote(&data)= %then %let data=&syslast;
proc freq data=&data noprint;
tables &id /out=freqsub;
tables &time / out=freqtime;
run;
proc iml;
use freqsub;
read all var {&id,count};
nsub = nrow(&id);
use freqtime;
read all var {&time,count};
ntime = nrow(&time);
use &data;
read all var {&id,&time,&response};
n = nrow(&response);
locf = &response;
ind = 1;
print nsub;
print ntime;
do while (ind <= nsub);
	j=2;
	do while (j <= ntime);
		if (locf[(ind-1)*ntime+j]=.) then locf[(ind-1)*ntime+j]=locf[(ind-1)*ntime+j-1];
		j= j+1;
	end;
	ind = ind+1;
end;
create help var {&id &time &response locf};
append;
quit;
data &out;
merge &data help;
run;
%mend;

%locf(data=armd111,id=subject,time=time,response=bindif,out=armdlocf);

proc print data=armdlocf;
var subject treat time bindif locf;
run;

* WGEE: macro for creating variables "dropout" and "prev";

%macro dropout(data=,id=,time=,response=,out=);
%if %bquote(&data)= %then %let data=&syslast;
proc freq data=&data noprint;
tables &id /out=freqid;
tables &time / out=freqtime;
run;
proc iml;
reset noprint;
use freqid;
read all var {&id};
nsub = nrow(&id);
use freqtime;
read all var {&time};
ntime = nrow(&time);
time = &time;
use &data;
read all var {&id &time &response};
n = nrow(&response);
dropout = j(n,1,0);
ind = 1;
do while (ind <= nsub);
	j=1;
	if (&response[(ind-1)*ntime+j]=.) then print "First Measurement is Missing";
	if (&response[(ind-1)*ntime+j]^=.) then
	do;
		j = ntime;
		do until (j=1);
			if (&response[(ind-1)*ntime+j]=.) then
			do;
				dropout[(ind-1)*ntime+j]=1;
				j = j-1;
			end;
			else j = 1;
		end;
	end;
	ind = ind+1;
end;
prev = j(n,1,1);
prev[2:n] = &response[1:n-1];
i=1;
do while (i<=n);
	if &time[i]=time[1] then prev[i]=.;
	i = i+1;
end;
create help var {&id &time &response dropout prev};
append;
quit;
data &out;
merge &data help;
run;
%mend;

%dropout(data=armd111,id=subject,time=time,response=bindif,out=armdhlp);

proc genmod data=armdhlp descending;
class trt prev lesion time;
model dropout = prev trt lesion time / pred dist=b;
ods output obstats=pred;
ods listing exclude obstats;
run;

proc print data=pred;
run;

data pred;
set pred;
keep observation pred;
run;

data armdhlp;
merge pred armdhlp;
run;

proc print data=armdhlp;
run;

%macro dropwgt(data=,id=,time=,pred=,dropout=,out=);
%if %bquote(&data)= %then %let data=&syslast;
proc freq data=&data noprint;
tables &id /out=freqid;
tables &time / out=freqtime;
run;
proc iml;
reset noprint;
use freqid;
read all var {&id};
nsub = nrow(&id);
use freqtime;
read all var {&time};
ntime = nrow(&time);
time = &time;
use &data;
read all var {&id &time &pred &dropout};
n = nrow(&pred);
wi = j(n,1,1);
ind = 1;
do while (ind <= nsub);
	wihlp = 1;
	stay = 1;
	/* first measurement */
	if (&dropout[(ind-1)*ntime+2]=1)
		then do;
			wihlp = pred[(ind-1)*ntime+2];
			stay = 0;
		end;
	else if (&dropout[(ind-1)*ntime+2]=0)
		then wihlp = 1-pred[(ind-1)*ntime+2];
	/* second to penultimate measurement */
	j=2;
	do while ((j <= ntime-1) & stay);
		if (&dropout[(ind-1)*ntime+j+1]=1)
			then do;
				wihlp = wihlp*pred[(ind-1)*ntime+j+1];
				stay = 0;
			end;
		else if (&dropout[(ind-1)*ntime+j+1]=0)
			then wihlp = wihlp*(1-pred[(ind-1)*ntime+j+1]);
				j = j+1;
			end;
		j = 1;
		do while (j <= ntime);
			wi[(ind-1)*ntime+j]=wihlp;
			j = j+1;
		end;
	ind = ind+1;
end;
create help var {&id &time &pred &dropout wi};
append;
quit;
data &out;
merge &data help;
data &out;
set &out;
wi=1/wi;
run;
%mend;

%dropwgt(data=armdhlp,id=subject,time=time,pred=pred,dropout=dropout,out=armdwgee);

proc print data=armdwgee;
var subject time bindif dropout prev pred wi;
run;

*3. CC, LOCF, and observed data analyses;
/* LOCF, CC, direct likelihood, (W)GEE analysis */

proc genmod data=armdcc;
title ’CC - GEE’;
class time treat subject;
model bindif = time treat*time / noint dist=binomial;
repeated subject=subject / withinsubject=time type=exch modelse;
run;

proc glimmix data=armdcc;
title ’CC - GEE - linearized version’;
nloptions maxiter=50 technique=newrap;
class time treat subject;
model bindif = time treat*time / noint solution dist=binary;
random _residual_ / subject=subject type=cs;
run;

proc glimmix data=armdcc empirical;
title ’CC - GEE - linearized version - empirical’;
nloptions maxiter=50 technique=newrap;
class time treat subject;
model bindif = time treat*time / noint solution dist=binary;
random _residual_ / subject=subject type=cs;
run;

proc genmod data=armdlocf;
title ’LOCF - GEE’;
class time treat subject;
model locf = time treat*time / noint dist=binomial;
repeated subject=subject / withinsubject=time type=exch modelse;
run;

proc glimmix data=armdlocf;
title ’LOCF - GEE - linearized version’;
nloptions maxiter=50 technique=newrap;
class time treat subject;
model locf = time treat*time / noint solution dist=binary;
random _residual_ / subject=subject type=cs;
run;

proc glimmix data=armdlocf empirical;
title ’LOCF - GEE - linearized version - empirical’;
nloptions maxiter=50 technique=newrap;
class time treat subject;
model locf = time treat*time / noint solution dist=binary ;
random _residual_ / subject=subject type=cs;
run;

proc genmod data=armdwgee;
title ’data as is - GEE’;
class time treat subject;
model bindif = time treat*time / noint dist=binomial;
repeated subject=subject / withinsubject=time type=exch modelse;
run;

proc glimmix data=armdwgee;
title ’data as is - GEE - linearized version’;
nloptions maxiter=50 technique=newrap;
class time treat subject;
model bindif = time treat*time / noint solution dist=binary;
random _residual_ / subject=subject type=cs;
run;

proc glimmix data=armdwgee empirical;
title ’data as is - GEE - linearized version - empirical’;
nloptions maxiter=50 technique=newrap;
class time treat subject;
model bindif = time treat*time / noint solution dist=binary ;
random _residual_ / subject=subject type=cs;
run;

proc genmod data=armdwgee;
title ’data as is - WGEE’;
scwgt wi;
class time treat subject;
model bindif = time treat*time / noint dist=binomial;
repeated subject=subject / withinsubject=time type=exch modelse;
run;

proc glimmix data=armdwgee;
title ’data as is - WGEE - linearized version’;
nloptions maxiter=50 technique=newrap;
weight wi;
class time treat subject;
model bindif = time treat*time / noint solution dist=binary;
random _residual_ / subject=subject type=cs;
run;

proc glimmix data=armdwgee empirical;
title ’data as is - WGEE - linearized version - empirical’;
weight wi;
nloptions maxiter=50 technique=newrap;
class time treat subject;
model bindif = time treat*time / noint solution dist=binary ;
random _residual_ / subject=subject type=cs;
run;

proc glimmix data=armdcc method=rspl;
title ’CC - mixed - PQL’;
nloptions maxiter=50 technique=newrap;
class time treat subject;
model bindif = time treat*time / noint solution dist=binary;
random intercept / subject=subject type=un g gcorr;
run;

proc glimmix data=armdcc method=rspl;
title ’CC - mixed - PQL’;
nloptions maxiter=50 technique=newrap;
class time treat subject;
model bindif = time treat*time / noint solution dist=binary;
random intercept / subject=subject type=un g gcorr;
run;

proc glimmix data=armdlocf method=rspl;
title ’LOCF - mixed - PQL’;
nloptions maxiter=50 technique=newrap;
class time treat subject;
model locf = time treat*time / noint solution dist=binary;
random intercept / subject=subject type=un g gcorr;
run;

proc glimmix data=armdwgee method=rspl;
title ’as is - mixed - PQL’;
nloptions maxiter=50 technique=newrap;
class time treat subject;
model bindif = time treat*time / noint solution dist=binary;
random intercept / subject=subject type=un g gcorr;
run;

data help;
set armdcc;
time1=0;
time2=0;
time3=0;
time4=0;
if time=1 then time1=1;
if time=2 then time2=1;
if time=3 then time3=1;
if time=4 then time4=1;
run;

proc nlmixed data=help qpoints=20 maxiter=100 technique=newrap;
title ’CC - mixed - numerical integration’;
eta = beta11*time1+beta12*time2+beta13*time3+beta14*time4
+b
+(beta21*time1+beta22*time2+beta23*time3+beta24*time4)*(2-treat);
p = exp(eta)/(1+exp(eta));
model bindif ~ binary(p);
random b ~ normal(0,tau*tau) subject=subject;
estimate ’tau^2’ tau*tau;
run;

data help;
set armdlocf;
time1=0;
time2=0;
time3=0;
time4=0;
if time=1 then time1=1;
if time=2 then time2=1;
if time=3 then time3=1;
if time=4 then time4=1;
run;

proc nlmixed data=help qpoints=20 maxiter=100 technique=newrap;
title ’LOCF - mixed - numerical integration’;
eta = beta11*time1+beta12*time2+beta13*time3+beta14*time4
+b
+(beta21*time1+beta22*time2+beta23*time3+beta24*time4)*(2-treat);
p = exp(eta)/(1+exp(eta));
model locf ~ binary(p);
random b ~ normal(0,tau*tau) subject=subject;
estimate ’tau^2’ tau*tau;
run;

data help;
set armdwgee;
time1=0;
time2=0;
time3=0;
time4=0;
if time=1 then time1=1;
if time=2 then time2=1;
if time=3 then time3=1;
if time=4 then time4=1;
run;

proc nlmixed data=help qpoints=20 maxiter=100 technique=newrap;
title ’as is - mixed - numerical integration’;
eta = beta11*time1+beta12*time2+beta13*time3+beta14*time4
+b
+(beta21*time1+beta22*time2+beta23*time3+beta24*time4)*(2-treat);
p = exp(eta)/(1+exp(eta));
model bindif ~ binary(p);
random b ~ normal(0,tau*tau) subject=subject;
estimate ’tau^2’ tau*tau;
run;

* 4. Multiple Imputation;

libname longit  '/home/u43398024/Longitudinal Exercises/';
options nocenter;

data armd13;
set longit.allarmd;
diff4=visual4-visual0;
diff12=visual12-visual0;
diff24=visual24-visual0;
diff52=visual52-visual0;
if trt=1 then treat=1;
if trt=4 then treat=2;
subject=_n_;
run;

proc sort data=armd13;
by treat;
run;

proc mi data=armd13 seed=486048 simple out=armd13a nimpute=10 round=0.1;
var lesion diff4 diff12 diff24 diff52;
by treat;
run;

data armd13a;
set armd13a;
bindif4=0; if diff4 <= 0 then bindif4=1;
bindif12=0;if diff12 <= 0 then bindif12=1;
bindif24=0;if diff24 <= 0 then bindif24=1;
bindif52=0;if diff52 <= 0 then bindif52=1;
if diff4=. then bindif4=.;
if diff12=. then bindif12=.;
if diff24=. then bindif24=.;
if diff52=. then bindif52=.;
run;

proc print data=armd13a;
var _imputation_ diff4 diff12 diff24 diff52 bindif4 bindif12 bindif24 bindif52;
where (subject=1);
run;

data armd13b;
set armd13a;
array x (4) bindif4 bindif12 bindif24 bindif52;
array y (4) diff4 diff12 diff24 diff52;
do j=1 to 4;
	bindif=x(j);
	diff=y(j);
	time=j;
	output;
end;
run;

proc print data=armd13b;
title ’Dataset after imputation’;
var _imputation_ subject time diff bindif;
run;

data armd13c;
set armd13b;
time1=0;
time2=0;
time3=0;
time4=0;
trttime1=0;
trttime2=0;
trttime3=0;
trttime4=0;
if time=1 then time1=1;
if time=2 then time2=1;
if time=3 then time3=1;
if time=4 then time4=1;
if (time=1 & treat=1) then trttime1=1;
if (time=2 & treat=1) then trttime2=1;
if (time=3 & treat=1) then trttime3=1;
if (time=4 & treat=1) then trttime4=1;
run;

proc sort data=armd13c;
by _imputation_ subject time;
run;

proc genmod data=armd13c;
title ’GEE after multiple imputation’;
class time subject;
by _imputation_;
model bindif = time1 time2 time3 time4 trttime1 trttime2 trttime3 trttime4
/ noint dist=binomial covb;
repeated subject=subject / withinsubject=time type=exch modelse;
ods output ParameterEstimates=gmparms parminfo=gmpinfo CovB=gmcovb;
run;

data gmpinfo;
set gmpinfo;
if parameter=’Prm1’ then delete;
run;

proc print data=gmparms;
run;
proc print data=gmcovb;
run;
proc print data=gmpinfo;
run;

proc mianalyze parms=gmparms covb=gmcovb parminfo=gmpinfo wcov bcov tcov;
modeleffects time1 time2 time3 time4 trttime1 trttime2 trttime3 trttime4;
run;

proc nlmixed data=m.armd13c qpoints=20 maxiter=100 technique=newrap cov ecov;
title ’NLMIXED after multiple imputation’;
by _imputation_;
eta = beta11*time1+beta12*time2+beta13*time3+beta14*time4
+b
+beta21*trttime1+beta22*trttime2+beta23*trttime3+beta24*trttime4;
p = exp(eta)/(1+exp(eta));
model bindif ~ binary(p);
random b ~ normal(0,tau*tau) subject=subject;
estimate ’tau2’ tau*tau;
ods output ParameterEstimates=nlparms CovMatParmEst=nlcovb
AdditionalEstimates=nlparmsa CovMatAddEst=nlcovba;
run;

proc mianalyze parms=nlparms covb=nlcovb;
title ’MIANALYZE for NLMIXED’;
modeleffects beta11 beta12 beta13 beta14 beta21 beta22 beta23 beta24 tau;
run;

* 5. Fit GLMMs using PQL and MQL;

proc glimmix data = m.armd method = RSPL;
title ’PQL REML’;
class subject;
model bindif (event = ’1’) = timec treat * timec / dist = binary solution;
random intercept / subject = subject;
run;

proc glimmix data = m.armd method = MSPL noclprint noitprint;
title ’PQL ML’;
class subject;
model bindif (event = ’1’) = timec treat * timec / dist = binary solution;
random intercept / subject = subject;
run;

proc glimmix data = m.armd method = RMPL noclprint noitprint;
title ’MQL REML’;
class subject;
model bindif (event = ’1’) = timec treat * timec / dist = binary solution;
random intercept / subject = subject;
run;

proc glimmix data = m.armd method = MMPL noclprint noitprint;
title ’MQL ML’;
class subject;
model bindif (event = ’1’) = timec treat * timec / dist = binary solution;
random intercept / subject = subject;
run;

* 6. Fit GLMMs using Gaussian Quadrature;
* Get initial values for GLMMs;
proc genmod data = m.armd descending;
title ’Initial Values’;
model bindif = timec treat * timec / dist = bin;
run;

* Gauss-Quadrature non-adaptive Q = 25;
proc nlmixed data = m.armd noad qpoints = 25;
title ’MML GQ 25’;
parms beta0 = 0.5670 beta1 = 0.0098 beta2 = 0.0133 sigmab = 2;
eta = beta0 + b + beta1 * timec + beta2 * treat * timec;
pr = exp(eta) / (1 + exp(eta));
model bindif ~ binary(pr);
random b ~ normal(0, sigmab**2) subject = subject;
estimate ’sigmab^2’ sigmab**2;
run;

* Gauss-Quadrature non-adaptive Q = 51;
proc nlmixed data = m.armd noad qpoints = 51;
title ’MML GQ 51’;
parms beta0 = 0.5670 beta1 = 0.0098 beta2 = 0.0133 sigmab = 2;
eta = beta0 + b + beta1 * timec + beta2 * treat * timec;
pr = exp(eta) / (1 + exp(eta));
model bindif ~ binary(pr);
random b ~ normal(0, sigmab**2) subject = subject;
estimate ’sigmab^2’ sigmab**2;
run;

* Laplace approximation;
proc nlmixed data = m.armd qpoints = 1;
title ’MML Laplace’;
parms beta0 = 0.5670 beta1 = 0.0098 beta2 = 0.0133 sigmab = 2;
eta = beta0 + b + beta1 * timec + beta2 * treat * timec;
pr = exp(eta) / (1 + exp(eta));
model bindif ~ binary(pr);
random b ~ normal(0, sigmab**2) subject = subject;
estimate ’sigmab^2’ sigmab**2;
run;

* Gauss-Quadrature adaptive Q = 5;
proc nlmixed data = m.armd qpoints = 5;
title ’MML AGQ 5’;
parms beta0 = 0.5670 beta1 = 0.0098 beta2 = 0.0133 sigmab = 2;
eta = beta0 + b + beta1 * timec + beta2 * treat * timec;
pr = exp(eta) / (1 + exp(eta));
model bindif ~ binary(pr);
random b ~ normal(0, sigmab**2) subject = subject;
estimate ’sigmab^2’ sigmab**2;
run;

* Gauss-Quadrature adaptive Q = 11;
proc nlmixed data = m.armd qpoints = 11 ebopt;
title ’MML AGQ 11’;
parms beta0 = 0.5670 beta1 = 0.0098 beta2 = 0.0133 sigmab = 2;
eta = beta0 + b + beta1 * timec + beta2 * treat * timec;
pr = exp(eta) / (1 + exp(eta));
model bindif ~ binary(pr);
random b ~ normal(0, sigmab**2) subject = subject out = EB;
estimate ’sigmab^2’ sigmab**2;
run;

proc univariate data = EB;
var estimate;
histogram estimate;
title ’Empirical Bayes Estimates’;
run;

* 7. Likelihood Ratio Tests for random-slopes and treatment;
* Test for random slopes;
proc nlmixed data = m.armd qpoints = 11;
title ’MML AGQ 11 / Slopes’;
parms beta0 = 0.5670 beta1 = 0.0098 beta2 = 0.0133
sigmab1 = 2 sigmab2 = 1 rho = -0.4;
eta = beta0 + b1 + beta1 * timec + b2 * timec + beta2 * treat * timec;
pr = exp(eta) / (1 + exp(eta));
model bindif ~ binary(pr);
random b1 b2 ~ normal([0, 0], [sigmab1**2, rho * sigmab1 * sigmab2, sigmab2**2])
subject = subject;
run;

data LRT;
L01 = -2 * (-449.285125 - (-443.794458));
df = 2;
pval = 1 - probchi(L01, 2);
run;
proc print data = LRT;
run;

* Increase quadrature points and test again;
proc nlmixed data = m.armd qpoints = 21;
title ’MML AGQ 21’;
parms beta0 = 0.5670 beta1 = 0.0098 beta2 = 0.0133 sigmab = 2;
eta = beta0 + b + beta1 * timec + beta2 * treat * timec;
pr = exp(eta) / (1 + exp(eta));
model bindif ~ binary(pr);
random b ~ normal(0, sigmab**2) subject = subject;
run;

proc nlmixed data = m.armd qpoints = 21;
title ’MML AGQ 21 / Slopes’;
parms beta0 = 0.5670 beta1 = 0.0098 beta2 = 0.0133
sigmab1 = 2 sigmab2 = 1 rho = -0.4;
eta = beta0 + b1 + beta1 * timec + b2 * timec + beta2 * treat * timec;
pr = exp(eta) / (1 + exp(eta));
model bindif ~ binary(pr);
random b1 b2 ~ normal([0, 0], [sigmab1**2, rho * sigmab1 * sigmab2, sigmab2**2])
subject = subject;
run;

data LRT;
L01 = -2 * (-449.295733 - (-443.902654));
df = 2;
pval = 1 - probchi(L01, 2);
run;
proc print data = LRT;
run;

* Test for a treatment effect;
proc nlmixed data = m.armd qpoints = 21;
title ’MML AGQ 21 / No Treatment’;
parms beta0 = 0.5670 beta1 = 0.0098 sigmab1 = 2 sigmab2 = 1 rho = -0.4;
eta = beta0 + b1 + beta1 * timec + b2 * timec;
pr = exp(eta) / (1 + exp(eta));
model bindif ~ binary(pr);
random b1 b2 ~ normal([0, 0], [sigmab1**2, rho * sigmab1 * sigmab2, sigmab2**2])
subject = subject;
run;

data LRT;
L01 = 889.7 - 887.8;
df = 2;
pval = 1 - probchi(L01, 2);
run;
proc print data = LRT;
run;

* 8. Calculate and plot the marginal average evolutions.
%inc ’C:\ARMD\MVN.sas’;
data mean_b;
input m1;
cards;
0 0
run;

data var_b;
input m1-m2;
cards;
5.354133 -0.006765938
-0.006765938 0.006379217
run;

%mvn(varcov = var_b, means = mean_b, n = 4000, sample = b, seed = -1);
data b;
set b;
do t = 1 to 4 by 1;
	output;
end;
drop t;
run;

data SimulateValues;
do treat = 0 to 1 by 1;
	do subject = 1 to 2000 by 1;
		do t = 1 to 4 by 1;
			output;
		end;
	end;
end;
run;

proc sort data = SimulateValues;
by subject;
run;

data SimulateValues;
merge SimulateValues b;
run;

proc sort data = SimulateValues;
by t treat;
run;

data SimulateValues;
set SimulateValues;
timec = 0;
if t = 1 then timec = 4;
if t = 2 then timec = 12;
if t = 3 then timec = 24;
if t = 4 then timec = 52;
if treat = 0 then
y = 1 / (1 + exp(-(0.7860 + col1 + (0.04966 + col2)*timec)));
else
y = 1 / (1 + exp(-(0.7860 + col1 + (0.07458 + col2)*timec)));
run;

proc means data = SimulateValues;
var y;
by timec treat;
output out = out;
run;

proc gplot data = out;
plot y * timec = treat / haxis = axis1 vaxis = axis2 legend = legend1;
axis1 label = (h = 2 ’Time’) value = (h = 1.5) order = (3 to 53 by 10)
minor = none;
axis2 label = (h = 2 A = 90 ’P(Y = 1)’) value = (h = 1.5)
order = (0.6 to 0.9 by 0.1) minor = none;
legend1 label = (h = 1.5 ’Treatment: ’) value = (h = 1.5 ’A’ ’B’);
title h = 2.5 ’Marginal average evolutions (GLMM)’;
symbol1 c = black i = join w = 5 l = 1 mode = include;
symbol2 c = black i = join w = 5 l = 2 mode = include;
where _stat_ = ’MEAN’;
run; quit; run;

* 9. Test at week 52;
data m.armd52;
set m.armd;
time12 = 0;
time24 = 0;
time52 = 0;
if timec = 12 then time12 = 1;
if timec = 24 then time24 = 1;
if timec = 52 then time52 = 1;
run;

proc genmod data = m.armd52 descending;
title ’Initial Values’;
model bindif = time12 time24 time52 time12*treat
time24*treat time52*treat / dist = bin;
run;

proc nlmixed data = m.armd52 qpoints = 21;
title ’Time as factor’;
parms beta0 = 0.7522 beta1 = -0.3538 beta2 = -0.1253 beta3 = 0.5190
beta4 = 0.6288 beta5 = 0.4457 beta6 = 0.4205 sigmab = 2;
eta = beta0 + b + beta1 * time12 + beta2 * time24 + beta3 * time52 +
(beta4 * time12 + beta5 * time24 + beta6 * time52) * treat;
pr = exp(eta) / (1 + exp(eta));
model bindif ~ binary(pr);
random b ~ normal(0, sigmab**2) subject = subject;
estimate ’MrgTrEff’ beta6 / sqrt(0.345843 * sigmab**2 + 1);
run;
