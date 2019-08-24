#d;

cd "C:\Users\Jacob\Desktop\My Projects\Senior Seminar\STATA";

/*insheet using "2017a.csv", clear;
drop if schl<21;
drop if schl==.;
outsheet using 2017a2.csv, comma;

insheet using "2017b.csv", clear;
drop if schl<21;
drop if schl==.;
outsheet using 2017b2.csv, comma;

insheet using "2016a.csv", clear;
drop if schl<21;
drop if schl==.;
outsheet using 2016a2.csv, comma;

insheet using "2016b.csv", clear;
drop if schl<21;
drop if schl==.;
outsheet using 2016b2.csv, comma;

insheet using "2015a.csv", clear;
drop if schl<21;
drop if schl==.;
outsheet using 2015a2.csv, comma;

insheet using "2015b.csv", clear;
drop if schl<21;
drop if schl==.;
outsheet using 2015b2.csv, comma;

insheet using "2014a.csv", clear;
drop if schl<21;
drop if schl==.;
outsheet using 2014a2.csv, comma;

insheet using "2014b.csv", clear;
drop if schl<21;
drop if schl==.;
outsheet using 2014b2.csv, comma;

insheet using "2013a.csv", clear;
drop if schl<21;
drop if schl==.;
outsheet using 2013a2.csv, comma;

insheet using "2013b.csv", clear;
drop if schl<21;
drop if schl==.;
outsheet using 2013b2.csv, comma;

insheet using "2012a.csv", clear;
drop if schl<21;
drop if schl==.;
outsheet using 2012a2.csv, comma;

insheet using "2012b.csv", clear;
drop if schl<21;
drop if schl==.;
outsheet using 2012b2.csv, comma;
*/ 

/*ssc install csvconvert;
csvconvert D:\Econometrics\FinalProject\Data\NewData\CleanedData, replace;

use output, clear;
keep serialno sporder st adjinc pwgtp agep cit cow eng intp mar oip pap sch schg schl semp sex wagp wkhp wkw dis fod1p fod2p hisp indp naicsp occp pernp pincp pobp powsp rac1p socp waob fesrp esr yoep _csvfile;

gen year = 0;
replace year = 2017 if _csvfile=="2017a2.csv";
replace year = 2017 if _csvfile=="2017b2.csv";
replace year = 2016 if _csvfile=="2016a2.csv";
replace year = 2016 if _csvfile=="2016b2.csv";
replace year = 2015 if _csvfile=="2015a2.csv";
replace year = 2015 if _csvfile=="2015b2.csv";
replace year = 2014 if _csvfile=="2014a2.csv";
replace year = 2014 if _csvfile=="2014b2.csv";
replace year = 2013 if _csvfile=="2013a2.csv";
replace year = 2013 if _csvfile=="2013b2.csv";
replace year = 2012 if _csvfile=="2012a2.csv";
replace year = 2012 if _csvfile=="2012b2.csv";

drop _csvfile;

save "D:\Econometrics\FinalProject\Data\NewData\CleanedData\Clean.dta";
*/

use Data, clear;

drop if schl>=22;

gen logwage=ln(wagp);

gen doubleMajor = 0;
replace doubleMajor = 1 if fod2p !=.;

gen female = 0;
replace female = 1 if sex == 2;

gen white = 0;
replace white = 1 if rac1p==1;

gen black = 0;
replace black = 1 if rac1p==2;

gen hispanic = 0;
replace hispanic = 1 if hisp!=01;

gen other = 1;
replace other = 0 if white==1;
replace other = 0 if black==1;
replace other = 0 if hispanic==1;

gen married = 0;
replace married = 1 if mar==1;

gen agep2 = agep^2;

/****Correct ommitted wagp numbers. This had no effect on my regression****/
replace wagp =. if wagp==0 & wkhp>0;

/****Remove individuals who are retired. This had no effect on my regression****/
drop if wagp==. & wkhp==. & agep>=61;

/****Let's find some summary statistics!****/

sum agep;
mean(agep);

histogram wagp, normal ylabel(, angle(horizontal)) title(Histogram of Earnings) xtitle(Earnings);
graph export Histogram.png, replace;
mean(wagp);

/*
collapse (mean) wagp, by (agep);
scatter wagp agep,ylabel(, angle(horizontal)) title(Scatterplot of Earnings) xtitle(Age) ytitle(Earnings);
graph export ScatterPlot1.png, replace;
*/

/*Top most common majors*/
tab fod1p, plot sort;
tab fod2p, plot sort;
tab doubleMajor;
tab occp;

tab female;
tab married;

drop if agep >= 65;
drop if wagp >250000;

sum agep;
mean(agep);
mean(wagp);

/*
histogram wagp, normal ylabel(, angle(horizontal)) title(Histogram of Earnings) xtitle(Earnings);
graph export Histogram2.png, replace;
mean(wagp);
*/

/*
collapse (mean) wagp, by (agep);
scatter wagp agep;
graph export ScatterPlot2.png, replace;
*/

/*Regression Analysis*/
/*ssc install outreg2;*/


reg logwage doubleMajor;
outreg2 using Regression.xls, replace;

reg logwage doubleMajor female agep agep2 married black hispanic other;
outreg2 using Regression.xls, append;

reg logwage doubleMajor female agep agep2 married black hispanic other wkhp wkw;
outreg2 using Regression.xls, append;

areg logwage doubleMajor female agep agep2 married black hispanic other wkhp wkw, absorb(occp);
outreg2 using Regression.xls, append;

tab year, gen(year);
tab st, gen(st);

areg logwage doubleMajor female agep agep2 married black hispanic other wkhp wkw year2-year6 st2-st51, absorb(occp);
outreg2 using Regression.xls, append;

tab fod1p, gen(major);

areg logwage doubleMajor female agep agep2 married black hispanic other wkhp wkw year2-year6 st2-st51 major2-major173, absorb(occp);
outreg2 using Regression.xls, append;

gen unemployed = 0;
replace unemployed = 1 if esr ==3;

reg unemployed doubleMajor;
outreg2 using unemployed.xls, replace;

reg unemployed doubleMajor female agep agep2 married black hispanic other;
outreg2 using unemployed.xls, append;

areg unemployed doubleMajor female agep agep2 married black hispanic other, absorb(occp);
outreg2 using unemployed.xls, append;

areg unemployed doubleMajor female agep agep2 married black hispanic other year2-year6 st2-st51 major2-major173, absorb(occp);
outreg2 using unemployed.xls, append;
