// The following code replicates the analysis in: Eva Anduiza & Guillem Rico, 
// "Sexism and the far-right vote: The individual dynamics of gender backlash." 
// American Journal of Political Science.
//
// This code also prepares the data for additional analyses to be conducted 
// using R script replication_code_2.R.
//
// Analysis conducted using Stata/SE 16.1 for Windows (64-bit x86-64).
// Required package: estout (install using: ssc install estout, replace).




* If not already set, uncomment and set the folder where the data is stored as 
* the working directory
// cd "path/to/data"

* Load data
import delimited "spanish_political_attitudes_dataset_2017_to_2020.csv", clear




****************************
***** Data preparation *****
****************************

* Respondent id code
gen idcode=codpanelista2

* Time variable
gen time=wave-9
lab def time 0 "2017" 1 "2018" 2 "2019" 3 "2020"
lab val time time

* Panel settings
xtset idcode time

* Number of waves completed by respondent
duplicates tag idcode, gen(nwaves)
replace nwaves=nwaves + 1

* Year (i.e. wave)
recode wave (8=2016)(9=2017)(10=2018)(11=2019)(12=2020), gen(year)

* Gender
recode sex (1=0 "Male")(2=1 "Female"), gen(female)

* Age
recode age (16/25=1 "16-25")(26/35=2 "26-35")(36/45=3 "36-45")(46/100=4 "46+"), gen(age4)

* Cohort
gen coh0=age if wave==9
egen coh1=total(coh0), missing by(idcode)
replace coh1=age-1 if wave==10 & coh1>=.
egen coh2=mean(coh1), by(idcode)
replace coh2=age-2 if wave==11 & coh2>=.
egen coh3=mean(coh2), by(idcode)
replace coh3=age-3 if wave==12 & coh3>=.
egen coh4=mean(coh3), by(idcode)
gen cohort=coh4
drop coh0 coh1 coh2 coh3 coh4
recode cohort (15/29=1 "15-29")(30/44=2 "30-44")(45/max=3 "45+"), gen(g3cohort)

* Education
recode education (1/4=1 "Lower secondary")(5/7=2 "Upper secondary")(8/11=3 "Tertiary"), gen(edu3)

* Education dummies
qui tab edu3, gen(edu3_)

* Partner
lab def livingpartner 1 "Lives with partner"
lab val livingpartner livingpartner 

* Income (missing values imputed from other waves when available)
recode hhincome (99=.), gen(hincome)
foreach var of varlist hincome {
    gen `var'17 = `var' if year==2017
	gen `var'18 = `var' if year==2018
	gen `var'19 = `var' if year==2019
	gen `var'20 = `var' if year==2020
	egen `var'17all = max(`var'17), by(idcode)
	egen `var'18all = max(`var'18), by(idcode)
	egen `var'19all = max(`var'19), by(idcode)
	egen `var'20all = max(`var'20), by(idcode)
	replace `var'17all = `var'18all if `var'17all >= .
	replace `var'17all = `var'19all if `var'17all >= .
	replace `var'17all = `var'20all if `var'17all >= .
	replace `var'18all = `var'17all if `var'18all >= .
	replace `var'18all = `var'19all if `var'18all >= .
	replace `var'18all = `var'20all if `var'18all >= .
	replace `var'19all = `var'18all if `var'19all >= .
	replace `var'19all = `var'20all if `var'19all >= .
	replace `var'19all = `var'17all if `var'19all >= .
	replace `var'20all = `var'19all if `var'20all >= .
	replace `var'20all = `var'18all if `var'20all >= .
	replace `var'20all = `var'17all if `var'20all >= .
	gen `var'_all = `var'17all if year == 2017
	replace `var'_all = `var'18all if year == 2018
	replace `var'_all = `var'19all if year == 2019
	replace `var'_all = `var'20all if year == 2020
	drop `var'17-`var'20 
}
recode hincome_all (1/5=1 "Low")(6/8=2 "Mid")(9/12=3 "High"), gen(x3hincall)
gen dhincome_all = (hincome_all - 1) / 11

* Interest in politics
gen intpol = (4 - polintr) / 3
recode polintr (1 2=1 "Quite or very")(3 4=0 "Hardly or not at all")(*=.), gen(dintpol)

* Ideological identification
gen ideol=lrself/10
recode lrself (0/2=1 "Far left")(3 4=2 "Center left")(5=3 "Center")(6/7=4 "Center right")(8/10=5 "Far right"), gen(ideo5)

* Authoritarianism
recode indeprespect curiosmanners empathybehave (1=0)(2=1), gen(a_respect a_manner a_behave)
recode selfconfobed (1=1)(2=0), gen(a_obedient)
gen authoritarian=(a_respect + a_manner + a_behave + a_obedient)/4

* Nativism
gen natveco=(10-immigeco)/10
gen natvcult=immicult/10
egen nativism=rowmean(natveco natvcult)

* Populism
foreach var of varlist populisma populismd populismf populismi populismj populismn {
 gen ip_`var'=(`var'-1)/6
}
egen pop6amz=rowmean(ip_pop*)

* Territorial preferences
gen orgterr=(constpref-1)/4

* Modern sexism (9-item scale)
recode femindexa femindexb femindexc femindexd femindexe femindexf femindexg femindexh femindexi (.=.), gen(imsex_1a imsex_2b imsex_3c imsex_4d imsex_5e imsex_6f imsex_7g imsex_8h imsex_9i)
recode imsex_2b imsex_6f imsex_7g (1=7)(2=6)(3=5)(4=4)(5=3)(6=2)(7=1)
egen msexism=rowmean(imsex_*)
replace msexism=(msexism-1)/6

* Swim et al.'s (1995) original 8-item modern sexism scale
egen swim_msex=rowmean(imsex_1-imsex_8)
replace swim_msex=(swim_msex-1)/6

* Engagement in Women's Day protests
recode femstrike femdemonstrate feminfo femtalk (1=1)(2=0), gen(p8m_strike p8m_demonst p8m_mobiliz p8m_talked)
gen ip8m = (p8m_strike + p8m_demonst + p8m_mobiliz + p8m_talked)/4

* Intended vote for Vox (main dependent variable)
recode voteintentionspain (23=1 "Vox")(*=0 "Else"), gen(vim_vox)

* Intended vote for PP (placebo dependent variable)
recode voteintentionspain (2=1 "PP")(*=0 "Else"), gen(vim_pp)

* Reported vote for the four largest parties in the 2016 election, as reported 
* in the 2017 wave, otherwise as reported in the 2018 wave
recode vote2016 (1=1 "PSOE")(2=2 "PP")(3 5 25=3 "Podemos")(4=4 "Cs")(1/56=0 "Else"), gen(vr16_all)
gen vr16_17=vr16_all if year==2017
gen vr16_18=vr16_all if year==2018
egen vr16_17a=max(vr16_17), by(idcode)
egen vr16_18a=max(vr16_18), by(idcode)
gen v16all=vr16_17a
replace v16all=vr16_18a if (vr16_17a==0 | vr16_17a>=.) & vr16_18a<.
recode v16all (0=4 "Others")(1=1 "PSOE")(2=0 "PP")(3=2 "Podemos")(4=3 "Ciudadanos")(*=.), gen(rv16all)
drop vr16_all v16all vr16_17* vr16_18* 

* Creates time-invariant variables fixed at their 2017 values (2018 for living 
* with partner and engagement in Women's Day protests), to be used in the 
* multilevel growth-curve models
foreach var of varlist edu3 x3hincall dintpol ideo5 rv16all {
    bysort idcode (year): gen t1`var'=`var'[1] if nwaves==4
}
bysort idcode (year): gen t2partner=livingpartner[2] if nwaves==4
bysort idcode (year): gen t2ip8m=ip8m[2] if nwaves==4
lab var t2partner "Lives with partner"
lab var t2ip8m "Women’s Day protest engagement"

lab def t1edu3 1 "Lower 2ry" 2 "Upper 2ry" 3 "3ry"
lab val t1edu3 t1edu3 
lab def yesno 1 "Yes" 0 "No"
lab val t2partner yesno
lab def income 1 "Low" 2 "Mid" 3 "High"
lab val t1x3hincall income
lab def intpol 0 "Low" 1 "High"
lab val t1dintpol intpol
lab val t1rv16all rv16all
lab def t1ideo5 1 "Far left" 2 "Left" 3 "Center" 4 "Right" 5 "Far right"
lab val t1ideo5 t1ideo5

* Lagged and change variables
xtset idcode time
foreach var of varlist vim_vox authoritarian ideol nativism orgterr pop6 msexism swim_msex {
	gen l2`var' = l2.`var'
	gen ls`var' = ls.`var'
}

* Positive and negative (lagged) change in sexism (extended and original modern 
* sexism)
gen posmsex = lsmsex * (lsmsex > 0)
gen negmsex = lsmsex * (lsmsex < 0)
gen posswim = lsswim_msex * (lsmsex > 0)
gen negswim = lsswim_msex * (lsmsex < 0)

* Variable labels
lab var female "Female"
lab var age "Age"
lab var cohort "Cohort"
lab var edu3 "Education"
lab var livingpartner "Lives with partner"
lab var dhincome_all "Income"
lab var intpol "Interest in politics"
lab var ideol "Ideological identification"
lab var authoritarian "Authoritarianism"
lab var pop6amz "Populism"
lab var nativism "Nativism"
lab var orgterr "Territorial preference"
lab var msexism "Sexism"
lab var swim_msex "Sexism"
lab var vim_vox "Vox intention"
lab var vim_pp "PP intention"
lab var g3cohort "Cohort"
lab var t1edu3 "Education (ref. Lower 2ry or less)"
lab var t1ideo5 "Ideological identification (ref. Far left)"
lab var t2partner "Lives with partner"
lab var t1x3hincall "Income (ref. Low)"
lab var t1dintpol "Interest in politics"
lab var t1rv16all "Vote in 2016 (ref. PP)"




***********************
****** Main text ******
***********************

* Internal reliability of the 9-item modern sexism scale across waves 
alpha imsex_1-imsex_9 if year==2017
alpha imsex_1-imsex_9 if year==2018
alpha imsex_1-imsex_9 if year==2019
alpha imsex_1-imsex_9 if year==2020


* FIGURE 1
// Results saved as file data_figure_1.txt, to be imported using 
// replication_code_2.R, which produces Figure 1
qui reg msex i.time
eststo fig1: margins time, post
estout fig1 using "data_figure_1.txt", cells("b ci_l ci_u") label replace
eststo clear

// Paired t tests of differences across adjacent waves (all p < 0.001)
xtreg msex i.year if year<2019, fe
xtreg msex i.year if year==2018 | year==2019, fe
xtreg msex i.year if year>2018, fe

// As mentioned in the text, results are very similar if the sample is 
// restricted to respondents who completed all four waves
qui reg msex i.time if nwaves==4
margins time


* FIGURE 2
// Calculates changes in sexism across waves and exports data, to be imported 
// using replication_code_2.R, which produces Figure 2
gen difmsex=S.msexism
export delimited idcode year msexism difmsex using "data_figure_2.csv", nolabel replace

// Percentage of respondents increasing their levels of sexism and pct doing so
// by more than 0.1 points in 2018, 2019, and 2020
recode difmsex (min/0 = 0)(0/max=1), gen(ms_increase)
recode difmsex (min/0.1 = 0)(0.1/max=1), gen(ms_incplus)
tab ms_increase year, col
tab ms_incplus year, col


* FIGURE 3
// Estimates multilevel growth-curve models (might take some time to run)
eststo mix1: qui xtmixed msex (i.female g3cohort i.t1edu3 i.t1ideo5 i.t2partner i.t1x3hincall i.t1dintpol i.t1rv16all)##time if nwaves==4 || idcode: i.time, cov(uns)
eststo mix2: qui xtmixed msex (i.female g3cohort i.t1edu3 i.t1ideo5 i.t2partner i.t1x3hincall i.t1dintpol i.t1rv16all c.t2ip8m)##time if nwaves==4 || idcode: i.time, cov(uns)

// Predicts values of modern sexism and export data to be imported using 
// replication_code_2.R, which produces Figure 3 (and Figure A2)
est restore mix1
eststo female: margins time#female, post
estout female using "mx_female.txt", cells("b ci_l ci_u") label replace
est restore mix1
eststo cohort: margins time#g3cohort, post
estout cohort using "mx_cohort.txt", cells("b ci_l ci_u") label replace
est restore mix1
eststo education: margins time#t1edu3, post
estout education using "mx_education.txt", cells("b ci_l ci_u") label replace
est restore mix1
eststo partner: margins time#t2partner, post
estout partner using "mx_partner.txt", cells("b ci_l ci_u") label replace
est restore mix1
eststo income: margins time#t1x3hincall, post
estout income using "mx_income.txt", cells("b ci_l ci_u") label replace
est restore mix1
eststo interest: margins time#t1dintpol, post
estout interest using "mx_interest.txt", cells("b ci_l ci_u") label replace
est restore mix1
eststo ideology: margins time#t1ideo5, post
estout ideology using "mx_ideology.txt", cells("b ci_l ci_u") label replace
est restore mix1
eststo party: margins time#t1rv16all, post
estout party using "mx_party.txt", cells("b ci_l ci_u") label replace
est restore mix2
eststo ip8m: margins time, at(t2ip8m=(0(.25)1)) post
estout ip8m using "mx_8m.txt", cells("b ci_l ci_u") label replace
eststo clear


* TABLE 1
// Column 1 (2019)
eststo t1m1: logit vim_vox female age i.edu3 dhincome_all livingpartner intpol authoritarian ideol nativism orgterr pop6 msex if year==2019

// Column 2 (2020)
eststo t1m2: logit vim_vox female age i.edu3 dhincome_all livingpartner intpol authoritarian ideol nativism orgterr pop6 msex if year==2020

// Prints Table 1
esttab t1*, b(3) se(3) star(+ 0.1 * 0.05 ** 0.01) nobase noomit eqlabels(none) label nonum mtitle(2019 2020) title("Table 1. Predictors of intention to vote for Vox in 2019 and 2020") nogap varwidth(35)

// Change in probability of supporting Vox if sexism increases from 5th to 95th 
// percentile
est restore t1m1
sum msex if e(sample), det
margins, at((p5) msex) at((p95) msex)
margins, at((p5) msex) at((p95) msex) contrast(atcontrast(r) effects)

est restore t1m2
sum msex if e(sample), det
margins, at((p5) msex) at((p95) msex)
margins, at((p5) msex) at((p95) msex) contrast(atcontrast(r) effects)
eststo clear


* TABLE 2
xtset idcode time

// Column 1 (2019)
eststo t2m1: logit vim_vox female age i.edu3 dhincome_all livingpartner intpol l.(vim_vox authoritarian ideol nativism orgterr pop6 msex) if year==2019

// Column 2 (2020)
eststo t2m2: logit vim_vox female age i.edu3 dhincome_all livingpartner intpol l.(vim_vox authoritarian ideol nativism orgterr pop6 msex) if year==2020

// Prints Table 2
esttab t2*, b(3) se(3) star(+ 0.1 * 0.05 ** 0.01) nobase noomit eqlabels(none) rename(L.vim_vox vim_vox L.ideol ideol L.nativism nativism L.orgterr orgterr L.pop6amz pop6amz L.msexism msexism L.authoritarian authoritarian) refcat(vim_vox "Prior values (t-1)", nolabel) label nonum mtitle(2019 2020) title("Table 2. Effect of prior attitudes on intended vote for Vox") nogap varwidth(35)

// Change in probability of switching to Vox if lagged sexism increases from 
// 5th to 95th percentile
est restore t2m1
sum msex if e(sample), det
margins, at((p5) L.msexism) at((p95) L.msexism)
margins, at((p5) L.msexism) at((p95) L.msexism) contrast(atcontrast(r) effects)

est restore t2m2
sum msex if e(sample), det
margins, at((p5) L.msexism) at((p95) L.msexism)
margins, at((p5) L.msexism) at((p95) L.msexism) contrast(atcontrast(r) effects)
eststo clear


* TABLE 3
// The models in Table 3 are estimated using penalized maximum likelihood 
// as implemented in the brglm2 R package. The following code exports the data 
// to be analyzed using replication_code_2.R, which produces Table 3

// Exports data
export delimited idcode year vim_vox female age edu3 edu3_1-edu3_3 dhincome_all livingpartner intpol authoritarian ideol nativism orgterr pop6 msexism swim_msex l2* ls* posmsex negmsex posswim negswim using "data_table_3.csv" if year==2019 | year==2020, nolabel replace




**********************************
***** Supporting information *****
**********************************

* TABLE A2
// Identifies panel survival across waves and prepares data for the analysis
duplicates tag idcode if year == 2017 | year == 2018, gen(ret1718)
duplicates tag idcode if year == 2018 | year == 2019, gen(ret1819)
duplicates tag idcode if year == 2019 | year == 2020, gen(ret1920)
recode ret1718 (* = .) if year == 2018
recode ret1819 (* = .) if year == 2019
recode ret1920 (* = .) if year == 2020
duplicates tag idcode if year >= 2017 & year <= 2019, gen(ret789)
duplicates tag idcode if year >= 2018 & year <= 2020, gen(ret890)
recode ret789 ret890 (1=0)(2=1)
recode ret789 (* = .) if year != 2017
recode ret890 (* = .) if year != 2018

// Column 1 (2018-19)
eststo ta2_1: logit ret1819 female age i.edu3 dhincome_all livingpartner intpol authoritarian ideol nativism orgterr pop6 msex

// Column 2 (2019-20)
eststo ta2_2: logit ret1920 female age i.edu3 dhincome_all livingpartner intpol authoritarian ideol nativism orgterr pop6 msex

// Column 3 (2017-19)
eststo ta2_3: logit ret789 female age i.edu3 dhincome_all intpol authoritarian ideol nativism orgterr pop6 msex

// Column 4 (2018-20)
eststo ta2_4: logit ret890 female age i.edu3 dhincome_all livingpartner intpol authoritarian ideol nativism orgterr pop6 msex

// Prints Table A2
esttab ta2_*, b(3) se(3) star(+ 0.1 * 0.05 ** 0.01 *** 0.001) nobase noomit eqlabels(none) label mtitle("2018-19" "2019-20" "2017-19" "2018-20") title("Table A2. Predicting the probability of surviving over waves") nogap varwidth(35)
eststo clear


* TABLE A3
// Calculates inverse probability of panel survival (2018-19)
xtset idcode year
qui reg vim_vox female age i.edu3 dhincome_all livingpartner intpol l.(vim_vox authoritarian ideol nativism orgterr pop6 msex) if year==2019
gen s1819 = e(sample)
qui logit ret1819 female age i.edu3 dhincome_all livingpartner intpol authoritarian ideol nativism orgterr pop6 msex
predict pr1819 if s1819, pr
gen ipa1819 = (1/pr1819)
sum ipa1819
replace ipa1819 = ipa1819 * (1/r(mean))
bysort idcode: egen w1819 = max(ipa1819)

// Calculates inverse probability of panel survival (2019-20)
xtset idcode year
qui reg vim_vox female age i.edu3 dhincome_all livingpartner intpol l.(vim_vox authoritarian ideol nativism orgterr pop6 msex) if year==2020
gen s1920 = e(sample)
qui logit ret1920 female age i.edu3 dhincome_all livingpartner intpol authoritarian ideol nativism orgterr pop6 msex
predict pr1920 if s1920, pr
gen ipa1920 = (1/pr1920)
sum ipa1920
replace ipa1920 = ipa1920 * (1/r(mean))
bysort idcode: egen w1920 = max(ipa1920)

// Column 1 (2019)
eststo ta3_1: logit vim_vox female age i.edu3 dhincome_all livingpartner intpol l.(vim_vox authoritarian ideol nativism orgterr pop6 msex) if year==2019 [iw=w1819]

// Column 2 (2020)
eststo ta3_2: logit vim_vox female age i.edu3 dhincome_all livingpartner intpol l.(vim_vox authoritarian ideol nativism orgterr pop6 msex) if year==2020 [iw=w1920]

// Prints Table A3
esttab ta3_*, b(3) se(3) star(+ 0.1 * 0.05 ** 0.01 *** 0.001) nobase noomit eqlabels(none) rename(L.vim_vox vim_vox L.ideol ideol L.nativism nativism L.orgterr orgterr L.pop6amz pop6amz L.msexism msexism L.authoritarian authoritarian) refcat(vim_vox "Prior values (t-1)", nolabel) label nonum mtitle(2019 2020) title("Table A3. Effect of prior attitudes on intended vote for Vox, weighted by the inverse probability of panel survival") nogap varwidth(35)
eststo clear


* TABLE A4
// The models in Table A4 are estimated using penalized maximum likelihood 
// as implemented in the brglm2 R package. The following code prepares and 
// exports the data to be analyzed using replication_code_2.R, which 
// produces Table A4

// Calculates inverse probability of panel survival (2017-18-19)
xtset idcode year
qui reg vim_vox female age i.edu3 dhincome_all livingpartner intpol l2.(vim_vox authoritarian ideol nativism orgterr pop6 msex) ls.(vim_vox authoritarian ideol nativism orgterr pop6 msex) if year==2019
gen s789 = e(sample)
qui logit ret789 female age i.edu3 dhincome_all intpol authoritarian ideol nativism orgterr pop6 msex
predict pr789 if s789, pr
gen ipa789 = 1/(pr789)
sum ipa789
replace ipa789 = ipa789 * (1/r(mean))
bysort idcode: egen w789 = max(ipa789)

// Calculates inverse probability of panel survival (2018-19-20)
xtset idcode year
qui reg vim_vox female age i.edu3 dhincome_all livingpartner intpol l2.(vim_vox authoritarian ideol nativism orgterr pop6 msex) ls.(vim_vox authoritarian ideol nativism orgterr pop6 msex) if year==2020
gen s890 = e(sample)
logit ret890 female age i.edu3 dhincome_all livingpartner intpol authoritarian ideol nativism orgterr pop6 msex
predict pr890 if s890, pr
gen ipa890 = 1/(pr890)
sum ipa890
replace ipa890 = ipa890 * (1/r(mean))
bysort idcode: egen w890 = max(ipa890)

// Exports data
export delimited idcode year vim_vox female age edu3 edu3_1-edu3_3 dhincome_all livingpartner intpol authoritarian ideol nativism orgterr pop6 msexism swim_msex l2* ls* posmsex negmsex w789 w890 using "data_table_a4.csv", nolabel replace


* MEASUREMENT
// Internal reliability of authoritarianism scale across waves
alpha a_respect a_manner a_behave a_obedient if year==2017
alpha a_respect a_manner a_behave a_obedient if year==2018
alpha a_respect a_manner a_behave a_obedient if year==2019
alpha a_respect a_manner a_behave a_obedient if year==2020

// Internal reliability of populism scale across waves
alpha ip_pop* if year==2017
alpha ip_pop* if year==2018
alpha ip_pop* if year==2019
alpha ip_pop* if year==2020

// Internal reliability of Women’s Day protest engagement scale, 2018 wave
alpha p8m_strike p8m_demonst p8m_mobiliz p8m_talked if year==2018


* FIGURE A1
// Exports the data, to be imported using replication_code_2.R, which produces 
// Figure A1
export delimited idcode year msexism using "data_figure_a1.csv", nolabel replace


* TABLE A5
// Applies more informative value labels
lab def income2 1 "Low" 2 "Mid income" 3 "High income"
lab val t1x3hincall income2
lab def partner2 1 "Lives with partner"
lab val t2partner partner2
lab def intpol2 1 "High interest"
lab val t1dintpol intpol2

// Columns 1 and 2 (model 1)
eststo ta5m1: xtmixed msex (i.female g3cohort i.t1edu3 i.t1ideo5 i.t2partner i.t1x3hincall i.t1dintpol i.t1rv16all)##time if nwaves==4 || idcode: i.time, cov(uns)
matrix groups = e(N_g)
estadd scalar Respondents = groups[1,1]

// Columns 3 and 4 (model 2)
eststo ta5m2: xtmixed msex (i.female g3cohort i.t1edu3 i.t1ideo5 i.t2partner i.t1x3hincall i.t1dintpol i.t1rv16all c.t2ip8m)##time if nwaves==4 || idcode: i.time, cov(uns)
matrix groups = e(N_g)
estadd scalar Respondents = groups[1,1]

// Prints Table A5
esttab ta5m1 ta5m2, b(3) se(3) star(+ 0.1 * 0.05 ** 0.01 *** 0.001) wide nobase noomit nogap nomtitle transform(ln*: exp(@) exp(@) at*: tanh(@) (1-tanh(@)^2)) eqlabels("" "sd(2018)" "sd(2019)" "sd(2020)" "sd(Intercept)" "corr(2018, 2019)" "corr(2018, 2020)" "corr(2018, Intercept)" "corr(2019, 2020)" "corr(2019, Intercept)" "corr(2020, Intercept)" "sd(Residual)", none) noeqlines varlabels(msexism:_cons "Constant", blist(lns1_1_1:_cons "Random part {break}")) refcat(1.female "Fixed part" 2.g3cohort "Cohort (ref. 15-29)" 2.t1edu3 "Education (ref. Lower 2ry or less)" 2.t1ideo5 "Ideological identification (ref. Far left)" 2.t1x3hincall "Income (ref. Low)" 1.t1rv16all "Vote in 2016 (ref. PP)" 1.time "Year (ref. 2017)", nolabel) label scalars("Respondents Groups (respondents)") sfmt(0) collabels("b" "(SE)") varwidth(43) title("Table A5. Multilevel growth curve models of modern sexism")
eststo clear


* FIGURE A2
// Data for this figure was produced along with that for Figure 3 above (main 
// text section). Figure A2 itself is produced using replication_code_2.R


* TABLE A6
// Column 1 (2018)
xtset idcode year
eststo ta6m1: logit vim_pp female age i.edu3 dhincome_all livingpartner intpol l.(vim_pp authoritarian ideol nativism orgterr pop6 msex) if year==2018

// Column 2 (2019)
eststo ta6m2: logit vim_pp female age i.edu3 dhincome_all livingpartner intpol l.(vim_pp authoritarian ideol nativism orgterr pop6 msex) if year==2019

// Column 3 (2020)
eststo ta6m3: logit vim_pp female age i.edu3 dhincome_all livingpartner intpol l.(vim_pp authoritarian ideol nativism orgterr pop6 msex) if year==2020

// Prints Table A6
esttab ta6*, b(3) se(3) star(+ 0.1 * 0.05 ** 0.01 *** 0.001) nobase noomit eqlabels(none) rename(L.vim_pp vim_pp L.ideol ideol L.nativism nativism L.orgterr orgterr L.pop6amz pop6amz L.msexism msexism L.authoritarian authoritarian) refcat(vim_pp "Prior values (t-1)", nolabel) varlabels(_cons "Constant") label nonum mtitle(2018 2019 2020) title("Table A6. Effect of prior attitudes and vote intention on intended vote for PP") nogap varwidth(35) 
eststo clear


* TABLE A7
// Column 1 (model 1, 2019)
eststo ta7m1: logit vim_pp female age i.edu3 dhincome_all livingpartner intpol l2.(vim_pp authoritarian ideol nativism orgterr pop6 msex) ls.(vim_pp authoritarian ideol nativism orgterr pop6 msex) if year==2019

// Column 2 (model 2, 2020)
eststo ta7m2: logit vim_pp female age i.edu3 dhincome_all livingpartner intpol l2.(vim_pp authoritarian ideol nativism orgterr pop6 msex) ls.(vim_pp authoritarian ideol nativism orgterr pop6 msex) if year==2020

// Column 3 (model 3, 2019)
eststo ta7m3: logit vim_pp female age i.edu3 dhincome_all livingpartner intpol l2.(vim_pp authoritarian ideol nativism orgterr pop6 msex) ls.(vim_pp authoritarian ideol nativism orgterr pop6) posmsex negmsex if year==2019

// Column 4 (model 4, 2020)
eststo ta7m4: logit vim_pp female age i.edu3 dhincome_all livingpartner intpol l2.(vim_pp authoritarian ideol nativism orgterr pop6 msex) ls.(vim_pp authoritarian ideol nativism orgterr pop6) posmsex negmsex if year==2020

// Prints Table A7
esttab ta7*, b(3) se(3) star(+ 0.1 * 0.05 ** 0.01 *** 0.001) nobase noomit eqlabels(none) rename(L2.ideol ideol L2.nativism nativism L2.orgterr orgterr L2.pop6amz pop6amz L2.msexism msexism L2.authoritarian authoritarian) refcat(L2.vim_pp "Prior values (t-2)" LS.vim_pp "Prior change (t–2 minus t–1)", nolabel) varlabels(L2.vim_pp "PP intention" LS.vim_pp "PP intention"LS.ideol "Ideological identification" LS.nativism "Nativism" LS.orgterr "Territorial preference" LS.authoritarian "Authoritarianism" LS.pop6amz "Populsim" LS.msexism "Sexism" posmsex "Increase in sexism" negmsex "Decrease in sexism" _cons "Constant") label mtitle(2019 2020 2019 2020) title("Table A7. Effect of prior change in attitudes and vote intention on intended vote for PP") nogap varwidth(35) 
eststo clear


* TABLE A8
// Column 1 (2019)
eststo ta8m1: logit vim_vox female age i.edu3 dhincome_all livingpartner intpol l.(vim_vox authoritarian ideol nativism orgterr pop6 swim_msex) if year==2019

// Column 2 (2020)
eststo ta8m2: logit vim_vox female age i.edu3 dhincome_all livingpartner intpol l.(vim_vox authoritarian ideol nativism orgterr pop6 swim_msex) if year==2020

// Prints Table A8
esttab ta8*, b(3) se(3) star(+ 0.1 * 0.05 ** 0.01 *** 0.001) nobase noomit eqlabels(none) rename(L.vim_vox vim_vox L.ideol ideol L.nativism nativism L.orgterr orgterr L.pop6amz pop6amz L.swim_msex msexism L.authoritarian authoritarian) refcat(vim_vox "Prior values (t-1)", nolabel) varlabels(_cons "Constant") label nonum mtitle(2019 2020) title("Table A8. Effect of prior attitudes on intended vote for Vox, using the original modern sexism scale") nogap varwidth(35) 
eststo clear


* TABLE A9
// The models in Table A9 are estimated using penalized maximum likelihood 
// as implemented in the brglm2 R package. The necessary data was exported above
// to produce Table 3 (main text section). Table A9 itself is produced using 
// replication_code_2.R



