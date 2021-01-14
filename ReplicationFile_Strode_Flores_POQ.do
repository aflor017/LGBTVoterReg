************************
**Replication file for: Voter Registration Rates & Traits by Sexual Orientation and Gender Expression
**Dakota Strode & Andrew R. Flores

**Download the 2016 & 2018 CCES data from the Harvard DataVerse: https://cces.gov.harvard.edu/
**Extract files/store them where necesary, change the directory to that location:

cd "C:\Documents\"

use "cces18_common_vv.dta", clear
gen year = 2018
*append the datasets

append using "CCES16_Common_OUTPUT_Feb2018_VV.dta", force

**Data preprocessing
gen my_registered = 1 if CL_matched=="Y" & CL_voterstatus=="active"
recode my_registered (.=0) if (CL_matched=="Y" & CL_voterstatus!="active") | votereg_post==2
recode my_registered (.=0) if votereg == 2
gen myreg_rev = my_registered
recode myreg_rev (0=1) if CL_voterstatus == "inactiv" | CL_voter_status == 3

gen lgb_group = .
replace lgb_group = 1 if sexuality==1 & gender==1
replace lgb_group = 2 if sexuality==1 & gender==2
replace lgb_group = 3 if sexuality==2 
replace lgb_group = 4 if sexuality==3 
replace lgb_group = 5 if sexuality==4 & gender==1
replace lgb_group = 6 if sexuality==4 & gender==2

label define lgb_group 1 "Straight Men" 2 "Straight Women" 3 "Lesbians" 4 "Gay Men" 5 "BiMen" 6 "BiWomen"
label values lgb_group lgb_group

gen lgb_group2 = lgb_group

recode lgb_group2 (. = 7) if (sexuality==5 | sexuality==6) & (gender==1)
recode lgb_group2 (.=8) if (sexuality==5 | sexuality==6) & (gender==2)

label define lgb_group 7 "OtherMen" 8 "OtherWomen", add

label values lgb_group2 lgb_group

gen new_trans = trans
recode new_trans (2=0) (3/9=.)

gen new_trans2 = new_trans
recode new_trans2 (.=2) if trans==3

label define new_trans 0 "Cisgender" 1 "Transgender" 2 "Other"
label values new_trans2 new_trans

gen age = 2016 - birthyr

**demographics 
*simplify
recode educ (1/4 = 0 "NoColl") (5/6 = 1 "BA"), gen(collgrad)
 recode educ (1/2 = 1 "HS or Less") (3/4 = 2 "Some College") (5 = 3 "4-year") (6 = 4 "Post-Grad"), gen(educ_2)

recode employ (1/2 = 1 "Employed") (3/4 6/9 = 2 "UnempOut") (5 = 3 "Retired"), gen(employ_3)

gen homeowner = ownhome==1

gen inc_3 = 1 if faminc>=1 & faminc<=3
replace inc_3 =2 if faminc>=4 & faminc<=6
replace inc_3 = 3 if faminc>=7 & faminc<=31

replace inc_3 = 1 if faminc_new>=1 & faminc_new<=3
replace inc_3 = 2 if faminc_new>=4 & faminc_new<=6
replace inc_3 = 3 if faminc_new>=7 & faminc_new<=16
label define inc_3 1 "<$30K" 2 "$30K-$60K" 3 "60K+"
label values inc_3 inc_3

gen income2 = faminc
recode income2 (12/31 = 12) (97/98=97)
label values income2 FAMINC

replace income2 = faminc_new if income2==. & faminc_new >=1 & faminc_new<=11
replace income2 = 12 if income2==. & faminc_new >=12 & faminc_new<=16
replace income2 = 97 if income2==. & faminc_new==97
 
recode marstat (1/4 = 1 "Ever Married") (5 = 2 "Single") (6=3 "DomesticPartner") (8=.), gen(marital)

recode age (18/29 = 1 "18-29") (30/54 = 2 "30-54") (55/99=3 "55+"), gen(age_cat)
recode urbancity (1=1 "Urban") (2=2 "Suburban") (3/5 = 3 "Rural"), gen(urban_2)

gen race_2 = race
recode race_2 (5/8 = 7)
label values race_2 RACE
 
recode race_2 (5=4), gen(race_3)
label define race_3 1 "White" 2 "Black" 3 "Latino" 4 "Other"
label values race_3 race_3

**weights!
gen analytic_weight = .
replace analytic_weight = commonweight_vv_lgbt
replace analytic_weight = commonweight if year==2018

svyset _n [pweight=analytic_weight], vce(linearized) singleunit(missing)

****************************
*Table 1: Summary breakdown*
****************************

svy: tab lgb_group2 year, col ci obs format(%12.3f)
svy: tab new_trans2 year, col ci obs format(%12.3f)

***************************
******Tables 2 & 3*********
******Tables S2.2 & S2.3*******
 
set more off
foreach var of varlist race_3 age_cat collgrad marital inc_3 urban_2 {
svy: tab `var' lgb_group2, col obs ci format(%12.3f)
di `e(df2_Pear)'
svy: tab `var' new_trans2, col obs ci format(%12.3f)
di `e(df2_Pear)'
}

svy: tab gender new_trans2, col obs ci format(%12.3f)
di `e(df2_Pear)'

*************************
**Supporting Information*
*************************

*S1

*Table S1.1
svyset _n [pweight=commonweight], vce(linearized) singleunit(missing)

svy, subpop(if year==2016): tab myreg_rev lgb_group2 if year==2016,  col obs format(%12.3f)
di `e(df2_Pear)'
svy, subpop(if year==2016): tab myreg_rev new_trans2 if year==2016,  col obs format(%12.3f)
di `e(df2_Pear)'


tab myreg_rev lgb_group2 if year==2016, col chi
tab myreg_rev new_trans2 if year==2016, col chi

tab myreg_rev lgb_group2 if year==2018, col chi
tab myreg_rev new_trans2 if year==2018, col chi

*Tasble S1.2
eststo clear

eststo: svy, subpop(if year==2016): logit myreg_rev i.lgb_group2 i.new_trans2 if year==2016
eststo: svy, subpop(if year==2016): logit myreg_rev i.lgb_group2 i.new_trans2 i.educ_2 age i.race_2 i.employ_4 i.homeowner i.income2 if year==2016
eststo: logit myreg_rev i.lgb_group2 i.new_trans2 if year==2016
eststo: logit myreg_rev i.lgb_group2 i.new_trans2 i.educ_2 age i.race_2 i.employ_4 i.homeowner i.income2 if year==2016
eststo: logit myreg_rev i.lgb_group2 i.new_trans2 if year==2018
eststo: logit myreg_rev i.lgb_group2 i.new_trans2 i.educ_2 age i.race_2 i.employ_2 i.homeowner i.income i.urban_2 if year==2018
esttab using "lgbt_regmodelaux.csv", se(a2) scalars(F df_m df_r) b(a2) interaction(" X ") label nobaselevels star(+ 0.20 ^ 0.10 * 0.05 ** 0.01 *** 0.001) replace
eststo clear

*S2

*Table S2.1
svyset _n [pweight=analytic_weight], vce(linearized) singleunit(missing)

svy: tab lgb_group2 new_trans2, col ci obs  format(%12.3f)

*Table S2.4
*Table S2.5
foreach var of varlist race_2 age_cat educ_2 marstat income2 employ_3 {
svy, subpop(if year==2016): tab `var' lgb_group if year==2016, col obs ci format(%12.3f)
di `e(df2_Pear)'
svy, subpop(if year==2018): tab `var' lgb_group if year==2018, col obs ci format(%12.3f)
di `e(df2_Pear)'
svy, subpop(if year==2016): tab `var' new_trans if year==2016, col obs ci format(%12.3f)
di `e(df2_Pear)'
svy, subpop(if year==2018): tab `var' new_trans if year==2018, col obs ci format(%12.3f)
di `e(df2_Pear)'
}

svy, subpop(if year==2018): tab urban_2 lgb_group if year==2018, col obs ci format(%12.3f)
di `e(df2_Pear)'

svy, subpop(if year==2018): tab urban_2  new_trans if year==2018, col obs ci format(%12.3f)
di `e(df2_Pear)'

*S3

*Table S3.1
*Table S3.2
foreach var of varlist race_3 age_cat collgrad marital inc_3 urban_2 {
svy: tab `var' lgb_group2, row obs format(%12.3f)
svy: tab `var' new_trans2, row obs format(%12.3f)
}


*S4

*Table S4.1
set more off
eststo: svy, subpop(if year==2016): logit myreg_rev i.lgb_group2 i.new_trans2 if year==2016
eststo: svy, subpop(if year==2016): logit myreg_rev i.lgb_group2 i.new_trans2 i.educ_2 age i.race_2 i.employ_4 i.homeowner i.income2 if year==2016
eststo: svy, subpop(if year==2018): logit myreg_rev i.lgb_group2 i.new_trans2  if year==2018
eststo: svy, subpop(if year==2018): logit myreg_rev i.lgb_group2 i.new_trans2 i.educ_2 age i.race_2 i.employ_2 i.homeowner i.income i.urban_2 if year==2018

esttab using "lgbt_regmodelsrr.csv", se(a2) scalars(F df_m df_r) b(a2) interaction(" X ") label nobaselevels star(+ 0.20 ^ 0.10 * 0.05 ** 0.01 *** 0.001) replace
eststo clear

*S5: repeats information from prior demographics tables

*S6

*Table S6.1
gen regist = .
recode regist (.=1) if votereg==1 | votereg_f==1
recode regist (.=0) if (votereg==2 | votereg==3) | (votereg_f==2)
recode regist (1=0) if votereg_f==2

svy, subpop(if year==2016): tab regist lgb_group2 if year==2016, col obs format(%12.4f)
di `e(df2_Pear)'
svy, subpop(if year==2016): tab regist new_trans2 if year==2016, col obs format(%12.4f)
di `e(df2_Pear)'

svy, subpop(if year==2018): tab regist lgb_group2 if year==2018, col obs format(%12.4f)
di `e(df2_Pear)'
svy, subpop(if year==2018): tab regist new_trans2 if year==2018, col obs format(%12.4f)
di `e(df2_Pear)'

*Table S6.2
eststo: svy, subpop(if year==2016): logit regist i.lgb_group2 i.new_trans2 if year==2016
eststo: svy, subpop(if year==2016): logit regist i.lgb_group2 i.new_trans2 i.educ_2 age i.race_2 i.employ_4 i.homeowner i.income2 if year==2016
eststo: svy, subpop(if year==2018): logit regist i.lgb_group2 i.new_trans2  if year == 2018
eststo: svy, subpop(if year==2018): logit regist i.lgb_group2 i.new_trans2 i.educ_2 age i.race_2 i.employ_2 i.homeowner i.income i.urban_2 if year==2018

esttab using "C:\Users\aflor\Downloads\lgbt_regaux.csv", se(a2) scalars(F df_m df_r) b(a2) interaction(" X ") label nobaselevels star(+ 0.20 ^ 0.10 * 0.05 ** 0.01 *** 0.001) replace
eststo clear

*S7

*Table S7.1

*2016
svy: tab CL_voterstatus
di .8855+.0118
*2018
svy: tab CL_voter_status
di .8898+.0137

tab myreg_rev [iweight=analytic_weight] if year==2016, mis
tab myreg_rev [iweight=analytic_weight] if year==2018, mis
