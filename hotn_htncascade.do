** --------------------------------------------------------------------------------------------------------------------
**  GENERAL DO-FILE COMMENTS
**  program:     	moh_report_00
**  project:      	Health of the Nation (2011 - 2012)
**  author:       	HOWITT / 01-APRIL-2015; Alvarado 9/4/2015
**  Details			ANALYSIS PERFORMED TO PRODUCE PREVALENCE OF HYPERTENSION AND SUB-CATEGORIES OF DIAGNOSED, TREATED
**					AND CONTROLLED

**				Original DO files are:
**					C.Howitt --> (2015-02-11_hotn1_report.do, version 5, 12-Jan-2015)
**
**				Dataset is HOTN V4.1 (prepared by Hambleton)
** 				With additional RPAQ variables (prepared by HOWITT
** --------------------------------------------------------------------------------------------------------------------

** General algorithm set-up
version 15
clear all
macro drop _all
set more 1
set linesize 80

** Set working directories: this is for DATASET and LOGFILE import and export
** DATASETS to encrypted SharePoint folder
local datapath "X:\The University of the West Indies\DataGroup - repo_data\data_p119\version01\1-input"
** LOGFILES to unencrypted OneDrive folder
local logpath "X:\OneDrive - The University of the West Indies\repo_datagroup\repo_p119"

*Load HOTN CORE dataset
use "`datapath'\hotn_v41RPAQ.dta", clear



** --------------------------------------------------------------------------------------------------------------------
** DATA PREPARATION
** --------------------------------------------------------------------------------------------------------------------

* THREE AGE GROUPS
* AGE IN 3 BANDS (25-44, 45-64, 65+)
	gen age_gr2 =.
	replace age_gr2= 1 if agey >=25 & agey <45
	replace age_gr2= 2 if agey >=45 & agey <65
	replace age_gr2= 3 if agey >=65 & agey <.
	label variable age_gr2 "Age in 3 bands"
	label define age_gr2 1 "25 - <45 years" 2 "45 - <65 years" 3 "65 and over years"
	label values age_gr2 age_gr2
	order age_gr2, after(agey)

** SEX indicators
	gen female = (sex==1) if !missing(sex)
	gen male = (sex==2) if !missing(sex)

** AGE indicators
	gen age25 = (age_gr2==1) if !missing(age_gr2)
	gen age45 = (age_gr2==2) if !missing(age_gr2)
	gen age65 = (age_gr2==3) if !missing(age_gr2)

*education category: two different education category groupings are created. The first places "technical/secretarial after primary school" in group 3, and the
*second places it in group 1.

	gen educ_gr1=.
	replace educ_gr1 = 1 if educ ==1 | educ ==2 | educ==3
	replace educ_gr1 =2 if educ ==4
	replace educ_gr1 = 3 if educ ==5 | educ ==6 | educ==7
	replace educ_gr1 = 4 if educ ==8 | educ ==9
	label define educ_gr1 1 "Group 1: less than secondary school completed" 2 "Group 2: secondary school completed" 3 "Group 3: technical, secretarial or teacher training" 4 "Group 4: university education "
	label values educ_gr1 educ_gr1
	label variable educ_gr1 "education category 1"

	gen educ_gr2=.
	replace educ_gr2 = 1 if educ ==1 | educ ==2 | educ==3 | educ==5
	replace educ_gr2 =2 if educ ==4
	replace educ_gr2 = 3 if educ ==6 | educ==7
	replace educ_gr2 = 4 if educ ==8 | educ ==9
	label define educ_gr2 1 "Group 1: less than secondary school completed, inc tech after primary school" 2 "Group 2: secondary school completed" 3 "Group 3: technical, secretarial or teacher training after secondary school" 4 "Group 4: university education "
	label values educ_gr2 educ_gr2
	label variable educ_gr2 "education category 2"

	/* notes educ_gr1: There are two possible education groupings that will be used for HotN analysis. For educ_gr1, "technical/secretarial after primary school" ///
	is included in group 3 (technical, trade or teacher education)
	notes educ_gr2: There are two possible education groupings that will be used for HotN analysis. In educ_gr2, "technical/secretarial after primary school" ///
	is included in group 1 (less than secondary school)
	 */

*occupation groups
*occupation groups - including last occupation if retired
	replace occg = occg_retired  if occg_retired<.
	gen occ_grade =.
	replace occ_grade = 1 if occg == 6 | occg == 7 | occg ==8 | occg==9
	replace occ_grade = 2 if occg == 3 | occg == 4 | occg ==5
	replace occ_grade = 3 if occg ==1 | occg ==2
	replace occ_grade = 4 if pocc !=1 & pocc !=6
	replace occ_grade = 5 if occ_grade ==.
	label variable occ_grade "occupation grade"
	label define occ_grade 1 "Routine/manual" 2 "Intermediate" 3 "Professional" 4 "Not in employment or retired" 5 "Unable to code"
	label values occ_grade occ_grade
	/* notes occ_grade: For retired people, includes last occupation before retirement, where available
	notes occ_grade: Category 4  includes people on maternity or sick leave; people looking for work; home duties; students; incapacited people
	notes occ_grade: Category 5 includes people who reported occupation, but that occupation was not listed in BARSOC coding, as well as retired people ///
	who could not be contacted to determine occupation prior to retirement */


** --------------------------------------------------------------------------------------------------------------------
** Full survey weighting
** --------------------------------------------------------------------------------------------------------------------
	svyset ed [pweight=wfinal1_ad], strata(region)



** --------------------------------------------------------------------------------------------------------------------
** HYPERTENSION
** Table 20: Mean systolic and diastolic blood pressure in the Barbadian population aged 25 years and over
** Table 21: Proportion of the Barbadian population aged 25 years and over who were on medication for hypertension (n= 367)
** Table 22: Prevalence of known hypertension with sub-optimal control in the Barbadian population aged 25 years and over
** Table 23: Total prevalence of hypertension* in the Barbadian population aged 25 years and over
** --------------------------------------------------------------------------------------------------------------------

**average systolic and diastolic pressure taken from 2nd and 3rd readings (1st reading discarded)
	egen a_sbp = rowmean ( sbp2 sbp3)
	egen a_dbp = rowmean ( dbp2 dbp3)

*******************************************************************
*** EXCLUSIONS
*******************************************************************
	drop if a_sbp==.
	drop if pregnant==1


** TABLE 20. Mean SBP and DBP
	svy, subpop(age25): mean a_sbp, over(sex)
	svy, subpop(age25): mean a_sbp
	svy, subpop(age45): mean a_sbp, over(sex)
	svy, subpop(age45): mean a_sbp
	svy, subpop(age65): mean a_sbp, over(sex)
	svy, subpop(age65): mean a_sbp
	svy: mean a_sbp, over(sex)
	svy: mean a_sbp
	svy, subpop(age25): mean a_dbp, over(sex)
	svy, subpop(age25): mean a_dbp
	svy, subpop(age45): mean a_dbp, over(sex)
	svy, subpop(age45): mean a_dbp
	svy, subpop(age65): mean a_dbp, over(sex)
	svy, subpop(age65): mean a_dbp
	svy: mean a_dbp, over(sex)
	svy: mean a_dbp



***************************************************************
*** GENERATE INDICATORS
***************************************************************

** On treatment for hypertension
	gen bp_trt = 1
	replace bp_trt = 2 if hyperm==1
	lab var bp_trt "On_trt_for_raised_bp"
	lab val bp_trt ny

** Blood pressure check
	gen bp_hyp=.
	replace bp_hyp=1 if a_dbp < 90 & a_sbp <140
	replace bp_hyp=2 if bp_hyp !=1 & a_dbp !=. & a_sbp!=.
	lab var bp_hyp "BP >= 140/90"
	lab def ny 1 "No" 2 "Yes"
	lab val bp_hyp ny


*** Those with a self report of hypertension and self report of medicine and good BP measurements
** Controlled hypertension is defined as SBP <140 mm Hg and DBP <90 mm Hg among persons with hypertension. what about pregnant women?
	gen bpcontrol=.
	replace bpcontrol =1 if bp_trt==2 & bp_hyp==2
	replace bpcontrol =2 if bp_trt==2 & bp_hyp==1
	lab var bpcontrol "Diagnosed_&_meds,_with_good_control"
	lab val bpcontrol ny

***  diagnosed, not on meds & uncontrolled
	gen nomedsuncontrol =.
	replace nomedsuncontrol =1 if hyper==1  & hyperm ==1
	replace nomedsuncontrol =2 if hyper==1  & hyperm !=1 & bp_hyp==2
	lab var nomedsuncontrol "Diagnosed_no_meds_&_uncontrolled"
	lab val nomedsuncontrol ny

*** Those with bad BP and no self report of Hypertension
	gen undiaghyp=.
	replace undiaghyp =1 if  hyperm==1 | (bp_hyp==2 & hyper==1)
	replace undiaghyp =2 if bp_hyp ==2 & hyper==2
	lab var undiaghyp "Undiagnosed"
	lab val undiaghyp ny

** Non-hypertensive - no self report of hypertesnion and good bp.
** what's the right definition? *i.e. on medication for hypertension and/or blood pressure >140 mmHg systolic and/or 90 mmHg diastolic.?
	gen nohyp =.
	replace nohyp = 1 if  bp_hyp==2 | (hyper==1 & hyperm ==1)
	replace nohyp = 2 if nohyp!=1
	lab var nohyp "Not_hypertensive"
	lab val nohyp ny

** prevalence of undiagnosed hypertension
	gen  undiaghypprev =1
	replace undiaghypprev =2 if bp_hyp ==2 & hyper!=1
	lab var undiaghypprev "Undiagnosed_Prev"
	lab val undiaghypprev ny

** proportion getting meds of all diagnosed?
	gen gettingmeds =.
	replace gettingmeds=1 if bp_hyp==2 & hyper==1
	replace gettingmeds =2 if hyperm==1
	lab var gettingmeds "Diagnosed_getting_meds"
	lab val gettingmeds ny

** Proportion diagnosed correctly?
	gen diag=.
	replace diag =1 if hyper!=1
	replace diag =1 if hyper==1 & hyperm!=1 & bp_hyp!=2
	replace diag =2 if hyper==1 & (bp_hyp==2 | hyperm==1)
	lab var diag "Diagnosed_prev"
	lab val diag ny

** Proportion controlled out of all hypertensives
	gen propcontrolled =.
	replace propcontrolled =1 if nohyp==1
	replace propcontrolled =2 if bpcontrol ==2
	lab var propcontrolled "Those controlled out of all hypertensives"
	lab val propcontrolled ny


** prevalence of those getting treated compared to population
	gen prevtreat =1
	replace prevtreat =2 if hyperm==1
	lab var prevtreat "Those treated out of all"
	lab val prevtreat ny


** Prevalence of those controlled out of whole population
	gen prevcontrol =1
	replace prevcontrol =2 if bp_trt==2 & bp_hyp==1
	lab var prevcontrol "Those controlled out of all"
	lab val prevcontrol ny

** Generating specific indicators we need
	gen diagnosed =.
	replace diagnosed =0 if nohyp==1
	replace diagnosed =1 if undiaghyp==1

	gen onmeds =.
	replace onmeds =0 if nohyp==1
	replace onmeds =1 if gettingmeds==2


** Generating indicators for regressions
	gen medsofthosediag =gettingmeds-1

	gen propcontrolofmeds =.
	replace propcontrolofmeds =0 if onmeds==1
	replace propcontrolofmeds =1 if propcontrolled==1 & onmeds==1

** prevalence of those diagnosed but uncontrolled
	gen diagnocontrol =1
	replace diagnocontrol =2 if diag==2  & bp_hyp==2
	lab var diagnocontrol "Diagnosed_&_uncontrolled_prev"
	lab val diagnocontrol ny


	tempfile data
	save `data', replace




****************************************************************

	use `data', clear
	svy: tab nohyp sex, percent ci row


	svy: mean  agey,  over(nohyp)


	** looks like it would be better if we could map to years of school
	gen educy=.
	replace educy=0 if educ==1
	replace educy=3 if educ==2
	replace educy=6 if educ==3
	replace educy=11 if educ==4
	replace educy=8 if educ==5
	replace educy=13 if educ==6
	replace educy=13 if educ==7
	replace educy=15 if educ==8
	replace educy=17 if educ==9

	svy: mean  educy,  over(nohyp)

**  TABLE TWO. Prevalence of hypertension - out of whole population

	svy, subpop(age25): tab  nohyp sex , percent ci col
	tabout  nohyp sex using "table2.csv" if age25==1 , svy replace cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age25)

	svy, subpop(age45): tab  nohyp sex, percent ci row
	tabout  nohyp sex using "table2.csv" if age45==1, svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age45)

	svy, subpop(age65): tab  nohyp sex , percent ci row
	tabout nohyp  sex  using "table2.csv" if age65==1, svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age65)

	svy: tab  nohyp sex, percent ci col
	tabout    nohyp sex using "table2.csv", svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(all)

** TABLE TWO B. Prevalence of diagnosed but not controlled
	svy, subpop(age25): tab  diagnocontrol sex , percent ci col
	tabout  diagnocontrol sex using "table2b.csv" if age25==1 , svy replace cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age25)

	svy, subpop(age45): tab  diagnocontrol sex, percent ci row
	tabout  diagnocontrol sex using "table2b.csv" if age45==1, svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age45)

	svy, subpop(age65): tab  diagnocontrol sex , percent ci row
	tabout diagnocontrol  sex  using "table2b.csv" if age65==1, svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age65)

	svy: tab  diagnocontrol sex, percent ci col
	tabout    diagnocontrol sex using "table2b.csv", svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(all)

** TABLE TWO C. Prevalence of  undiagnosed
	svy, subpop(age25): tab  undiaghypprev sex , percent ci col
	tabout  undiaghypprev sex using "table2c.csv" if age25==1 , svy replace cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age25)

	svy, subpop(age45): tab  undiaghypprev sex, percent ci row
	tabout  undiaghypprev sex using "table2c.csv" if age45==1, svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age45)

	svy, subpop(age65): tab  undiaghypprev sex , percent ci row
	tabout undiaghypprev  sex  using "table2c.csv" if age65==1, svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age65)

	svy: tab  undiaghypprev sex, percent ci col
	tabout    undiaghypprev sex using "table2c.csv", svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(all)

** Table THREE A. cascade by age, 25
	svy, subpop(age25): tab  diagnosed sex , percent ci col
	tabout  diagnosed sex  using "table3.csv" if age25==1, svy replace cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age25)

	svy, subpop(age25): tab  onmeds  sex, percent ci col
	tabout  onmeds sex  using "table3.csv" if age25==1, svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age25)

	svy, subpop(age25): tab  propcontrolled sex , percent ci col
	tabout  propcontrolled sex  using "table3.csv" if age25==1, svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age25)

** Table THREE B. cascade by age, 45
	svy, subpop(age45): tab  diagnosed sex , percent ci col
	tabout  diagnosed sex  using "table3.csv" if age45==1, svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age45)

	svy, subpop(age45): tab  onmeds  sex, percent ci col
	tabout  onmeds sex  using "table3.csv" if age45==1, svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age45)

	svy, subpop(age45): tab  propcontrolled sex , percent ci col
	tabout  propcontrolled sex  using "table3.csv" if age45==1, svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age45)


** Table THREE C. cascade by age, 65
	svy, subpop(age65): tab  diagnosed sex , percent ci col
	tabout  diagnosed sex  using "table3.csv" if age65==1, svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age65)

	svy, subpop(age65): tab  onmeds  sex, percent ci col
	tabout  onmeds sex  using "table3.csv" if age65==1, svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age65)

	svy, subpop(age65): tab  propcontrolled sex , percent ci col
	tabout  propcontrolled sex  using "table3.csv" if age65==1, svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(age65)

**  TABLE THREE D.  Overall cascade
	svy: tab  diagnosed sex, percent ci col
	tabout  diagnosed  sex using "table3.csv", svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(all)

	svy: tab  onmeds sex , percent ci col
	tabout onmeds sex  using "table3.csv", svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(all)

	svy: tab  propcontrolled sex, percent ci col
	tabout  propcontrolled sex using "table3.csv", svy append cells(col ci) format(1 1 1) percent style(csv) lay(row) h1(all)


********************************************************************
** GRAPHING CODE
*******************************************************************

** Cascasde by gender - 4 categories:
** all with hypertension
** all diagnosed
** all treated
** all controlled
** EDITING TO match cascade proportions instead.

** No uncertainty graphic first

** TABLE EIGHT: Prevalence of diagnosed hypertension.
	svy: tab diagnosed sex, percent ci col
	tabout diagnosed sex using "diag.csv", svy replace  cells(col lb ub) format(3 3 3) style(csv) h2( status | femaleavg | femalelb | femaleub | maleavg | malelb | maleub |  totalavg | totallb | totalub)

** Prevalence of treated hypertension.
	svy: tab onmeds sex, percent ci col
	tabout onmeds sex using "treat.csv", svy replace  cells(col lb ub) format(3 3 3) style(csv) h2( status | femaleavg | femalelb | femaleub | maleavg | malelb | maleub |  totalavg | totallb | totalub)

** Prevalence of controlled hypertension.
	svy: tab propcontrolled sex, percent ci col
	tabout propcontrolled sex using "control.csv", svy replace  cells(col lb ub) format(3 3 3) style(csv) h2( status | femaleavg | femalelb | femaleub | maleavg | malelb | maleub |  totalavg | totallb | totalub)


** Bring in all data

	insheet using "diag.csv", comma clear
	drop in 1
	outsheet using "diag.csv", comma replace nonames
	insheet using "diag.csv", clear comma names
	save "`datapath'\diag.dta", replace

	insheet using "treat.csv", comma clear
	drop in 1
	outsheet using "treat.csv", comma replace nonames
	insheet using "treat.csv", clear comma names
	save "`datapath'\treat.dta", replace

	insheet using "control.csv", comma clear
	drop in 1
	outsheet using "control.csv", comma replace nonames
	insheet using "control.csv", clear comma names
	save "`datapath'\control.dta", replace

	use "`datapath'\diag.dta", clear
	append using "`datapath'\treat.dta"
	append using "`datapath'\control.dta"


** continue to format
	gen var=status
	replace var ="" if var=="No" | var=="Yes" | var=="Total" | var=="0" | var=="1"
	carryforward var, replace
	drop if status==var
	destring female*, replace force
	destring male*, replace force
	destring total*, replace force
	drop if status=="Total"


	gen zero = 0
	replace maleavg=maleavg*100
	replace malelb = malelb*100
	replace maleub=maleub*100
	replace femaleavg=femaleavg*100
	replace femalelb = femalelb*100
	replace femaleub=femaleub*100
	replace maleavg = maleavg*-1
	egen test = min(maleavg)
	replace test = test-1

	gen order =.
	set obs 7
	replace order =4 in 7
	replace femaleavg=100 in 7
	replace maleavg=-100 in 7
	replace order =3 if status=="1" & var =="diagnosed"
	replace order =2 if status=="1" & var =="onmeds"
	replace order =1 if status=="Yes" & var =="Those controlled out of all hypertensives"




	gen malelabel=-1*maleavg
	format malelabel %10.1f
	format femaleavg %10.1f

	**twoway bar maleavg order, horizontal xvarlab(Males) barwidth(.6) || bar  femaleavg order , barwidth(.6) horizontal xvarlab(Females)  xtitle("Percent of Population") ytitle("")  xlabel(-80 "80%" -40 "40%"0 "0%" 40 "40%" 80 "80%", tlength(0) grid gmin gmax) legend(label(1 Males) label(2 Females)) legend(order(1 2)) xscale(range(-80 -40 0 40 80)) ylabel(1 "Controlled" 2 "Treated" 3 "Diagnosed" 4 "Hypertensive", angle(0)) title("Hypertension Cascade") || scatter  order maleavg if order>1, mlabel(malelabel) msymbol(none) mlabcolor(white) mlabsize(medlarge) || scatter  order femaleavg if order>1, mlabel(femaleavg) msymbol(none) mlabcolor(white) mlabsize(medlarge) mlabp(9) || scatter  order maleavg if order==1, mlabel(malelabel) msymbol(none) mlabcolor(black) mlabsize(medlarge) mlabp(9) || scatter  order femaleavg if order==1, mlabel(femaleavg) msymbol(none) mlabcolor(white) mlabsize(medlarge) mlabp(9)

	twoway bar maleavg order, horizontal xvarlab(Males) barwidth(.6) || bar  femaleavg order , barwidth(.6) horizontal xvarlab(Females)  xtitle("Percent of Population") ytitle("")  xlabel(-100 "100%" -50 "50%"0 "0%" 50 "50%" 100 "100%", tlength(0) grid gmin gmax) legend(label(1 Males, N=206) label(2 Females, N=340)) legend(order(1 2)) xscale(range(-100 -50 0 50 100)) ylabel(1 "Controlled" 2 "Treated" 3 "Diagnosed" 4 "Hypertensive", angle(0)) title("Hypertension Cascade") || scatter  order maleavg, mlabel(malelabel) msymbol(none) mlabcolor(white) mlabsize(medlarge) || scatter  order femaleavg , mlabel(femaleavg) msymbol(none) mlabcolor(white) mlabsize(medlarge) mlabp(9)



	graph export "cascade_sex_revised_scaled.png", replace height(6000) width(8000)
	graph export "cascade_sex_revised_small_scaled.png", replace height(600) width(800)

	replace maleavg = maleavg*-1
	gen maleorder = order +.2
	replace order = order*-1
	replace maleorder = maleorder*-1
	twoway scatter femaleavg order , mcolor(red)|| rcap femalelb femaleub order, lcolor(red) || scatter maleavg maleorder, mcolor(blue) || rcap malelb maleub maleorder, lcolor(blue) xlabel(-1 "Controlled" -2 "Treated" -3 "Diagnosed" -4 "Hypertensive") legend(label(1 Females) label(3 Males)) legend(order(1 3)) ylabel(,angle(0)) ytitle("Percent") title("Hypertensive Population")

	graph export "uncertainty_sex.png", replace height(6000) width(8000)
	graph export "uncertainty_sex_small.png", replace height(600) width(800)

***********************************************************
*** AGES 25-44
***********************************************************
	use `data', clear


** TABLE EIGHT: Prevalence of diagnosed hypertension.
	svy, subpop(age25): tab diagnosed sex, percent ci col
	tabout diagnosed sex using "diag25.csv" if age25==1, svy replace  cells(col lb ub) format(3 3 3) style(csv) h2( status | femaleavg | femalelb | femaleub | maleavg | malelb | maleub |  totalavg | totallb | totalub)

** Prevalence of treated hypertension.
	svy, subpop(age25): tab onmeds sex, percent ci col
	tabout onmeds sex using "treat25.csv" if age25==1, svy replace  cells(col lb ub) format(3 3 3) style(csv) h2( status | femaleavg | femalelb | femaleub | maleavg | malelb | maleub |  totalavg | totallb | totalub)

** Prevalence of controlled hypertension.
	svy, subpop(age25): tab propcontrolled sex, percent ci col
	tabout propcontrolled sex using "control25.csv" if age25==1, svy replace  cells(col lb ub) format(3 3 3) style(csv) h2( status | femaleavg | femalelb | femaleub | maleavg | malelb | maleub |  totalavg | totallb | totalub)


** Bring in all data

	insheet using "diag25.csv", comma clear
	drop in 1
	outsheet using "diag25.csv", comma replace nonames
	insheet using "diag25.csv", clear comma names
	save "`datapath'\diag25.dta", replace

	insheet using "treat25.csv", comma clear
	drop in 1
	outsheet using "treat25.csv", comma replace nonames
	insheet using "treat25.csv", clear comma names
	save "`datapath'\treat25.dta", replace

	insheet using "control25.csv", comma clear
	drop in 1
	outsheet using "control25.csv", comma replace nonames
	insheet using "control25.csv", clear comma names
	save "`datapath'\control25.dta", replace

	use "`datapath'\diag25.dta", clear
	append using "`datapath'\treat25.dta"
	append using "`datapath'\control25.dta"


** continue to format
	gen var=status
	replace var ="" if var=="No" | var=="Yes" | var=="Total" | var=="0" | var=="1"
	carryforward var, replace
	drop if status==var
	destring female*, replace force
	destring male*, replace force
	destring total*, replace force
	drop if status=="Total"


	gen zero = 0
	replace maleavg=maleavg*100
	replace malelb = malelb*100
	replace maleub=maleub*100
	replace femaleavg=femaleavg*100
	replace femalelb = femalelb*100
	replace femaleub=femaleub*100
	replace maleavg = maleavg*-1
	egen test = min(maleavg)
	replace test = test-1

	gen order =.
	set obs 7
	replace order =4 in 7
	replace femaleavg=100 in 7
	replace maleavg=-100 in 7
	replace order =3 if status=="1" & var =="diagnosed"
	replace order =2 if status=="1" & var =="onmeds"
	replace order =1 if status=="Yes" & var =="Those controlled out of all hypertensives"




	gen malelabel=-1*maleavg
	format malelabel %10.1f
	format femaleavg %10.1f


	twoway bar maleavg order, horizontal xvarlab(Males) barwidth(.6) || bar  femaleavg order , barwidth(.6) horizontal xvarlab(Females)  xtitle("Percent of Population") ytitle("")   xlabel(-100 "100%" -50 "50%"0 "0%" 50 "50%" 100 "100%", tlength(0) grid gmin gmax) legend(label(1 Males, N=26) label(2 Females, N=46)) legend(order(1 2)) xscale(range(-100 -50 0 50 100)) ylabel(1 "Controlled" 2 "Treated" 3 "Diagnosed" 4 "Hypertensive", angle(0)) title("Hypertension Cascade, Ages 25-44") ||  scatter  order maleavg if order>1 , mlabel(malelabel) msymbol(none) mlabcolor(white) mlabsize(medlarge) mlabp(3) || scatter  order femaleavg , mlabel(femaleavg) msymbol(none) mlabcolor(white) mlabsize(medlarge) mlabp(9) ||  scatter  order maleavg if order==1 , mlabel(malelabel) msymbol(none) mlabcolor(black) mlabsize(medlarge) mlabp(9)




	graph export "cascade_sex25_scaled.png", replace height(6000) width(8000)
	graph export "cascade_sex25_small_scaled.png", replace height(600) width(800)


	replace maleavg = maleavg*-1
	gen maleorder = order +.2
	replace order = order*-1
	replace maleorder = maleorder*-1
	twoway scatter femaleavg order , mcolor(red)|| rcap femalelb femaleub order, lcolor(red) || scatter maleavg maleorder, mcolor(blue) || rcap malelb maleub maleorder, lcolor(blue) xlabel(-1 "Controlled" -2 "Treated" -3 "Diagnosed" -4 "Hypertensive") legend(label(1 Females) label(3 Males)) legend(order(1 3)) ylabel(,angle(0)) ytitle("Percent") title("Hypertensive Population 25-44")
	graph export "uncertainty_sex25.png", replace height(6000) width(8000)

	graph export "uncertainty_sex25_small.png", replace height(600) width(800)


***********************************************************
*** AGES 45-64
***********************************************************
	use `data', clear


** TABLE EIGHT: Prevalence of diagnosed hypertension.
	svy, subpop(age45): tab diagnosed sex, percent ci col
	tabout diagnosed sex using "diag45.csv" if age45==1, svy replace  cells(col lb ub) format(3 3 3) style(csv) h2( status | femaleavg | femalelb | femaleub | maleavg | malelb | maleub |  totalavg | totallb | totalub)

** Prevalence of treated hypertension.
	svy, subpop(age45): tab onmeds sex, percent ci col
	tabout onmeds sex using "treat45.csv" if age45==1, svy replace  cells(col lb ub) format(3 3 3) style(csv) h2( status | femaleavg | femalelb | femaleub | maleavg | malelb | maleub |  totalavg | totallb | totalub)

** Prevalence of controlled hypertension.
	svy, subpop(age45): tab propcontrolled sex, percent ci col
	tabout propcontrolled sex using "control45.csv" if age45==1, svy replace  cells(col lb ub) format(3 3 3) style(csv) h2( status | femaleavg | femalelb | femaleub | maleavg | malelb | maleub |  totalavg | totallb | totalub)


** Bring in all data

	insheet using "diag45.csv", comma clear
	drop in 1
	outsheet using "diag45.csv", comma replace nonames
	insheet using "diag45.csv", clear comma names
	save "`datapath'\diag45.dta", replace

	insheet using "treat45.csv", comma clear
	drop in 1
	outsheet using "treat45.csv", comma replace nonames
	insheet using "treat45.csv", clear comma names
	save "`datapath'\treat45.dta", replace

	insheet using "control45.csv", comma clear
	drop in 1
	outsheet using "control45.csv", comma replace nonames
	insheet using "control45.csv", clear comma names
	save "`datapath'\control45.dta", replace

		use "`datapath'\diag45.dta", clear
	append using "`datapath'\treat45.dta"
	append using "`datapath'\control45.dta"


** continue to format
	gen var=status
	replace var ="" if var=="No" | var=="Yes" | var=="Total" | var=="0" | var=="1"
	carryforward var, replace
	drop if status==var
	destring female*, replace force
	destring male*, replace force
	destring total*, replace force
	drop if status=="Total"


	gen zero = 0
	replace maleavg=maleavg*100
	replace malelb = malelb*100
	replace maleub=maleub*100
	replace femaleavg=femaleavg*100
	replace femalelb = femalelb*100
	replace femaleub=femaleub*100
	replace maleavg = maleavg*-1
	egen test = min(maleavg)
	replace test = test-1

	gen order =.
	set obs 7
	replace order =4 in 7
	replace femaleavg=100 in 7
	replace maleavg=-100 in 7
	replace order =3 if status=="1" & var =="diagnosed"
	replace order =2 if status=="1" & var =="onmeds"
	replace order =1 if status=="Yes" & var =="Those controlled out of all hypertensives"




	gen malelabel=-1*maleavg
	format malelabel %10.1f
	format femaleavg %10.1f



	twoway bar maleavg order, horizontal xvarlab(Males) barwidth(.6) || bar  femaleavg order , barwidth(.6) horizontal xvarlab(Females)  xtitle("Percent of Population") ytitle("")  xlabel(-100 "100%" -50 "50%"0 "0%" 50 "50%" 100 "100%", tlength(0) grid gmin gmax) legend(label(1 Males, N=100) label(2 Females, N=164)) legend(order(1 2)) xscale(range(-100 -50 0 50 100)) ylabel(1 "Controlled" 2 "Treated" 3 "Diagnosed" 4 "Hypertensive", angle(0)) title("Hypertension Cascade, Ages 45-64")  ||  scatter  order maleavg , mlabel(malelabel) msymbol(none) mlabcolor(white) mlabsize(medlarge) mlabp(3) || scatter  order femaleavg , mlabel(femaleavg) msymbol(none) mlabcolor(white) mlabsize(medlarge) mlabp(9)





	graph export "cascade_sex45_scaled.png", replace height(6000) width(8000)
	graph export "cascade_sex45_small_scaled.png", replace height(600) width(800)

	replace maleavg = maleavg*-1
	gen maleorder = order +.2
	replace order = order*-1
	replace maleorder = maleorder*-1
	twoway scatter femaleavg order , mcolor(red)|| rcap femalelb femaleub order, lcolor(red) || scatter maleavg maleorder, mcolor(blue) || rcap malelb maleub maleorder, lcolor(blue) xlabel(-1 "Controlled" -2 "Treated" -3 "Diagnosed" -4 "Hypertensive") legend(label(1 Females) label(3 Males)) legend(order(1 3)) ylabel(,angle(0)) ytitle("Percent") title("Hypertensive Population 45-64")
	graph export "uncertainty_sex45.png", replace height(6000) width(8000)

	graph export "uncertainty_sex45_small.png", replace height(600) width(800)


***********************************************************
*** AGES 65 +
***********************************************************
	use `data', clear


** TABLE EIGHT: Prevalence of diagnosed hypertension.
	svy, subpop(age65): tab diagnosed sex, percent ci col
	tabout diagnosed sex using "diag65.csv" if age65==1, svy replace  cells(col lb ub) format(3 3 3) style(csv) h2( status | femaleavg | femalelb | femaleub | maleavg | malelb | maleub |  totalavg | totallb | totalub)

** Prevalence of treated hypertension.
	svy, subpop(age65): tab onmeds sex, percent ci col
	tabout onmeds sex using "treat65.csv" if age65==1, svy replace  cells(col lb ub) format(3 3 3) style(csv) h2( status | femaleavg | femalelb | femaleub | maleavg | malelb | maleub |  totalavg | totallb | totalub)

** Prevalence of controlled hypertension.
	svy, subpop(age65): tab propcontrolled sex, percent ci col
	tabout propcontrolled sex using "control65.csv" if age65==1, svy replace  cells(col lb ub) format(3 3 3) style(csv) h2( status | femaleavg | femalelb | femaleub | maleavg | malelb | maleub |  totalavg | totallb | totalub)


** Bring in all data

	insheet using "diag65.csv", comma clear
	drop in 1
	outsheet using "diag65.csv", comma replace nonames
	insheet using "diag65.csv", clear comma names
	save "`datapath'\diag65.dta", replace

	insheet using "treat65.csv", comma clear
	drop in 1
	outsheet using "treat65.csv", comma replace nonames
	insheet using "treat65.csv", clear comma names
	save "`datapath'\treat65.dta", replace

	insheet using "control65.csv", comma clear
	drop in 1
	outsheet using "control65.csv", comma replace nonames
	insheet using "control65.csv", clear comma names
	save "`datapath'\control65.dta", replace

		use "`datapath'\diag65.dta", clear
	append using "`datapath'\treat65.dta"
	append using "`datapath'\control65.dta"


** continue to format
	gen var=status
	replace var ="" if var=="No" | var=="Yes" | var=="Total" | var=="0" | var=="1"
	carryforward var, replace
	drop if status==var
	destring female*, replace force
	destring male*, replace force
	destring total*, replace force
	drop if status=="Total"


	gen zero = 0
	replace maleavg=maleavg*100
	replace malelb = malelb*100
	replace maleub=maleub*100
	replace femaleavg=femaleavg*100
	replace femalelb = femalelb*100
	replace femaleub=femaleub*100
	replace maleavg = maleavg*-1
	egen test = min(maleavg)
	replace test = test-1

	gen order =.
	set obs 7
	replace order =4 in 7
	replace femaleavg=100 in 7
	replace maleavg=-100 in 7
	replace order =3 if status=="1" & var =="diagnosed"
	replace order =2 if status=="1" & var =="onmeds"
	replace order =1 if status=="Yes" & var =="Those controlled out of all hypertensives"




	gen malelabel=-1*maleavg
	format malelabel %10.1f
	format femaleavg %10.1f


	twoway bar maleavg order, horizontal xvarlab(Males) barwidth(.6) || bar  femaleavg order , barwidth(.6) horizontal xvarlab(Females)  xtitle("Percent of Population") ytitle("") xlabel(-100 "100%" -50 "50%"0 "0%" 50 "50%" 100 "100%", tlength(0) grid gmin gmax) legend(label(1 Males, N=80) label(2 Females, N=130)) legend(order(1 2)) xscale(range(-100 -50 0 50 100)) ylabel(1 "Controlled" 2 "Treated" 3 "Diagnosed" 4 "Hypertensive", angle(0)) title("Hypertension Cascade, Ages 65+")   ||  scatter  order maleavg  , mlabel(malelabel) msymbol(none) mlabcolor(white) mlabsize(medlarge) mlabp(3) || scatter  order femaleavg , mlabel(femaleavg) msymbol(none) mlabcolor(white) mlabsize(medlarge) mlabp(9)

	graph export "cascade_sex65_scaled.png", replace height(6000) width(8000)
	graph export "cascade_sex65_small_scaled.png", replace height(600) width(800)

	replace maleavg = maleavg*-1
	gen maleorder = order +.2
	replace order = order*-1
	replace maleorder = maleorder*-1
	twoway scatter femaleavg order , mcolor(red)|| rcap femalelb femaleub order, lcolor(red) || scatter maleavg maleorder, mcolor(blue) || rcap malelb maleub maleorder, lcolor(blue) xlabel(-1 "Controlled" -2 "Treated" -3 "Diagnosed" -4 "Hypertensive") legend(label(1 Females) label(3 Males)) legend(order(1 3)) ylabel(,angle(0)) ytitle("Percent") title("Hypertensive Population 65+")
	graph export "uncertainty_sex65.png", replace height(6000) width(8000)
	graph export "uncertainty_sex65_small.png", replace height(600) width(800)
