
##Instruction: request and download the LFS 2019 and 2020 data from Statistics Canada (Stata version), and save in "Input" Folder.
Then run the following code

#Occupational Employment Statistics

//Crosswalks: ONET and NOC
import excel "../Output/eis_benchmark_remote.csv"
gen wforce_baseline= employment*baseline 
gen telew_baseline= wforce_baseline/employment
collapse (mean) telew_baseline, by(noc16_2digit)
gen NOC_40 = _n
save "../Input/LFS/eis_benchmark_remote_NOC_40.dta"
//Crosswalks: manual alternation and NOC
import excel "../Output/eis_manual_remote.csv"
gen wforce_manual= employment*teleworkable
gen telew_manual= wforce_manual/employment
collapse(mean) telew_manual, by(noc16_2digit)
gen NOC_40 = _n
save "../Input/LFS/eis_manual_remote_NOC_40.dta"


##Remote work: LFS 2019

//Append monthly data and make it yearly
use "../Input/LFS/lfs-71M0001-E-2019-january_F1.dta"
append using "../Input/LFS/lfs-71M0001-E-2019-february_F1.dta"
append using "../Input/LFS/lfs-71M0001-E-2019-march_F1.dta"
append using "../Input/LFS/LFS-71M0001-E-2019-April_F1.dta"
append using "../Input/LFS/LFS-71M0001-E-2019-May_F1.dta"
append using "../Input/LFS/LFS-71M0001-E-2019-June_F1.dta"
append using "../Input/LFS/LFS-71M0001-E-2019-July_F1.dta"
append using "../Input/LFS/LFS-71M0001-E-2019-August_F1.dta"
append using "../Input/LFS/LFS-71M0001-E-2019-September_F1.dta"
append using "../Input/LFS/LFS-71M0001-E-2019-October_F1.dta"
append using "../Input/LFS/LFS-71M0001-E-2019-November_F1.dta"
append using "../Input/LFS/LFS-71M0001-E-2019-December_F1.dta"
save "../Input/LFS/Canadian LFS 2019_yearly.dta"

//Remote work: province, CMAs, industry
use "../Input/LFS/Canadian LFS 2019_yearly.dta"
gen employees=1 if FTPTMAIN>=1
gen ft_employees=1 if FTPTMAIN==1
gen male=1 if SEX==1 & FTPTMAIN>=1
gen female=1 if SEX==2 & FTPTMAIN>=1
collapse (sum) employees ft_employees male female[iweight= FINALWT], by(PROV CMA NOC_40 NAICS_21)
merge m:1 NOC_40 using "../Input/LFS/eis_benchmark_remote_NOC_40.dta"
drop NOC16_2digit NOC16_2digit _merge
merge m:1 NOC_40 using "../Input/LFS/eis_manual_remote_NOC_40.dta"
drop _merge
gen telwf_benchmark=ft_employees*telew_baseline
gen telejob_benchmark=telwf_benchmark/ft_employees

gen telwf_manuala=ft_employees*telew_manual
gen teljob_manual=telwf_manuala/ft_employees
replace telejob_benchmark=0 if telejob_benchmark==.
replace teljob_manual=0 if teljob_manual==.

tabstat telejob_benchmark teljob_manual, by(PROV)
tabstat telejob_benchmark teljob_manual, by(CMA)
tabstat telejob_benchmark teljob_manual, by(NAICS_21)

tabstat telejob_benchmark teljob_manual if PROV==10, by(NAICS_21)
tabstat telejob_benchmark teljob_manual if PROV==11, by(NAICS_21)
tabstat telejob_benchmark teljob_manual if PROV==12, by(NAICS_21)
tabstat telejob_benchmark teljob_manual if PROV==13, by(NAICS_21)
tabstat telejob_benchmark teljob_manual if PROV==24, by(NAICS_21)
tabstat telejob_benchmark teljob_manual if PROV==35, by(NAICS_21)
tabstat telejob_benchmark teljob_manual if PROV==46, by(NAICS_21)
tabstat telejob_benchmark teljob_manual if PROV==47, by(NAICS_21)
tabstat telejob_benchmark teljob_manual if PROV==48, by(NAICS_21)
tabstat telejob_benchmark teljob_manual if PROV==59, by(NAICS_21)

//female share by industry and occupations
gen female_share= female/employee
tabstat female_share, by(NAICS_21)
tabstat female_share, by(NOC_40)
save "../Input/LFS/Canadian LFS 2019_remote_work.dta", replace

##LFS:2019-Workers characteristics: who are more vulnerable?

use "../Input/LFS/Canadian LFS 2019_yearly.dta"
merge m:1 NOC_40 using "../Input/LFS/Documents/eis_benchmark_remote_NOC_40.dta"
drop NOC16_2digit NOC16_2digit _merge
merge m:1 NOC_40 using" ../Input/LFS/Documents/eis_manual_remote_NOC_40.dta"
drop _merge

drop if NOC_40==.
gen employee=1 if FTPTMAIN>=1
gen male=1 if SEX==1 & FTPTMAIN>=1
gen female=1 if SEX==2 & FTPTMAIN>=1
gen parttime=1 if FTPTMAIN==2
gen fulltime=1 if FTPTMAIN==1

gen w_wage=HRLYEARN*UHRSMAIN
tabstat w_wage, by(SEX)stat(mean median max min)

gen below_medianwage=1 if w_wage<882
gen no_college=1 if EDUC<3
gen immigrant=1 if IMMIG<3
gen less50=1 if AGE_12<=7
gen single=1 if MARSTAT==6
gen small_farm=1 if FIRMSIZE<=2
gen private=1 if COWMAIN==2
gen job_nature=1 if PERMTEMP>1
gen nonimmigrant=1 if IMMIG==3

recode male (.=0)
recode female (.=0)
recode fulltime (.=0) 
recode parttime(.=0)
recode below_medianwage(.=0)
recode no_college(.=0)
recode immigrant(.=0)
recode less50(.=0)
recode single(.=0)
recode small_farm(.=0)
recode private(.=0)
recode job_nature(.=0)
recode nonimmigrant(.=0)

//Benchmark
reg male telew_baseline
reg parttime telew_baseline
reg below_medianwage telew_baseline
reg no_college telew_baseline
reg nonimmigrant telew_baseline
reg less50 telew_baseline
reg single telew_baseline
reg small_farm telew_baseline
reg private telew_baseline
reg job_nature telew_baseline

//Alternative method
reg male telew_manual
reg parttime telew_manual
reg below_medianwage telew_manual
reg no_college telew_manual
reg nonimmigrant telew_manual
reg less50 telew_manual
reg single telew_manual
reg small_farm telew_manual
reg private telew_manual
reg job_nature telew_manual

##Income percentile and Remotness score
use "../Input/LFS/Documents/Canadian LFS 2019_yearly.dta"
drop if NOC_40==.
gen employee=1 if FTPTMAIN>=1
gen male=1 if SEX==1 & FTPTMAIN>=1
gen female=1 if SEX==2 & FTPTMAIN>=1
gen parttime=1 if FTPTMAIN==2
gen fulltime=1 if FTPTMAIN==1
gen w_wage=HRLYEARN*UHRSMAIN
tile w_wage_100= w_wage, nq(100)
tabstat w_wage, by(w_wage_100)
merge m:1 NOC_40 using "../Input/LFS/eis_benchmark_remote_NOC_40.dta"
drop NOC16_2digit NOC16_2digit _merge
merge m:1 NOC_40 using "../Input/LFS/eis_manual_remote_NOC_40.dta"
drop _merge
collapse (mean) telew_manual telew_baseline, by(w_wage_100)
twoway (scatter telew_baseline w_wage_100) (lfit telew_baseline w_wage_100)

##Employment Dynamics: LFS2020 (January-April)
use "../Input/LFS/lfs-71M0001-E-2020-january_F1.dta"
append using "../Input/LFS/lfs-71M0001-E-2020-february_F1.dta"
append using "../Input/LFS/lfs-71M0001-E-2020-march_F1.dta"
append using "../Input/LFS/lfs-71M0001-E-2020-April_F1.dta"
merge m:1 NOC_40 using "../Input/LFS/Documents/eis_benchmark_remote_NOC_40.dta"
drop NOC16_2digit NOC16_2digit _merge
merge m:1 NOC_40 using "../Input/LFS/eis_manual_remote_NOC_40.dta"
drop _merge
save "../Input/LFS/Canadian LFS 2020_jan-apr.dta"

drop if NOC_40==.
gen employees=1 if FTPTMAIN>=1
gen male=1 if SEX==1 & FTPTMAIN>=1
gen female=1 if SEX==2 & FTPTMAIN>=1
gen parttime=1 if FTPTMAIN==2
gen fulltime=1 if FTPTMAIN==1gen no_college=1 if EDUC<3
gen immigrant=1 if IMMIG<3
gen less50=1 if AGE_12<=7
gen single=1 if MARSTAT==6
gen small_farm=1 if FIRMSIZE<=2
gen private=1 if COWMAIN==2
gen job_nature=1 if PERMTEMP>1
replace HRLYEARN=HRLYEARN/100 if SURVMNTH==4 //april data excel files has 4 digit hours
gen w_wage=HRLYEARN*UHRSMAIN 
tabstat w_wage if SURVMNTH, by(SEX)stat(mean median max min)
gen below_medianwage=1 if w_wage<1120
gen college=1 if no_college==0
gen medianwage=1 if below_medianwage==0
gen nonimmigrant=1 if immigrant==0
gen notsingle=1 if single==0
gen more50=1 if less50==0
gen public=1 if COWMAIN==1
gen ml=1 if small_farm==0
recode male (.=0)
recode female (.=0)
recode fulltime (.=0) 
recode parttime(.=0)
recode below_medianwage(.=0)
recode no_college(.=0)
recode immigrant(.=0)
recode less50(.=0)
recode single(.=0)
recode small_farm(.=0)
recode private(.=0)
recode job_nature(.=0)
recode college(.=0)
recode medianwage(.=0)
recode nonimmigrant(.=0)
recode notsingle(.=0)
recode more50(.=0)
recode public(.=0)
recode ml(.=0)
tabstat male female parttime fulltime below_medianwage medianwage no_college college immigrant nonimmigrant less50 more50 single notsingle small_farm ml private public, by(SURVMNTH) stat(sum)
save "../Input/LFS//Canadian LFS 2020_jan-apr_remote_work.dta", replace

##Employment losses by occupation, industry, province, CMAs
use "../Input/LFS/Canadian LFS 2020_jan-apr.dta"
gen employees=1 if FTPTMAIN>=1
drop if NOC_40==.
gen emp_jan= employee if SURVMNTH==1
gen emp_feb= employee if SURVMNTH==2
gen emp_mar= employee if SURVMNTH==3
gen emp_apr= employee if SURVMNTH==4
collapse(sum) emp_jan emp_feb emp_mar emp_apr, by(NOC_10 NOC_40 NAICS_21 PROV CMA)
gen empg_janfeb= 100*[(emp_feb-emp_jan)/emp_feb]
gen empg_febmar= 100*[(emp_mar-emp_feb)/emp_mar]
gen empg_marapr= 100*[(emp_apr-emp_mar)/emp_apr]
gen empg_febapr= 100*[(emp_apr-emp_feb)/emp_apr]
tabstat telew_baseline telew_manual empg_janfeb empg_febmar empg_marapr empg_febapr, by(NOC_10)
tabstat telew_baseline telew_manual empg_janfeb empg_febmar empg_marapr empg_febapr, by(NOC_40)
tabstat telew_baseline telew_manual empg_janfeb empg_febmar empg_marapr empg_febapr, by(NAICS_21)
tabstat empg_janfeb empg_febmar empg_marapr empg_febapr, by(PROV)
tabstat empg_janfeb empg_febmar empg_marapr empg_febapr, by(CMA)






