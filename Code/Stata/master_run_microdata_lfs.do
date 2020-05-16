
##Instruction: request and download the LFS 2019 and 2020 data from Statistics Canada (Stata version), and save in "Input" Folder.
Then run the following code

#Occupational Employment Statistics

//Crosswalks_ONET_NOC
clear
import excel "../Output/eis_benchmark_remote.csv"
gen wforce_baseline= employment*baseline 
gen telew_baseline= wforce_baseline/employment
collapse (mean) telew_baseline, by(noc16_2digit)
gen NOC_40 = _n
save "../Input/LFS/eis_benchmark_remote_NOC_40.dta"
//Crosswalks_manual_alternation_NOC
clear
import excel "../Output/eis_manual_remote.csv"
gen wforce_manual= employment*teleworkable
gen telew_manual= wforce_manual/employment
rename NOC6_2digit noc16_2digit
collapse(mean) telew_manual, by(noc16_2digit)
gen NOC_40 = _n
save "../Input/LFS/eis_manual_remote_NOC_40.dta"


##Remote_work_LFS_2019
//Append_monthly_data_to_yearly
clear
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

//Remote_work_province_CMAs_industry
clear
use "../Input/LFS/Canadian LFS 2019_yearly.dta"
gen employees=1 if FTPTMAIN>=1
gen ft_employees=1 if FTPTMAIN==1
gen male=1 if SEX==1 & FTPTMAIN>=1
gen female=1 if SEX==2 & FTPTMAIN>=1
collapse (sum) employees ft_employees male female[iweight=FINALWT], by(PROV CMA NOC_40 NAICS_21)
merge m:1 NOC_40 using "../Input/LFS/eis_benchmark_remote_NOC_40.dta"
drop noc6_2digit noc16_2digit _merge
merge m:1 NOC_40 using "../Input/LFS/eis_manual_remote_NOC_40.dta"
drop _merge
gen telwf_benchmark=ft_employees*telew_baseline
gen telejob_benchmark=telwf_benchmark/ft_employees

gen telwf_manuala=ft_employees*telew_manual
gen teljob_manual=telwf_manuala/ft_employees
replace telejob_benchmark=0 if telejob_benchmark==.
replace teljob_manual=0 if teljob_manual==.
save "../Input/LFS/remote_work.dta"

//remote_work_province
clear
use "../Input/LFS/remote_work.dta"
collapse(mean)telejob_benchmark teljob_manual, by(PROV)
rename telejob_benchmark ShareofJobsBenchmark
rename teljob_manua ShareofJobsAlternative
rename PROV Provinces
export delimited Provinces ShareofJobsBenchmark ShareofJobsAlternative using "../Output/remote_work_province.csv", replace

//remote_work_big_cities
clear
use "../Input/LFS/remote_work.dta"
collapse(mean)telejob_benchmark teljob_manual, by(CMA)
rename telejob_benchmark ShareofJobsBenchmark
rename teljob_manua ShareofJobsAlternative
rename CMA NineCMAs
export delimited NineCMAs ShareofJobsBenchmark ShareofJobsAlternative using "../Output/remote_work_big_cities.csv", replace

//remote_work_industry
clear
use "../Input/LFS/remote_work.dta"
collapse(mean)telejob_benchmark teljob_manual, by(NAICS_21)
rename telejob_benchmark ShareofJobsBenchmark
rename teljob_manua ShareofJobsAlternative
rename NAICS_21 Sector
export delimited Sector ShareofJobsBenchmark ShareofJobsAlternative using "../Output/remote_work_industry.csv", replace

//remote_employment_female_industry
clear
use "../Input/LFS/remote_work.dta"
gen female_share= female/employee
collapse(mean)telejob_benchmark teljob_manual female_share, by(NAICS_21)
rename telejob_benchmark ShareofJobsBenchmark
rename teljob_manua ShareofJobsAlternative
rename NAICS_21 Sector
rename female_share shareoffemale
drop if Sector==.
export delimited Sector ShareofJobsBenchmark ShareofJobsAlternative shareoffemale using "../Output/remote_employment_female_industry.csv", replace

//essential_worker_NOC_40
clear
import excel "../Output/LMIC/lmic_essential_workers.xlsx", sheet("Sheet1") firstrow
rename NOCcode noccode
sort noccode
save "../Input/LFS/lmic_essential_workers.dta", replace

clear
import delimited "../Output/eis_benchmark_remote.csv", varnames(1) encoding(UTF-8)
save "../Input/LFS/eis_benchmark_remote.dta", replace
merge m:m noccode using "../Input/LFS/lmic_essential_workers.dta"
drop _merge
gen ess_worker=0
replace ess_worker=1 if LMICspreliminaryshortlist=="Yes"
collapse(mean) ess_worker, by( noc16_2digit)
drop if noc16_2digit==""
gen NOC_40 = _n
save "../Input/LFS/essential_worker_NOC_40.dta", replace

//remote_employment_female_2_digit_occp_with_code
clear
use "../Input/LFS/remote_work.dta"
gen female_share= female/employee
collapse(mean)telejob_benchmark teljob_manual female_share, by(NOC_40)
gen NOC_40 = _n
drop if NOC_40==.
merge 1:1 NOC_40 using "../Input/LFS/essential_worker_NOC_40.dta"
drop _merge
rename telejob_benchmark ShareofJobsBenchmark
rename teljob_manua ShareofJobsAlternative
rename NOC_40 NOC
rename noc16_2digit digit2_occupation
rename female_share shareoffemale
order NOC ess_worker digit2_occupation ShareofJobsBenchmark ShareofJobsAlternative shareoffemale
export delimited NOC ess_worker digit2_occupation ShareofJobsBenchmark ShareofJobsAlternative shareoffemale using "../Output/remote_employment_female_2_digit_occp_with_code.csv", replace

##remote_worker_characteristics
clear
use "../Input/LFS/Canadian LFS 2019_yearly.dta"
merge m:1 NOC_40 using "../Input/LFS/Documents/eis_benchmark_remote_NOC_40.dta"
drop noc6_2digit noc16_2digit _merge
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
eststo:reg male telew_baseline i.NOC_40
eststo:reg parttime telew_baseline i.NOC_40
eststo:reg below_medianwage telew_baseline i.NOC_40
eststo:reg no_college telew_baseline i.NOC_40
eststo:reg nonimmigrant telew_baseline i.NOC_40
eststo:reg less50 telew_baseline i.NOC_40
eststo:reg single telew_baseline i.NOC_40
eststo:reg small_farm telew_baseline i.NOC_40
eststo:reg private telew_baseline i.NOC_40
eststo:reg job_nature telew_baseline i.NOC_40
asdoc esttab
eststo clear

//Alternative method
eststo:reg male telew_manual i.NOC_40
eststo:reg parttime telew_manual i.NOC_40
eststo:reg below_medianwage telew_manual i.NOC_40
eststo:reg no_college telew_manual i.NOC_40
eststo:reg nonimmigrant telew_manual i.NOC_40
eststo:reg less50 telew_manual i.NOC_40
eststo:reg single telew_manual i.NOC_40
eststo:reg small_farm telew_manual i.NOC_40
eststo:reg private telew_manual i.NOC_40
eststo:reg job_nature telew_manual i.NOC_40
esttab
eststo clear

##remote_income_percentile
clear
use "../Input/LFS/Documents/Canadian LFS 2019_yearly.dta"
drop if NOC_40==.
gen employee=1 if FTPTMAIN>=1
gen male=1 if SEX==1 & FTPTMAIN>=1
gen female=1 if SEX==2 & FTPTMAIN>=1
gen parttime=1 if FTPTMAIN==2
gen fulltime=1 if FTPTMAIN==1
gen w_wage=HRLYEARN*UHRSMAIN
xtile w_wage_100= w_wage, nq(100)
tabstat w_wage, by(w_wage_100)
merge m:1 NOC_40 using "../Input/LFS/eis_benchmark_remote_NOC_40.dta"
drop noc6_2digit noc16_2digit _merge
merge m:1 NOC_40 using "../Input/LFS/eis_manual_remote_NOC_40.dta"
drop _merge
collapse (mean) telew_manual telew_baseline, by(w_wage_100)
rename telew_baseline ShareofJobsBenchmark
rename telew_manual ShareofJobsAlternative
rename w_wage_100 income_percentile
drop if income_percentile==.
export delimited income_percentile ShareofJobsBenchmark ShareofJobsAlternative using "../Output/remote_income_percentile.csv", replace

##Employment_Dynamics_LFS2020_January_April
clear
use "../Input/LFS/lfs-71M0001-E-2020-january_F1.dta"
append using "../Input/LFS/lfs-71M0001-E-2020-february_F1.dta"
append using "../Input/LFS/lfs-71M0001-E-2020-march_F1.dta"
append using "../Input/LFS/lfs-71M0001-E-2020-April_F1.dta"
merge m:1 NOC_40 using "../Input/LFS/Documents/eis_benchmark_remote_NOC_40.dta"
drop noc6_2digit noc16_2digit _merge
merge m:1 NOC_40 using "../Input/LFS/eis_manual_remote_NOC_40.dta"
drop _merge
save "../Input/LFS/Canadian LFS 2020_jan-apr.dta"

drop if NOC_40==.
gen employees=1 if FTPTMAIN>=1
gen male=1 if SEX==1 & FTPTMAIN>=1
gen female=1 if SEX==2 & FTPTMAIN>=1
gen parttime=1 if FTPTMAIN==2
gen fulltime=1 if FTPTMAIN==1
gen no_college=1 if EDUC<3
gen immigrant=1 if IMMIG<3
gen less50=1 if AGE_12<=7
gen single=1 if MARSTAT==6
gen small_farm=1 if FIRMSIZE<=2
gen private=1 if COWMAIN==2
gen job_nature=1 if PERMTEMP>1
replace HRLYEARN=HRLYEARN/100 if SURVMNTH==4 //april_data_excel_files_has_4_digit_hours
gen w_wage=HRLYEARN*UHRSMAIN 
tabstat w_wage if SURVMNTH, by(SEX)stat(mean median max min)
gen below_medianwage=1 if w_wage<1120
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
gen college=1 if no_college==0
gen medianwage=1 if below_medianwage==0
gen nonimmigrant=1 if immigrant==0
gen notsingle=1 if single==0
gen more50=1 if less50==0
gen public=1 if COWMAIN==1
gen ml=1 if small_farm==0
recode college(.=0)
recode medianwage(.=0)
recode nonimmigrant(.=0)
recode notsingle(.=0)
recode more50(.=0)
recode public(.=0)
recode ml(.=0)
tabstat male female parttime fulltime below_medianwage medianwage no_college college immigrant nonimmigrant less50 more50 single notsingle small_farm ml private public, by(SURVMNTH) stat(sum)
//results copied in excel and then calculated growth=100*[(emp_feb-emp_jan)/emp_feb]

##Employment_losses_by_occupation_industry_province_CMAs
clear
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
merge m:1 NOC_40 using "../Input/LFS/eis_benchmark_remote_NOC_40.dta"
drop noc6_2digit noc16_2digit _merge
merge m:1 NOC_40 using "../Input/LFS/eis_manual_remote_NOC_40.dta"
drop _merge
save "../Input/LFS/remote_employment_dynamics.dta"

//remote_employment_dynamics_10_occp
clear
use "../Input/LFS/remote_employment_dynamics.dta"
collapse(mean) telew_baseline telew_manual empg_janfeb empg_febmar empg_marapr empg_febapr, by(NOC_10)
rename telew_baseline JobsBenchmark
rename telew_manual JobsAlternative
export delimited NOC_10 JobsBenchmark JobsAlternative empg_janfeb empg_febmar empg_marapr empg_febapr using "../Output/remote_employment_dynamics_10_occp.csv", replace

//remote_employment_dynamics_2_digit_occp_with_code
clear
use "../Input/LFS/remote_employment_dynamics.dta"
collapse(mean) telew_baseline telew_manual empg_janfeb empg_febmar empg_marapr empg_febapr, by(NOC_40)
gen NOC_40 = _n
merge 1:1 NOC_40 using "../Input/LFS/lmic_essential_workers.dta"
drop _merge
rename NOC_40 digit2_occupation
rename telew_baseline JobsBenchmark
rename telew_manual JobsAlternative
export delimited digit2_occupation ess_worker JobsBenchmark JobsAlternative empg_janfeb empg_febmar empg_marapr empg_febapr using "../Output/remote_employment_dynamics_2_digit_occp_with_code.csv", replace

//remote_employment_dynamics_industry_level
clear
use "../Input/LFS/remote_employment_dynamics.dta"
collapse(mean) telew_baseline telew_manual empg_janfeb empg_febmar empg_marapr empg_febapr, by(NAICS_21)
rename NAICS_21 Industrylevel
rename telew_baseline JobsBenchmark
rename telew_manual JobsAlternative
export delimited Industrylevel JobsBenchmark JobsAlternative empg_janfeb empg_febmar empg_marapr empg_febapr using "../Output/remote_employment_dynamics_industry_level.csv", replace

//remote_employment_dynamics_province
clear 
use "../Input/LFS/remote_employment_dynamics.dta"
collapse(mean) telew_baseline telew_manual empg_janfeb empg_febmar empg_marapr empg_febapr, by(PROV)
rename PROV Province
rename telew_baseline JobsBenchmark
rename telew_manual JobsAlternative
export delimited Province JobsBenchmark JobsAlternative empg_janfeb empg_febmar empg_marapr empg_febapr using "../Output/remote_employment_dynamics_province.csv", replace

//remote_employment_dynamics_CMAs
clear 
use "../Input/LFS/remote_employment_dynamics.dta"
collapse(mean) telew_baseline telew_manual empg_janfeb empg_febmar empg_marapr empg_febapr, by(CMA)
rename CMA Nine_CMAs
rename telew_baseline JobsBenchmark
rename telew_manual JobsAlternative
export delimited Nine_CMAs JobsBenchmark JobsAlternative empg_janfeb empg_febmar empg_marapr empg_febapr using "../Output/remote_employment_dynamics_CMAs.csv", replace






