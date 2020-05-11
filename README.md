This repository contains the code and data for the paper "[Remote Work and Employment Dynamics under Covid-19: Evidence from Canada](https://guillgall.github.io/files/remotework_dynamics.pdf)" by Mahesh Acharya, Shantanu Debbarman, Guillermo Gallacher and Iqbal Hossain

## Results

If you want to download some of our Remote-Work Index results without running any replication code, you can download the following CSV files:
- [Canada (Aggregate), Provincial, City and Territorial level](Data/Output/telework_estimates.csv).
- [Industry level](Data/Output/remote_work_industry.csv).
- [By percentile of income distribution](Data/Output/remote_income_percentile.csv).
- [By 2-digit employment percentage change](Data/Output/remote_employment_dynamics_2_digit_occp_with_code.csv).

## Code organization

There are two main folders `Code` and `Data`. The code inside `Code` transforms data from `Data/Input` into `Data/Output`.

## Software requirements
The code is written in [R](https://www.r-project.org/) and [Stata](http://www.stata.com).

## Replication instructions

### Download and run code
1. Download (or clone) this repository by clicking the green `Clone or download` button above.
Uncompress the ZIP file into a working directory on your computer. 
2. Request and download the 2019 (January-December) and 2020 (January-April) [Labor Force Survey](https://www.statcan.gc.ca/eng/survey/household/3701) micro-data. Save this data in `Input`->`LFS` subfolder. 
3. Follow instructions (in script comments) for the [R Code](Code/R/0_master_run.R) and for the [Stata Code](Code/Stata/master_run_microdata_lfs.do).

### Notes
- The R code in `Code/R` generates all tables, graphs and regressions in the paper (the uploaded data in `Data/Output` is enough to replicate all results)
- The LFS micro-data is however required to run the Stata code in `Code/Stata`.

## Related work
- Replication package by [Jonathan I. Dingel and Brent Neiman](https://github.com/jdingel/DingelNeiman-workathome/) for their paper "How Many Jobs Can be Done at Home?"
