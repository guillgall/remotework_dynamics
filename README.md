This repository contains the code and data the paper "[Remote Work and Employment Dynamics under COVID-19: Evidence from Canada](https://utpjournals.press/doi/10.3138/cpp.2020-026)" by Guillermo Gallacher and Iqbal Hossain.

Update (June 2021): we have corrected some "[coding errors](https://www.utpjournals.press/doi/abs/10.3138/cpp.2020-026-corrigendum)"

## Results

If you want to download some of our Remote-Work Index results without running any replication code, you can download the following CSV files:
- [Canada (Aggregate), Provincial, City and Territorial level](Data/Output/remote_work_geographies_estimates.csv).
- [Industry level](Data/Output/NAICS_21_remote_work.csv).
- [By decile of income distribution](Data/Output/income_decile_remote_work.csv).
- [By 2-digit occupation and employment percentage change](Data/Output/NOC_40_essential_employment_variation.csv).

## Code organization

There are two main folders `Code` and `Data`. The code inside `Code` transforms data from `Data/Input` into `Data/Output`.

## Software requirements
The code is written in [R](https://www.r-project.org/).

## Replication instructions

### Download and run code
1. Download (or clone) this repository by clicking the green `Clone or download` button above.
Uncompress the ZIP file into a working directory on your computer. 
2. Request and download the 2019 (January-December) and 2020 (January-April) [Labor Force Survey](https://www.statcan.gc.ca/eng/survey/household/3701) micro-data. Save this data in your desktop `Desktop/Data/LFS/2017-2021` subfolder. 
3. Follow instructions in the [master code](Code/00_master_run.R).

## Related work
- Replication package by [Jonathan I. Dingel and Brent Neiman](https://github.com/jdingel/DingelNeiman-workathome/) for their paper "How Many Jobs Can be Done at Home?"
