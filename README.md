This repository contains the code and data for the paper "[Remote Work and Employment Dynamics under Covid-19: Evidence from Canada](https://www.TBC/)" by Mahesh Acharya, Shantanu Debbarman, Guillermo Gallacher and Iqbal Hossain

## Code organization

There are two main folders `Code` and `Data`. The code inside `Code` transforms data from `Data/Input` into `Data/Output`.

## Software requirements
The code is written in [R](https://www.r-project.org/) and [Stata](http://www.stata.com)

## Replication instructions

### Download and run code
1. Download (or clone) this repository by clicking the green `Clone or download` button above.
Uncompress the ZIP file into a working directory on your computer. 
2. Request and download the 2019 (January-December) and 2020 (January-April) [Labor Force Survey](https://www.statcan.gc.ca/eng/survey/household/3701) micro-data. Save this data in `Input`->`LFS` subfolder. 
3. Follow instructions (in script comments) for the [R Code](Code/R/0_master_run.R) and for the [Stata Code](Code/Stata/master_run_microdata_lfs.do).

## Related work
- Replication package by [Jonathan I. Dingel and Brent Neiman](https://github.com/jdingel/DingelNeiman-workathome/) for their paper "How Many Jobs Can be Done at Home?"
