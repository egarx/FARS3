
# FARS3

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/egarx/FARS3.svg?branch=master)](https://travis-ci.com/egarx/FARS3)
[![R-CMD-check](https://github.com/egarx/FARS3/workflows/R-CMD-check/badge.svg)](https://github.com/egarx/FARS3/actions)
<!-- badges: end -->

The functions programmed using data from the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System (FARS), which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes. 

## Steps to run the functions:

1. Libraries used in this project
2. This function reads data from .csv file
3. Make data file name
4. Read FARS years
5. Summarize FARS data by years.
6. Display accidents map by state and year.

## Examples

1. fars_summarize_years(2015)
2. fars_summarize_years(c(2015, 2014))
3. fars_map_state(49, 2015) # selecting any state, in this case it selected # 49
