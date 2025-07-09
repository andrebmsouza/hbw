
# [How to Bet on Winners and Losers Replication Files](https://ssrn.com/abstract=????)

This repository contains the replication files for the paper <i>How to Bet on Winners and Losers</i>
by Christian Brownlees and Andre B.M. Souza which is available on SSRN at the address
[https://ssrn.com/abstract=3461214](https://ssrn.com/abstract=????)

## Authors 
 [Christian Brownlees](http://www.econ.upf.edu/~cbrownlees/) and [Andre B.M. Souza](http://www.andrebmsouza.com)

## Software Requirements

[R](https://cran.r-project.org/) The code has been tested with R version 4.1.2.

## Instructions

To replicate the results on the paperun the script <tt>gar_replication.m</tt>.
The script will create Tables 4 to 6 of the paper. The tables will be stored as individual CSV files in the directory <tt>tables</tt>.

## Data

***Important Disclaimer:*** The data used in this study was downloaded from the following sources in June 2019.

 - [CRSP]([https://stats.oecd.org/sdmx-json/data/DP_LIVE/.QGDP.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en](https://wrds-www.wharton.upenn.edu/pages/about/data-vendors/center-for-research-in-security-prices-crsp/)) from the CRSP Database
 - [Characteristics Data]([https://www.imf.org/~/media/Files/Publications/GFSR/2017/October/chapter-3/csv-data/data-appendix.ashx?la=eni](https://dachxiu.chicagobooth.edu/download/datashare.zip)) from Dacheng Xiu's webpage.

## Additional Resources
### [Vulnerable Growth Replication Files (Adrian et al, 2019)](https://www.aeaweb.org/articles?id=10.1257/aer.20161923)
 - rq.m: Function to compute quantile regression. Source: Vulnerable Growth Replication Files (Adrian et al, 2019)
 - QuantilesInterpolation.m: Function to interpolate quantiles and get a Skewed T.
 - 
### Packages:
 - [SDPT3-4.0](https://github.com/sqlp/sdpt3)
 
