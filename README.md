[![Build Status](https://travis-ci.org/resplab/CHDWilson.svg?branch=master)](https://travis-ci.org/resplab/CHDWilson)
[![CRAN Status](https://www.r-pkg.org/badges/version/CHDWilson)](https://cran.r-project.org/web/packages/CHDWilson/index.html)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# CHDWilson

R package for the prediction of chance of developing Coronary Heart Disease (CHD) risk using Total Cholesterol and LDL Cholesterol risk factors over 10 years by Joint National Committee (JNC-V) blood pressure and National Cholesterol Education program (NCEP) categories respectively described in [https://www.ahajournals.org/doi/full/10.1161/01.CIR.97.18.1837](https://www.ahajournals.org/doi/full/10.1161/01.CIR.97.18.1837). 
LDL Cholesterol is the major atherogenic lipoprotein and that measurement of LDL Cholesterol levels in the clinical setting provides an advantage base on clinical trial results.

## Installation

The latest stable version can be downloaded from CRAN:  
`install.packages ('CHDWilson')`

Alternatively, you can download the latest development version from GitHub:

```
install.packages("devtools")
devtools::install_github("resplab/CHDWilson")
```

# Web App for CHDWilson 

CHDWilson is also available as web app, accessible at [http://resp.core.ubc.ca/ipress/CHDWilson](http://resp.core.ubc.ca/ipress/CHDWilson)

# Coronary Heart Disease Prediction Model

To get a prediction for Coronary Heart Disease (CHD), you will need to pass in patient's risk factors. For example: 

```
predictCHD (age = 55, gender = 1, TChol = 250, LDL = 120, HDL = 39, SBP = 146, DBP = 88, diabetes = 0 , smoker =1)
```

The ***predictCFMortality()*** function returns the probability of developing Coronary Heart Disease (CHD) risk using Total cholesterol and LDL Cholesterol risk factors over 10 years.

## Cloud-based API Access
The [PRISM platform](http://prism.resp.core.ubc.ca) allows users to access CFMortality through the cloud. A MACRO-enabled Excel-file can be used to interact with the model and see the results. To download the PRISM Excel template file for CFMortality please refer to the [PRISM model repository](http://resp.core.ubc.ca/ipress/prism).


## Citation

Please cite: 

```
Wilson, P. W., D’Agostino, R. B., Levy, D., Belanger, A. M., Silbershatz, H., & Kannel, W. B. (1998). Prediction of coronary heart disease using risk factor categories. Circulation, 97(18), 1837-1847.
```
