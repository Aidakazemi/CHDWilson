<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/chdwilson)](https://CRAN.R-project.org/package=chdwilson)
<!-- badges: end --> 
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

## chdwilson

R package for the prediction of chance of developing Coronary Heart Disease (CHD) risk using Total Cholesterol and LDL Cholesterol risk factors over 10 years by Joint National Committee (JNC-V) blood pressure and National Cholesterol Education program (NCEP) categories respectively described in [https://www.ahajournals.org/doi/full/10.1161/01.CIR.97.18.1837](https://www.ahajournals.org/doi/full/10.1161/01.CIR.97.18.1837). 


### Installation

You can download the latest development version from GitHub:

```
install.packages("remotes")
remotes::install_github("resplab/chdwilson")
```


### Coronary Heart Disease Prediction

To get a prediction for Coronary Heart Disease (CHD), you will need to pass in patient's risk factors. For example: 

```
predictchd (age = 55, gender = 1, TChol = 250, LDL = 120, HDL = 39, SBP = 146, DBP = 88, diabetes = 0 , smoker =1)
```

The ***predictCHD()*** function returns the probability of developing Coronary Heart Disease (CHD) risk using Total Cholesterol and LDL Cholesterol risk factors over 10 years. LDL Cholesterol is the major atherogenic lipoprotein and that measurement of LDL Cholesterol levels in the clinical setting provides an advantage base on clinical trial results.

### Cloud-based API Access
The [PRISM platform](http://prism.resp.core.ubc.ca) allows users to access CHDWilson through the cloud. A MACRO-enabled Excel-file can be used to interact with the model and see the results. To download the PRISM Excel template file for CHDWilson please refer to the [PRISM model repository](http://resp.core.ubc.ca/ipress/prism).


### Citation

Please cite: 

```
Wilson, P. W., D’Agostino, R. B., Levy, D., Belanger, A. M., Silbershatz, H., & Kannel, W. B. (1998). Prediction of coronary heart disease using risk factor categories. Circulation, 97(18), 1837-1847.
```
