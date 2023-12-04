
/**************************************************************************************************************************************************/
//  PROJECT:............The co-benefits of the EU ETS
//  PARTICIPANTS:.......Pier Basaglia, Jonas Grunau, and Moritz Drupp   				                
//  DATE:...............November 2023                       			
//  Task of program: .. SDID and TWFE-DiD approaches 

// All analyses have been run with:
// Stata/SE 15.1 for Mac (64-bit Intel)
// Revision 03 Feb 2020
// Copyright 1985-2017 StataCorp LLC

// Approximate time to run SDID and TWFE-DiD analyses: 6m 23s
/**************************************************************************************************************************************************/

/*
This repository contains Stata codes to replicate the Synthetic Difference-in-Differences (SDID) and Two-Way Fixed Effects (TWFE) analyses presented in the paper titled "The European Union Emissions Trading System yields large co-benefits from pollution reduction". 

Overview
The SDID and TWFE analyses are structured in three parts:
1.	Data Cleaning: Ensuring a strongly balanced sample, which is a prerequisite for the SDID estimator (cf. Arkhangelsky et al., 2021).
2.	Estimation: Utilizing the Stata command sdid developed by Pailañir & Clarke (2022) for the SDID analysis.
3.	Exponential Transformation: Applying an exponential transformation to express the estimates in percentage terms, as our analyses are conducted using log-linear models.

These steps are repetead to estimate our analyses starting from (i) the first trading period of the EU ETS in 2005 and (ii) the second trading period of the EU ETS in 2008.

*/


clear all
set matsize 11000
set maxvar 10000
set emptycells drop 
set more off, permanently 
prog drop _all
mac drop _all


// Location of the repository --> adjust according to who's running the file

* Insert Code for Adjusting Directory
global repo "C:/.../EU_ETS_Co_Benefits/"

// Location of the Stata SDID code within the repository
global home ${repo}Stata_SDID/


// Main Stata package needed to run the code:

// We use the sdid command (Pailañir & Clarke, 2022), which implements the synthetic difference-in-differences method of Arkhangelsky et al. (AER, 2021) for STATA.
ssc install sdid, replace

// Additional Stata packages needed to run the code:
*ssc install reghdfe, replace // only install if you want to rely on an alternative command to run the TWFE-DiD estimations
ssc install estout, replace 

*******************************************
********  Macros: paths  ******************
*******************************************

global dofiles 		${home}dofiles/
global dataIn 		${home}data_in/
global dataOut 		${home}data_out/
global dataTemp	    ${home}data_temp/
global tables  		${home}tables/
global figures 		${home}figures/
global paper_results 		${home}results/

// File format for figures:
global fileformat png 


*******************************************
********  Input requirements  *************
*******************************************

*Data inputs to run the codes: 
	* ${home}data_in/so2_gscm_data.csv
	* ${home}data_in/pm25_gscm_data.csv
	* ${home}data_in/nox_gscm_data.csv

*******************************************
***********  Main results  ****************
*******************************************

******* Begin recording .log file *******

cd "${home}logs"

log using EUETS_SDID.log, replace

clear all

*Loop analysis over the pollutants under investigation
local varlist "so2 pm25 nox"

foreach v in `varlist' {

        import delimited "${dataIn}`v'_gscm_data.csv", encoding(UTF-8) clear

        encode unit_id, gen(id)
        tsset id year //Panel is unbalanced --> Need to make it strongly balanced to run SDID estimator (Arkhangelsky et al., 2021)

        preserve

        cd "${paper_results}"
        
        // First step: Cleaning the sample

        *The command sdid requires a balanced sample so (i) we need to exclude a number of Eastern European countries due to missing data in the early 90s and (ii) need to restrict the sample prior to 2019 not to drop the UK out of the sample 

        keep if year<=2019

        drop if country=="Estonia"
        drop if country=="Hungary"
        drop if country=="Latvia"
        drop if country=="Lithuania"
        drop if country=="Slovakia"
        drop if country=="Slovenia"

        capt label variable log_emissions "Emissions (log)"
        capt label variable treat_post "EU ETS"

        ****************************************

        //Second step: Estimations

        *SDiD
        * Follow our main specification (cf. Methods Section in the paper for more details) based on Bayer and Aklin (2020): include log_gdp log_gdp_2 as controls.
        * Implement the optimization procedure introduced by Kranz (2022) with the option "projected" in the covariates environment.
        * Select a random number of seeds to replicate the same bootstrapped sample on the grounds of transparency and enhanced replicability.

        cd "${figures}"
        eststo sdid_1: sdid log_emissions id year treat_post, vce(bootstrap) covariates(log_gdp log_gdp_2, projected) reps(800) seed(1615) 
	
	    * If you add the additional part of the code below to line 135, graphs will be displayed in the style of Figure 1 from Arkhangelsky et al. (2021). Use the option "help sdid" for further details.
	    * Graphs are already saved in the Online Repository in "EU_ETS_Co_Benefits/Stata_SDID/figure". Please make sure to delete the existing gph files in the folder before running the following code to prevent any errors arising from attempts to overwrite the existing files.
	    * graph g1on  g1_opt(xtitle(""))  g2_opt(xtitle("") saving(`v')) graph_export(`v', .png)

        *Results come from a log-linear model, save results to compute exp() transformation in a later step
        cd "${tables}"
        putexcel set EUETS_SDID_`v'_tbc, replace
        putexcel A1=("estimate") B1=("standarderror")  A2=matrix(e(ATT)) B2=matrix(e(se))

        *TWFE-DiD
        *Three options here: (i) run it with the same sdid command or (ii) absorb fixed effects with reghdfe. If using (ii), then we can additionally absorb country-year effects which reflect a third option (iii).
        *They all yield almost identical results, we prefer option (i) so we rely on a consistent package to yield all our DiD estimations
        eststo sdid_2: sdid log_emissions id year treat_post, vce(bootstrap) covariates(log_gdp log_gdp_2, projected) method(did) reps(800) seed(1615)
        *eststo sdid_2: reghdfe log_emissions treat_post log_gdp log_gdp_2, abs(i.id i.year)
        *eststo sdid_2: reghdfe log_emissions treat_post log_gdp log_gdp_2, abs(i.id i.year i.country_id##i.year)

        putexcel set EUETS_TWFEDID_`v'_tbc, replace
        putexcel A1=("estimate") B1=("standarderror")  A2=matrix(e(b)) B2=matrix(e(se))

        *Export estimates
        esttab sdid_1 sdid_2 using EUETS_SDID_`v'.csv, keep(treat_post) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) b(%-9.3f) se(%-9.3f) label replace

        ****************************************

        //Third Step: Results come from a log-linear model, we need to apply exp() transformation to interpret our results as percentages

        *SDiD
        import excel "${tables}EUETS_SDID_`v'_tbc.xlsx", sheet("Sheet1") firstrow clear

        // Compute 95% confidence intervals
        local critical_value = 1.96 // for a 95% confidence interval

        gen CI_lower = estimate - (`critical_value' * standarderror)
        gen CI_upper = estimate + (`critical_value' * standarderror)

        replace estimate = exp(estimate)-1

        replace CI_lower = exp(CI_lower)-1
        replace CI_upper = exp(CI_upper)-1


        cd "${paper_results}"
        export delimited using "EUETS_SDID_`v'_exp", replace

        *TWFE-DiD
        import excel "${tables}EUETS_TWFEDID_`v'_tbc.xlsx", sheet("Sheet1") firstrow clear

        // Compute 95% confidence intervals
        local critical_value = 1.96 // for a 95% confidence interval

        gen CI_lower = estimate - (`critical_value' * standarderror)
        gen CI_upper = estimate + (`critical_value' * standarderror)

        replace estimate = exp(estimate)-1

        replace CI_lower = exp(CI_lower)-1
        replace CI_upper = exp(CI_upper)-1


        cd "${paper_results}"
        export delimited using "EUETS_TWFEDID_`v'_exp", replace

        restore
}

log close

/*

*******************************************
*********  Results from 2008  *************
*******************************************

******* Begin recording .log file *******

cd "${home}logs"

log using EUETS_SDID_2008.log, replace

clear all

*Loop analysis over the pollutants under investigation
local varlist "so2 pm25 nox"

foreach v in `varlist' {

        import delimited "${dataIn}`v'_gscm_data.csv", encoding(UTF-8) clear

        encode unit_id, gen(id)
        tsset id year //Panel is unbalanced --> Need to make it strongly balanced to run SDID estimator (Arkhangelsky et al., 2021)

        *SDID and DID
        preserve

        // First step: Cleaning the sample
        *The command sdid requires a balanced sample so (i) we need to exclude a number of Eastern European countries due to missing data in the early 90s and (ii) need to restrict the sample prior to 2019 not to drop the UK out of the sample 

        keep if year<=2019

        drop if country=="Estonia"
        drop if country=="Hungary"
        drop if country=="Latvia"
        drop if country=="Lithuania"
        drop if country=="Slovakia"
        drop if country=="Slovenia"

        capt label variable log_emissions "Emissions (log)"
        capt label variable treat_post "EU ETS"

        *Shift treatment to 2008 to investigate impacts over the second trading phase of the EU ETS
        gen ets_second_phase=1 if year>=2008
        replace ets_second_phase=0 if missing(ets_second_phase)

        replace treat_post = treat_post*ets_second_phase

        ****************************************

        //Second step: Estimations

        *SDiD
        * Follow our main specification (cf. Methods Section in the paper for more details) based on Bayer and Aklin (2020): include log_gdp log_gdp_2 as controls.
        * Implement the optimization procedure introduced by Kranz (2022) with the option "projected" in the covariates environment.
        * Select a random number of seeds to replicate the same bootstrapped sample on the grounds of transparency and enhanced replicability.

        cd "${figures}"
        eststo sdid_1: sdid log_emissions id year treat_post, vce(bootstrap) covariates(log_gdp log_gdp_2, projected) reps(800) seed(1615)

        *Results come from a log-linear model, save results to compute exp() transformation in a later step
        cd "${tables}"
        putexcel set EUETS_SDID_`v'_tbc_2008, replace
        putexcel A1=("estimate") B1=("standarderror")  A2=matrix(e(ATT)) B2=matrix(e(se))

        *TWFE-DiD
        *Three options here: (i) run it with the same sdid command or (ii) absorb fixed effects with reghdfe. If using (ii), then we can additionally absorb country-year effects which reflect a third option (iii).
        eststo sdid_2: sdid log_emissions id year treat_post, vce(bootstrap) covariates(log_gdp log_gdp_2, projected) method(did) reps(800) seed(1615)
        *eststo sdid_2: reghdfe log_emissions treat_post log_gdp log_gdp_2, abs(i.id i.year)
        *eststo sdid_2: reghdfe log_emissions treat_post log_gdp log_gdp_2, abs(i.id i.year i.country_id##i.year)

        *Results come from a log-linear model, sae results to compute exp() transofrmation in a later step
        putexcel set EUETS_TWFEDID_`v'_tbc_2008, replace
        putexcel A1=("estimate") B1=("standarderror")  A2=matrix(e(b)) B2=matrix(e(se))

        *Export estimates
        esttab sdid_1 sdid_2 using EUETS_SDID_`v'_2008.csv, keep(treat_post) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) b(%-9.3f) se(%-9.3f) label replace

        ****************************************

        //Third Step: Results come from a log-linear model, we need to apply exp() transformation to interpret our results as percentages

        *SDiD
        import excel "${tables}EUETS_SDID_`v'_tbc_2008.xlsx", sheet("Sheet1") firstrow clear

        // Compute 95% confidence intervals
        local critical_value = 1.96 // for a 95% confidence interval

        gen CI_lower = estimate - (`critical_value' * standarderror)
        gen CI_upper = estimate + (`critical_value' * standarderror)

        replace estimate = exp(estimate)-1

        replace CI_lower = exp(CI_lower)-1
        replace CI_upper = exp(CI_upper)-1


        cd "${paper_results}"
        export delimited using "EUETS_SDID_`v'_exp_2008", replace

        *TWFE-DiD
        import excel "${tables}EUETS_TWFEDID_`v'_tbc_2008.xlsx", sheet("Sheet1") firstrow clear

        // Compute 95% confidence intervals
        local critical_value = 1.96 // for a 95% confidence interval

        gen CI_lower = estimate - (`critical_value' * standarderror)
        gen CI_upper = estimate + (`critical_value' * standarderror)

        replace estimate = exp(estimate)-1

        replace CI_lower = exp(CI_lower)-1
        replace CI_upper = exp(CI_upper)-1


        cd "${paper_results}"
        export delimited using "EUETS_TWFEDID_`v'_exp_2008", replace

        restore
}

log close

******* End recording .log file *******


***********************************
*********  References  ************
***********************************
 
* D Arkhangelsky, S Athey, DA Hirshberg, GW Imbens, S Wager, Synthetic difference-in-differences. Am. Econ. Rev. 111, 4088–4118 (2021).
* S Athey, M Bayati, N Doudchenko, G Imbens, K Khosravi, Matrix completion methods for causal panel data models. J. Am. Stat. Assoc. 116, 1716–1730 (2021)
* P Bayer, M Aklin, The european union emissions trading system reduced co2 emissions despite low prices. Proc. Natl. Acad. Sci. 117, 8804–8812 (2020).
* S Kranz, Synthetic difference-in-differences with time-varying covariates, Technical report (2022)
* D Pailañir & D Clarke, SDID: Stata module to perform synthetic difference-in-differences estimation, inference, and visualization. Statistical Software Components S459058, Boston College Department of Economics, revised 03 Mar 2023.
