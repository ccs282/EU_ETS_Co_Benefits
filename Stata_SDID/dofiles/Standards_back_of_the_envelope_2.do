
/**************************************************************************************************************************************************/
//  PROJECT:............The co-benefits of the EU ETS
//  PARTICIPANTS:.......Pier Basaglia, Jonas Grunau, and Moritz Drupp   				                
//  DATE:...............March 2024                       			
//  Task of program: .. Back-of-the-envelope calculations with EEA data
//                     
// All analyses have been run with:
// Stata/SE 15.1 for Mac (64-bit Intel)
// Revision 03 Feb 2020
// Copyright 1985-2017 StataCorp LLC

/**************************************************************************************************************************************************/

/*
This repository contains Stata codes to replicate back-of-the-envelope calculations with EEA data

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

// File format for figures:
global fileformat png 


*******************************************
********  Input requirements  *************
*******************************************


*******************************************
********  Input requirements  *************
*******************************************

*Data inputs to run the codes: 
	* ${home}STATA/data_in/F1_4_Detailed releases at facility level with E-PRTR Sector and Annex I Activity detail into Air.csv

import delimited "${dataIn}F1_4_Detailed releases at facility level with E-PRTR Sector and Annex I Activity detail into Air.csv", varnames(1) encoding(utf8) clear 

*Extract emissions from combustion plants which are jointly subject to the EU ETS and standards
keep if eprtranneximainactivitylabel=="Thermal power stations and other combustion installations"

*Focus on the pollutants covered in our analyses
keep if pollutant=="Nitrogen oxides (NOX)" | pollutant=="Sulphur oxides (SOX)" | pollutant=="Particulate matter (PM10)"

*Focus on EU-25 to use a consistent sample as in our empirical analyses
drop if countryname=="Bulgaria" | countryname=="Croatia" | countryname=="Romania" | countryname=="Serbia" | countryname=="Switzerland" | countryname=="Norway"

*Focus on the binding period of the LCPD and IED 2008-2021 in our estimation sample
*In sum: We consider that any estimated pollution reductions estimated in the Main Specification are fully attributed to emission standards. 
*Using plant-level data from the EEA, we now assume that without policy intervention, emissions for each year after 2008 would have been higher relative to the previous year by a percentage equivalent to the ATTs in the Report.

*We leverage lagged emissions starting from the year prior to standards became binding (--> 2007-2020).
*For instance, we consider that, in the absence of treatment, emissions in 2008 would have been higher relative to 2007 by an amount equal to the ATT (for SO2 that is 39% higher). 
*We continue until we get to 2020 to predict emissions in 2021 without the estimated reductions induced by our ATTs.

drop if reportingyear==2021
drop if reportingyear==2022

gen year=2008 if reportingyear==2007
gen year=2009 if reportingyear==2008
gen year=2010 if reportingyear==2009
gen year=2011 if reportingyear==2010
gen year=2012 if reportingyear==2011
gen year=2013 if reportingyear==2012
gen year=2014 if reportingyear==2013
gen year=2015 if reportingyear==2014
gen year=2016 if reportingyear==2015
gen year=2017 if reportingyear==2016
gen year=2018 if reportingyear==2017
gen year=2019 if reportingyear==2018
gen year=2020 if reportingyear==2019
gen year=2021 if reportingyear==2020

*As we do not observe PM2.5 but only PM10, we follow the assumption in the UBA report (UBA, 2012) that 70% of PM10 is PM2.5
replace emissions=emissions*0.70 if pollutant=="Particulate matter (PM10)" 

*Emissions are in kilograms. Transform to tonnes and leverage the ATTs from the Brief Report
*We assume their emissions in each post-treatment period (i..e, from 2008) would have been higher than the previous year by a percentage equivalent to the ATTs presented in the Report.
gen red=emissions*0.39/1000 if pollutant=="Sulphur oxides (SOX)" 
replace red=emissions*0.28/1000 if pollutant=="Particulate matter (PM10)" 
replace red=emissions*0.14/1000 if pollutant=="Nitrogen oxides (NOX)" 

*Get cumulative emissions by pollutant over the regulated period
collapse(sum) red, by(pollutant)

*Generate variable refelecting the damage estimates from UBA
gen damage = 13875 if pollutant=="Nitrogen oxides (NOX)" 
replace damage = 54694 if pollutant=="Particulate matter (PM10)" 
replace damage = 13606 if pollutant=="Sulphur oxides (SOX)" 

*Compute pollutant-specific damage estimates
gen damage_estimate=red*damage

*From the manuscript (Main Specification)
gen main_Red=15195564 if pollutant=="Sulphur oxides (SOX)" 
replace main_Red=935873 if pollutant=="Particulate matter (PM10)" 
replace main_Red=4762361 if pollutant=="Nitrogen oxides (NOX)" 

*Double-check pollutant-specific damage estimates from the main estimates
gen damage_estimate_main=main_Red*damage

*Compute sum and residual health co-benefits
collapse(sum) damage_estimate damage_estimate_main
sum damage_estimate damage_estimate_main
gen residual_health=damage_estimate_main - damage_estimate

*in billion Euros
replace residual_health=residual_health/1000000000
*199.62306 or approx. 200

