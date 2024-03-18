
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

*******************************************************************
**********Make this sample comparable to estimation sample*********
*******************************************************************
*Focus on EU-25 to use a consistent sample as in our empirical analyses
drop if countryname=="Bulgaria" | countryname=="Croatia" | countryname=="Romania" | countryname=="Serbia" | countryname=="Switzerland" | countryname=="Norway"

*Focus on the binding period of the LCPD and IED 2008-2021 in our estimation sample
drop if reportingyear==2007
drop if reportingyear==2022

drop if countryname=="United Kingdom" & reportingyear>=2020

*We assume their emissions in each post-treatment period (i..e, from 2008) would have been higher by a percentage equivalent to the ATTs presented in the Report (cf. SI Appendix).

*As we do not observe PM2.5 but only PM10, we follow the assumption in the UBA report (UBA, 2012) that 70% of PM10 is PM2.5
replace emissions=emissions*0.70 if pollutant=="Particulate matter (PM10)"

collapse(sum) emissions, by(pollutant reportingyear)

*Generate pollutant-specific variables that reflect ATTt over time

gen ATT_pct=0.006 if reportingyear==2008 & pollutant=="Nitrogen oxides (NOX)"
replace ATT_pct=0.09 if reportingyear==2009 & pollutant=="Nitrogen oxides (NOX)"
replace ATT_pct=0.07 if reportingyear==2010 & pollutant=="Nitrogen oxides (NOX)"
replace ATT_pct=0.06 if reportingyear==2011 & pollutant=="Nitrogen oxides (NOX)"
replace ATT_pct=0.07 if reportingyear==2012 & pollutant=="Nitrogen oxides (NOX)"
replace ATT_pct=0.12 if reportingyear==2013 & pollutant=="Nitrogen oxides (NOX)"
replace ATT_pct=0.14 if reportingyear==2014 & pollutant=="Nitrogen oxides (NOX)"
replace ATT_pct=0.20 if reportingyear==2015 & pollutant=="Nitrogen oxides (NOX)"
replace ATT_pct=0.26 if reportingyear==2016 & pollutant=="Nitrogen oxides (NOX)"
replace ATT_pct=0.30 if reportingyear==2017 & pollutant=="Nitrogen oxides (NOX)"
replace ATT_pct=0.27 if reportingyear==2018 & pollutant=="Nitrogen oxides (NOX)"
replace ATT_pct=0.33 if reportingyear==2019 & pollutant=="Nitrogen oxides (NOX)"
replace ATT_pct=0.30 if reportingyear==2020 & pollutant=="Nitrogen oxides (NOX)"
replace ATT_pct=0.28 if reportingyear==2021 & pollutant=="Nitrogen oxides (NOX)"

replace ATT_pct=0.12 if reportingyear==2008 & pollutant=="Sulphur oxides (SOX)" 
replace ATT_pct=0.23 if reportingyear==2009 & pollutant=="Sulphur oxides (SOX)" 
replace ATT_pct=0.29 if reportingyear==2010 & pollutant=="Sulphur oxides (SOX)" 
replace ATT_pct=0.26 if reportingyear==2011 & pollutant=="Sulphur oxides (SOX)" 
replace ATT_pct=0.34 if reportingyear==2012 & pollutant=="Sulphur oxides (SOX)" 
replace ATT_pct=0.41 if reportingyear==2013 & pollutant=="Sulphur oxides (SOX)" 
replace ATT_pct=0.39 if reportingyear==2014 & pollutant=="Sulphur oxides (SOX)" 
replace ATT_pct=0.43 if reportingyear==2015 & pollutant=="Sulphur oxides (SOX)" 
replace ATT_pct=0.53 if reportingyear==2016 & pollutant=="Sulphur oxides (SOX)" 
replace ATT_pct=0.56 if reportingyear==2017 & pollutant=="Sulphur oxides (SOX)" 
replace ATT_pct=0.61 if reportingyear==2018 & pollutant=="Sulphur oxides (SOX)" 
replace ATT_pct=0.64 if reportingyear==2019 & pollutant=="Sulphur oxides (SOX)" 
replace ATT_pct=0.59 if reportingyear==2020 & pollutant=="Sulphur oxides (SOX)" 
replace ATT_pct=0.61 if reportingyear==2021 & pollutant=="Sulphur oxides (SOX)" 

replace ATT_pct=0.17 if reportingyear==2008 & pollutant=="Particulate matter (PM10)"
replace ATT_pct=0.32 if reportingyear==2009 & pollutant=="Particulate matter (PM10)"
replace ATT_pct=0.30 if reportingyear==2010 & pollutant=="Particulate matter (PM10)"
replace ATT_pct=0.26 if reportingyear==2011 & pollutant=="Particulate matter (PM10)"
replace ATT_pct=0.36 if reportingyear==2012 & pollutant=="Particulate matter (PM10)"
replace ATT_pct=0.37 if reportingyear==2013 & pollutant=="Particulate matter (PM10)"
replace ATT_pct=0.29 if reportingyear==2014 & pollutant=="Particulate matter (PM10)"
replace ATT_pct=0.33 if reportingyear==2015 & pollutant=="Particulate matter (PM10)"
replace ATT_pct=0.37 if reportingyear==2016 & pollutant=="Particulate matter (PM10)"
replace ATT_pct=0.30 if reportingyear==2017 & pollutant=="Particulate matter (PM10)"
replace ATT_pct=0.32 if reportingyear==2018 & pollutant=="Particulate matter (PM10)"
replace ATT_pct=0.35 if reportingyear==2019 & pollutant=="Particulate matter (PM10)"
replace ATT_pct=0.32 if reportingyear==2020 & pollutant=="Particulate matter (PM10)"
replace ATT_pct=0.30 if reportingyear==2021 & pollutant=="Particulate matter (PM10)"


*Emissions are in kilograms. Transform to tonnes and leverage the ATTs from the Brief Report
gen red=((emissions/(1-ATT_pct))-emissions)/1000 if pollutant=="Sulphur oxides (SOX)" 
replace red=((emissions/(1-ATT_pct))-emissions)/1000 if pollutant=="Particulate matter (PM10)" 
replace red=((emissions/(1-ATT_pct))-emissions)/1000 if pollutant=="Nitrogen oxides (NOX)" 

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
*approx 160


