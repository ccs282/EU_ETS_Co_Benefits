
/**************************************************************************************************************************************************/
//  PROJECT:............The co-benefits of the EU ETS
//  PARTICIPANTS:.......Pier Basaglia, Jonas Grunau, and Moritz Drupp   				                
//  DATE:...............March 2024                       			
//  Task of program: .. Descriptive statistics on fuel price variations 
//                     
// All analyses have been run with:
// Stata/SE 15.1 for Mac (64-bit Intel)
// Revision 03 Feb 2020
// Copyright 1985-2017 StataCorp LLC

/**************************************************************************************************************************************************/

/*
This repository contains Stata codes to replicate the descriptive statistics on fuel price variations over time using data from the IMF Primary Commodity Prices (accessible fom here: https://www.imf.org/en/Research/commodity-prices)

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

*Data inputs to run the codes: 
	* ${home}STATA/data_in/PCPS_03-11-2024 11-09-48-75_panel.csv
    *${home}STATA/data_in/fuel_prices_data.csv", clear 


import delimited "${dataIn}PCPS_03-11-2024 11-09-48-75_panel.csv", delimiter(",") encoding(utf8) clear 
keep if unitname=="US Dollars"

*Generate average across oil and coal export prices (corrleation higher than 0.95)
bysort timeperiod: gen oil_mean=(brentcrudepoilbre+wticrudepoilwti+dubaicrudepoildub)/3
bysort timeperiod: gen coal_mean=(coalaustraliapcoalau+coalsouthafricapcoalsa)/2

capt label variable naturalgaseupngaseu "Gas"
capt label variable oil_mean "Oil"
capt label variable coal_mean "Coal"

*Obtain index (100=2004) to make price variations comparable across different fuels
sum naturalgaseupngaseu if timeperiod==2004
generate gas_index = 100 * naturalgaseupngaseu / r(mean) 

sum oil_mean if timeperiod==2004
generate oil_index = 100 * oil_mean / r(mean) 

sum coal_mean if timeperiod==2004
generate coal_index = 100 * coal_mean / r(mean) 

preserve

rename timeperiod year
keep year gas_index oil_index coal_index 

*Save data
export delimited using "${dataOut}fuel_prices_index_2004.csv", replace
save "${dataOut}fuel_prices_index_2004.dta", replace

restore

capt label variable gas_index "Gas"
capt label variable oil_index "Oil"
capt label variable coal_index "Coal"

 line gas_index oil_index coal_index timeperiod,  lwidth(thick thick thick) ///
 graphregion(fcolor(white) lcolor(white)ifcolor(white)) scheme(white_tableau) xline(2005, lcolor(black) lwidth(thin) lpattern(solid)) ///
 scale(0.7) legend(pos(11) ring(0) row(3) size(5)) xlabel(#10, labsize(medlarge) nogrid) ylabel(0(100)500, labsize(medlarge) nogrid) ///
 xtitle("", size(medlarge)) ytitle("Index (2004=100)", size(medlarge)) aspect(1) name(index, replace) 
  graph export "${figures}fuel_prices_variation_index_2004.pdf", replace

  /* 
  *Obtain variations in absolute prices per unit over time
 line naturalgaseupngaseu oil_mean coal_mean timeperiod,  lwidth(thick thick thick) ///
 graphregion(fcolor(white) lcolor(white)ifcolor(white)) scheme(white_tableau) xline(2005, lcolor(black) lwidth(thin) lpattern(solid)) ///
 scale(0.7) legend(pos(11) ring(0) row(3) size(4)) xlabel(#10, labsize(medlarge) nogrid) ylabel(, labsize(medlarge) nogrid) ///
 xtitle("", size(medlarge)) ytitle("USD dollars / unit", size(medlarge)) 
  graph export "${figures}fuel_prices_variation.pdf", replace
  */
 *****

import delimited "${dataIn}PCPS_03-11-2024 11-09-48-75_panel.csv", delimiter(",") encoding(utf8) clear 
keep if unitname=="Percent Change over Previous Period"

bysort timeperiod: gen oil_mean=(brentcrudepoilbre+wticrudepoilwti+dubaicrudepoildub)/3
bysort timeperiod: gen coal_mean=(coalaustraliapcoalau+coalsouthafricapcoalsa)/2

capt label variable naturalgaseupngaseu "Gas"
capt label variable oil_mean "Oil"
capt label variable coal_mean "Coal"

 line naturalgaseupngaseu oil_mean coal_mean timeperiod, lwidth(thick thick thick)  ///
 graphregion(fcolor(white) lcolor(white)ifcolor(white)) scheme(white_tableau) xline(2005, lcolor(black) lwidth(thin) lpattern(solid)) ///
 scale(0.7) legend(off) xlabel(#10, labsize(medlarge) nogrid) ylabel(, labsize(medlarge) nogrid) ///
 xtitle("", size(medlarge)) ytitle("Percent change over previous year", size(medlarge)) yline(0, lcolor(gray) lwidth(thin) lpattern(dash)) aspect(1) name(pctchange, replace) 
  graph export "${figures}fuel_prices_percent_growth.pdf", replace
  
    preserve
	
*Generate correlograms including all IMF energy commodity prices considered	
	
	keep if timeperiod>=2005
  
  rename naturalgaseupngaseu PNGASEU
    rename brentcrudepoilbre POILBRE
	  rename coalaustraliapcoalau PCOALAU
	    rename coalsouthafricapcoalsa PCOALSA
    rename dubaicrudepoildub POILDUB
	  rename wticrudepoilwti POILWTI
		
  * Only change names of variable in local var_corr. 
  * The code will hopefully do the rest of the work without any hitch
  local var_corr PNGASEU POILBRE POILWTI POILDUB PCOALAU PCOALSA
  local countn : word count `var_corr'
  
  * Use correlation command
  quietly correlate `var_corr'
  matrix C = r(C)
  local rnames : rownames C
  
  * Now to generate a dataset from the Correlation Matrix
  clear
   
   * For no diagonal and total count
   local tot_rows : display `countn' * `countn'
   set obs `tot_rows'
   
   generate corrname1 = ""
   generate corrname2 = ""
   generate y = .
   generate x = .
   generate corr = .
   generate abs_corr = .
   
   local row = 1
   local y = 1
   local rowname = 2
    
   foreach name of local var_corr {
    forvalues i = `rowname'/`countn' { 
     local a : word `i' of `var_corr'
     replace corrname1 = "`name'" in `row'
     replace corrname2 = "`a'" in `row'
     replace y = `y' in `row'
     replace x = `i' in `row'
     replace corr = round(C[`i',`y'], .01) in `row'
     replace abs_corr = abs(C[`i',`y']) in `row'
     
     local ++row
     
    }
    
    local rowname = `rowname' + 1
    local y = `y' + 1
   
   }
   
  drop if missing(corrname1)
  replace abs_corr = 0.1 if abs_corr < 0.1 & abs_corr > 0.04
  
  colorpalette9 HCL pinkgreen, n(10) nograph intensity(0.05)
  *colorpalette9 CET CBD1, n(10) nograph //Color Blind Friendly option
  generate colorname = ""
  local col = 1
  forvalues colrange = -1(0.2)0.8 {
   replace colorname = "`r(p`col')'" if corr >= `colrange' & corr < `=`colrange' + 0.2'
   replace colorname = "`r(p10)'" if corr == 1
   local ++col
  } 
  
  
  * Plotting
  * Saving the plotting code in a local 
  forvalues i = 1/`=_N' {
  
   local slist "`slist' (scatteri `=y[`i']' `=x[`i']' "`: display %3.2f corr[`i']'", mlabposition(0) mlabcolor("black") msize(`=abs_corr[`i']*15') mcolor("`=colorname[`i']'") mcolor(*.8))"
  
  }
  
  
  * Gather Y axis labels
  labmask y, val(corrname1)
  labmask x, val(corrname2)
  
  levelsof y, local(yl)
  foreach l of local yl {
   local ylab "`ylab' `l'  `" "`:lab (y) `l''" "'" 
   
  } 

  * Gather X Axis labels
  levelsof x, local(xl)
  foreach l of local xl {
   local xlab "`xlab' `l'  `" "`:lab (x) `l''" "'" 
   
  }  
  
  * Plot all the above saved lolcas
  twoway `slist', title("Correlations between percent changes after the start of the EU ETS", size(3) pos(11)) ///
      note("Dataset Used: {bf:IMF Primary Commodity Prices}." "The original variable names correspond to the following commodities:" "{bf:PNGASEU:} EU Natural Gas TTF, {bf:POILBRE:} Brent crude, {bf:POILWTI:} WTI crude, {bf:POILDUB:} Dubai crude," "{bf:PCOALAU:} Australian coal export price, {bf:PCOALSA:} South African coal export price", size(2) margin(t=5)) ///
    xlabel(`xlab', labsize(2.5)) ylabel(`ylab', labsize(2.5)) ///
    xscale(range(1.75 )) yscale(range(0.75 )) ///
    ytitle("") xtitle("") graphregion(fcolor(white) lcolor(white)ifcolor(white))  ///
    legend(off) xsize(1) ysize(1) ///
    aspect(1) scale(0.8) ///
    scheme(white_tableau) name(correlogram, replace) 
	
	
graph export "${figures}IMF_fuel_percent_change_correlogram.pdf", replace	
	
	restore
	
graph  combine index pctchange, xsize(10) ysize(5) rows(1) graphregion(fcolor(white) lcolor(white)ifcolor(white) margin(nomargin))
graph export "${figures}relative_fuel_price_changes.pdf", replace	


**Import weighted fuel price change data****

import delimited "${dataIn}fuel_prices_data.csv", clear 

*Change UK name before reshaping to avoid errors
replace country="UK" if country=="United Kingdom"
reshape wide fuel_price, i(year) j(country) string
 
 *Use country codes
 rename fuel_priceAustria AT 
 rename fuel_priceBelgium BE 
 rename fuel_priceCyprus CY 
 rename fuel_priceCzechia CZ 
 rename fuel_priceDenmark DK 
 rename fuel_priceEstonia EE 
 rename fuel_priceFinland FI 
 rename fuel_priceFrance FR 
 rename fuel_priceGermany DE 
 rename fuel_priceGreece GR 
 rename fuel_priceHungary HU 
 rename fuel_priceIreland IE 
 rename fuel_priceItaly IT 
 rename fuel_priceLatvia LV 
 rename fuel_priceLithuania LT 
 rename fuel_priceLuxembourg LU 
 rename fuel_priceMalta MT 
 rename fuel_priceNetherlands NL 
 rename fuel_pricePoland PL 
 rename fuel_pricePortugal PT 
 rename fuel_priceSlovakia SK 
 rename fuel_priceSlovenia SI 
 rename fuel_priceSpain ES 
 rename fuel_priceSweden SE 
 rename fuel_priceUK UK

 *Generate correlograms	
	
  preserve
  
  keep if year>=2005
  
  * Only change names of variable in local var_corr. 
  * The code will hopefully do the rest of the work without any hitch
  local var_corr AT BE CY CZ DK EE FI FR DE GR HU IE IT LV LT LU MT NL PL PT SK SI ES SE UK
  local countn : word count `var_corr'
  
  * Use correlation command
  quietly correlate `var_corr'
  matrix C = r(C)
  local rnames : rownames C
  
  * Now to generate a dataset from the Correlation Matrix
  clear
   
   * For no diagonal and total count
   local tot_rows : display `countn' * `countn'
   set obs `tot_rows'
   
   generate corrname1 = ""
   generate corrname2 = ""
   generate y = .
   generate x = .
   generate corr = .
   generate abs_corr = .
   
   local row = 1
   local y = 1
   local rowname = 2
    
   foreach name of local var_corr {
    forvalues i = `rowname'/`countn' { 
     local a : word `i' of `var_corr'
     replace corrname1 = "`name'" in `row'
     replace corrname2 = "`a'" in `row'
     replace y = `y' in `row'
     replace x = `i' in `row'
     replace corr = round(C[`i',`y'], .01) in `row'
     replace abs_corr = abs(C[`i',`y']) in `row'
     
     local ++row
     
    }
    
    local rowname = `rowname' + 1
    local y = `y' + 1
   
   }
   
  drop if missing(corrname1)
  replace abs_corr = 0.1 if abs_corr < 0.1 & abs_corr > 0.04
  
  colorpalette9 HCL pinkgreen, n(10) nograph intensity(0.05)
  *colorpalette9 CET CBD1, n(10) nograph //Color Blind Friendly option
  generate colorname = ""
  local col = 1
  forvalues colrange = -1(0.2)0.8 {
   replace colorname = "`r(p`col')'" if corr >= `colrange' & corr < `=`colrange' + 0.2'
   replace colorname = "`r(p10)'" if corr == 1
   local ++col
  } 
  
  
  * Plotting
  * Saving the plotting code in a local 
  forvalues i = 1/`=_N' {
  
   local slist "`slist' (scatteri `=y[`i']' `=x[`i']' "`: display %3.2f corr[`i']'", mlabposition(0) mlabcolor("black") msize(`=abs_corr[`i']*5') mcolor("`=colorname[`i']'") mcolor(*.8))"
  
  }
  
  
  * Gather Y axis labels
  labmask y, val(corrname1)
  labmask x, val(corrname2)
  
  levelsof y, local(yl)
  foreach l of local yl {
   local ylab "`ylab' `l'  `" "`:lab (y) `l''" "'" 
   
  } 

  * Gather X Axis labels
  levelsof x, local(xl)
  foreach l of local xl {
   local xlab "`xlab' `l'  `" "`:lab (x) `l''" "'" 
   
  }  
  
  * Plot all the above saved lolcas
  twoway `slist', title("Correlations between weighted fuel price changes after the start of the EU ETS", size(2.5) pos(11)) ///
      note("Notes: Units correspond to standard two-letter ISO 3166-1 alpha-2 country codes", size(2) margin(t=5)) ///
    xlabel(`xlab', labsize(2.5)) ylabel(`ylab', labsize(2.5)) ///
    xscale(range(1.75 )) yscale(range(0.75 )) ///
    ytitle("") xtitle("") graphregion(fcolor(white) lcolor(white)ifcolor(white))  ///
    legend(off) xsize(1) ysize(1) ///
    aspect(1) scale(0.8) ///
    scheme(white_tableau) name(correlogram, replace) 
	
	
graph export "${figures}weighted_fuel_price.pdf", replace	
	
	restore




	
  
