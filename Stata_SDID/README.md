# Replication of SDID and TWFE Analyses + Back-of-the-envelope calculations

This repository contains Stata codes to replicate the Synthetic Difference-in-Differences (SDID) and Two-Way Fixed Effects (TWFE) analyses presented in the paper titled *“The European Union Emissions Trading System yields large co-benefits from pollution reduction”* (by Pier Basaglia, Jonas Grunau, Moritz Drupp). It additionally includes back-of-the-envelope calculations to bound the role of combustion standards based on data from the [European Environment Agency]([https://www.damianclarke.net/](https://sdi.eea.europa.eu/data/63a14e09-d1f5-490d-80cf-6921e4e69551?path=%2FUser%20friendly%20.csv%20file)). More details on the SDID and TWFE analyses follow below, while for more details on the back-of-the-envelope calculations see see do-file [Stata_SDID/dofiles/Standards_back_of_the_envelope_2.do](/Stata_SDID/dofiles/Standards_back_of_the_envelope_2.do)).

---

## Overview

The SDID and TWFE analyses are structured in three parts:

1. **Data Cleaning**: Ensuring a strongly balanced sample, which is a prerequisite for the SDID estimator (cf. [Arkhangelsky et. al, 2021](https://www.aeaweb.org/articles?id=10.1257/aer.20190159)).
2. **Estimation**: Utilizing the [Stata command sdid](https://github.com/Daniel-Pailanir/sdid) (version 2.0.0) developed by [Damian Clarke](https://www.damianclarke.net/) and [Daniel Pailañir](https://daniel-pailanir.github.io/), based on [Arkhangelsky et. al (2021)](https://www.aeaweb.org/articles?id=10.1257/aer.20190159). To leverage the SDID estimator, we chose the Stata package over its R counterpart because, as of the execution of this study in November 2023, the Stata package was the most up-to-date. Furthermore, the Stata version includes the option to implement the optimization procedure introduced by [Kranz (2022)](https://github.com/skranz/xsynthdid/tree/main/paper).
3. **Exponential Transformation**: Applying an exponential transformation to express the estimates in percentage terms, as our analyses are conducted using log-linear models.

---

## How to use

Refer to the provided do-file [Stata_SDID/dofiles/EUETS_SDID_SCM_DiD.do](/Stata_SDID/dofiles/EUETS_SDID_SCM_DiD.do), which contains commented codes to run the analyses step by step. It is crucial, as a first step, to adjust the directory of the code to where the data is being stored when running the analysis to ensure the code runs correctly. An example of how to do it is provided below:

```Stata
* Insert Code for Adjusting Directory
global repo "C:/.../EU_ETS_Co_Benefits/"
```

Everything between `global repo` and `"/EU_ETS_Co_Benefits"` needs to be adjusted depending on where the user locally saved the repository.

---

## Dependencies

### Stata packages

```Stata
ssc install sdid, replace
ssc install reghdfe, replace // (version 4.4.0)
ssc install estout, replace // To export tables with results
```

### Data inputs

The data sets used for the SDID analysis are provided under [./data_in/](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/Stata_SDID/data_in). The file [master.r](https://github.com/ccs282/EU_ETS_Co_Benefits/blob/main/master.r) contains the code that has generated the data sets.

---

## Log files

All results conducted for this study have been run using Stata/SE 15.1 for Mac (64-bit Intel), Revision 03 Feb 2020, Copyright 1985-2017 StataCorp LLC. The log files of the analyses are provided in the directory [Stata_SDID/logs/](/Stata_SDID/logs/).
Users can refer to these log files for detailed information on each step of the analyses, including any warnings or errors that may have occurred during the execution of the code.

---

## Accessing results

Results presented in the SI Appendix (expressed in percentage terms) from the SDID and TWFE analyses can be found in [Stata_SDID/results/](/Stata_SDID/results/) in a CSV format. The estimated log-linear coefficients before the exponential transformation are available in CSV tables located at [Stata_SDID/tables/](/Stata_SDID/tables/). These coefficients are additionally stored as XSLX files within [Stata_SDID/tables/](/Stata_SDID/tables/) during Step 2 of the SDID analysis (refer to [Overview](#overview) above). These files are saved with the '_tbc' tag for future reference within the code and they will undergo exponential transformation into percentage terms during Step 3 of the analysis.

---

## Contact

If users have any questions concerning the portion of the code written in Stata, they are encouraged to reach out to [Pier Basaglia](mailto:piero.basaglia@uni-hamburg.de).
