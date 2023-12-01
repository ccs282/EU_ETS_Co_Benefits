# Replication of SDID and TWFE Analyses

This repository contains Stata codes to replicate the Synthetic Difference-in-Differences (SDID) and Two-Way Fixed Effects (TWFE) analyses presented in the paper titled *“The European Union Emissions Trading System yields large co-benefits from pollution reduction”* (by Pier Basaglia, Jonas Grunau, Moritz Drupp).

---

## Overview

The SDID and TWFE analyses are structured in three parts:

1. **Data Cleaning**: Ensuring a strongly balanced sample, which is a prerequisite for the SDID estimator (cf. [Arkhangelsky et. al, 2021](https://www.aeaweb.org/articles?id=10.1257/aer.20190159)).
2. **Estimation**: Utilizing the [Stata command sdid](https://github.com/Daniel-Pailanir/sdid) (version 2.0.0) developed by [Damian Clarke](https://www.damianclarke.net/) and [Daniel Pailañir](https://daniel-pailanir.github.io/), based on [Arkhangelsky et. al (2021)](https://www.aeaweb.org/articles?id=10.1257/aer.20190159) for the SDID and [Athey et al. (2021)](https://doi.org/10.1080/01621459.2021.1891924) for the matrix completion (MC) method. To leverage the SDID estimator, we chose the Stata package over its R counterpart because, as of the execution of this study in November 2023, the Stata package was the most up-to-date. Furthermore, the Stata version includes the option to implement the optimization procedure introduced by [Kranz (2022)](https://github.com/skranz/xsynthdid/tree/main/paper).
3. **Exponential Transformation**: Applying an exponential transformation to express the estimates in percentage terms, as our analyses are conducted using log-linear models.

---

## How to use

Refer to the provided do-file [Stata_SDID/dofiles/EUETS_SDID_SCM_DiD.do](/Stata_SDID/dofiles/EUETS_SDID_SCM_DiD.do), which contains commented codes to run the analyses step by step. It is crucial, as a first step, to adjust the directory of the code to where the data is being stored when running the analysis to ensure the code runs correctly. An example of how to do it is provided below:

``` Stata
* Insert Code for Adjusting Directory
global repo "C:/.../EU_ETS_Co_Benefits/"
```

Everything between `global repo` and `"/EU_ETS_Co_Benefits"` needs to be adjusted depending on where the user locally saved the repository.

---

## Dependencies

### Stata packages

``` Stata
ssc install sdid, replace
ssc install reghdfe, replace // (version 4.4.0)
ssc install estout, replace // To export tables with results
```

### Data inputs

``` Stata
Stata_SDID/data_in/so2_gscm_data.csv
Stata_SDID/data_in/pm25_gscm_data.csv
Stata_SDID/data_in/nox_gscm_data.csv
```

---

## Log files

All results conducted for this study have been run using Stata/SE 15.1 for Mac (64-bit Intel), Revision 03 Feb 2020, Copyright 1985-2017 StataCorp LLC. The log files of the analyses are provided in the directory [Stata_SDID/logs/](/Stata_SDID/logs/).
Users can refer to these log files for detailed information on each step of the analyses, including any warnings or errors that may have occurred during the execution of the code.

---

## Contact

If users have any doubts concerning the portion of the code written in Stata, they are encouraged to reach out to [Pier Basaglia](mailto:piero.basaglia@uni-hamburg.de).
