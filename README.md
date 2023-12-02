# Online Materials: Data, Code, Results, and Plots

- [Online Materials: Data, Code, Results, and Plots](#online-materials-data-code-results-and-plots)
  - [How to use](#how-to-use)
    - [Software requirements](#software-requirements)
    - [R Packages](#r-packages)
    - [Running the analysis](#running-the-analysis)
  - [Function arguments of `execute_analysis()`](#function-arguments-of-execute_analysis)
    - [Specification choices](#specification-choices)
    - [Direct `gsynth()` input](#direct-gsynth-input)
    - [Misc. arguments](#misc-arguments)
  - [Accessing plots](#accessing-plots)
    - [Plots created after each run](#plots-created-after-each-run)
    - [Plots of the Brief Report and SI Appendix](#plots-of-the-brief-report-and-si-appendix)
  - [Accessing results](#accessing-results)
  - [Miscellaneous](#miscellaneous)
    - [Replication information](#replication-information)
    - [Synthetic Difference-in-Difference Analysis](#synthetic-difference-in-difference-analysis)
    - [Contact information](#contact-information)
    - [Copyright](#copyright)

Welcome to the online repository of the paper *"The European Union Emissions Trading System yields large co-benefits from pollution reduction"* (by Pier Basaglia, Jonas Grunau, Moritz Drupp).

The following sections lay out the structure of the repository and provide instructions on how to replicate the analysis and plots of the paper. For information on methodological details, please refer to the Brief Report and the Supplementary Information (SI) Appendix first.

Please note that this README-file explains the main analysis, robustness checks, and plotting routines performed in R. The synthetic difference-in-difference analysis (SDID) was conducted in Stata and is explained in the file [./Stata_SDID/README.md](./Stata_SDID/README.md).

---

## How to use

### Software requirements

The free software R is required and can be downloaded [here](https://www.r-project.org/). Furthermore, we recommend a code editor/IDE such as [Visual Studio Code](https://code.visualstudio.com/download) or [RStudio](https://posit.co/downloads/).

### R Packages

The packages needed to prepare the data, run the analysis, and plot results are listed at the beginning of [./master.r](./master.r). The core package to conduct the GSCM analysis is the [gsynth package](https://yiqingxu.org/packages/gsynth/index.html) by [Yiqing Xu](https://yiqingxu.org/) and [Licheng Liu](https://polisci.mit.edu/people/licheng-liu).

To install them all, run:

``` r
install.packages(c(
        "tidyverse",    # version 2.0.0
        "gsynth",       # version 1.2.1
        "panelView",    # version 1.1.17
        "countrycode",  # version 1.5.0
        "here",         # version 1.0.1
        "uuid"          # version 1.1-1
))
```

Moreover, packages needed to replicate the polished plots in the paper ([click here for more details on the plots](#accessing-plots)) are:

``` r
install.packages(c(
        "patchwork",    # version 1.1.3
        "ggh4x",        # version 0.2.6
        "viridis"       # version 0.6.4
))
```

### Running the analysis

Having saved the repository locally on your computer, the entire analysis can be controlled from the custom function `execute_analysis()` in [./master.r](./master.r). Before doing so, it is important to start a fresh R process *from within the repository [EU_ETS_Co_Benefits](https://github.com/ccs282/EU_ETS_Co_Benefits.git)*. For instance, in VSC, use 'CTRL+K CTRL+O' to open the repository and only then start a new R process. Alternatively, initiate the editor/IDE of your choice, such as VSC or RStudio, by opening [./master.r](./master.r) with the respective software and then start a new R terminal.

It is not recommended to run the entire script [./master.r](./master.r) all at once. Instead, run the code from `library(tidyverse)` until `source(here("src", "R", "functions.r"))` to load the required packages, set the starting point for relative paths using `here()`, and load the custom functions required for the analysis.

The rest of [./master.r](./master.r) after `source(here("src", "R", "functions.r"))` contains specific commands that replicate the results for the specifications of our model presented in the Brief Report and SI Appendix, such as the two below, which also illustrate the ease of use of `execute_analysis()`.

``` r
# Main specification
# Fig. 1 & Col. 2 of Fig. 2 in the Brief Report
execute_analysis(
        pollutant = "so2" # "nox", "so2", "pm25"
)

# Main specification with matrix completion estimator
# Fig.2, Col. 4 in the Brief Report
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        estimator = "mc"
)

```

The different specifications should be run separately so that after each run the user can explore the [results](#accessing-results) and [plots](#accessing-plots).

`execute_analysis()` draws upon several R scripts in [./src/R](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/src/R), which are not explained further. A list of the relevant arguments of `execute_analysis()` is provided in the [next section](#function-arguments-of-execute_analysis).

---

## Function arguments of `execute_analysis()`

The following table contains a description of the relevant function arguments as well as default values and alternative options. Default values are highlighted in **bold** and marked with an asterisk*. For the best understanding of what each argument is doing, users are encouraged to search the code in the repository for the respective argument name and consult the Brief Report and SI Appendix.

### Specification choices

| Argument |  Description | Values |
| --- |  --- | --- |
| `pollutant` | The pollutant to consider in the analysis. | `"so2"` `"pm25"` `"nox"` |
| `main_data` | The main data source to use for the analysis. `"emep23/un"` chooses EMEP (v2023; v2021 for the UK, see SI Appendix) data as in our main specification. `"edgar"` chooses EDGAR data as in our robustness check. | `"edgar"` **`"emep23/un"`*** |
| `ets_start_year` | Sets the start year of the EU ETS, i.e., the beginning of the treatment. | `2002:2008`; **`2005`*** |
| `year_first` | The first year to consider in the analysis. EMEP data starts in 1990, whereas EDGAR data starts in 1970. | `1970:1995`; **`1990`*** |
| `year_last` | The last year to consider in the analysis. EMEP data is available until 2021, whereas EDGAR ends in 2018. | `2016:2021`; **`2021`*** |
| `treat_countries` | Sets the pool of treated countries. Can be one of the country groups listed or a vector of country names. For definitions of the country groups, check the script [./src/R/vectors_df.r](./src/R/vectors_df.r). | `"ets_countries"` `"eu28_countries"` **`"eu25_countries"`*** `"eu15_countries"` `"eu10_countries"` `"sdid_countries"` |
| `treat_sectors` | Determines which treated sectors to analyze. `"ets_sectors"` considers all treated sectors available in the data. Also possible to choose only certain sectors. | **`"ets_sectors"`*** `"energy_sectors"` `"metals_sectors"` `"minerals_sectors"` `"chemicals_sectors"` `"paper_sectors"` |
| `donor_countries` | Countries to include in the donor pool. Same country groups (or country name vectors) as in `treat_countries` possible. `"same_as_treat"` chooses the same countries as are specified in `treat_countries`. | `"ets_countries"` `"eu28_countries"` `"eu25_countries"` `"eu10_countries"` `"eu15_countries"` `"sdid_countries"` **`"same_as_treat"`*** |
| `donor_sectors` | The sectors to contribute to the units in the donor pool. For the current analysis, `"all_available"` chooses all non-ETS-regulated sectors. Also possible to insert a vector of sector codes. | **`"all_available"`*** |
| `leave_one_out` | Set to `TRUE` if one conducts a leave-one-out test. | `TRUE` **`FALSE`*** |
| `country_to_leave_out` | Only relevant when `leave_one_out == TRUE`. The country to leave out in the analysis. Provide a vector of country names. | (see Description) |
| `include_1a2` | Only relevant when `main_data == "edgar"` as there is no data on the sub-sectors of 1.A.2 in EDGAR (see SI Appendix). Determines whether and how to include data of the sector 1.A.2 when using EDGAR data. `"emep21+23/un"` uses EMEP data (2023 version; 2021 version for the UK) of 1.A.2 to calculate the share of each sub-sector within 1.A.2 for each country and year. The shares are then used to split the EDGAR data on 1.A.2 into the sub-sectors of 1.A.2. `"edgar_fully_treated"` considers the full sector 1.A.2 as treated, whereas `"edgar_fully_control"` considers the full sector 1.A.2 as control. `"no"` does not include the sector 1.A.2 in the analysis. | `"no"` `"edgar_fully_treated"` `"edgar_fully_control"` **`"emep21+23/un"`*** |
| `include_bio` | Only relevant when `main_data == "edgar"`. Determines whether to include bio emissions data. | **`"yes"`*** `"no"` |
| `include_uk` | Determines whether to include UK data. The 2023 version of EMEP does not contain UK data. When choosing `main_data == "emep23/un"`, UK data is taken from the 2021 version of EMEP. | **`TRUE`*** `FALSE` |
| `include_aviation` | Determines whether to include aviation data. The sector has only been regulated since 2012. In our main specification, the sector contributes to the control units (as in Bayer & Aklin, 2020 to the best of our knowledge). In a robustness check, we exclude it from the analysis. | **`"control"`*** `"exclude"` |
| `balanced_panel` | Determines whether to use a balanced panel. | `TRUE` **`FALSE`*** |
| `gaps_in_years` | Determines how to handle gaps in the data, e.g., missing data for the year 2015 for the unit France-regulated (hypothetically). `"interpolate"` interpolates the missing data points if only one year is missing. `"drop"` drops the unit from the analysis. | **`"interpolate"`*** `"drop"` |
| `treatment_timing` | Determines whether to allow staggered treatment timing. In our main approach, we set it to `"common"`, meaning that `"ets_start_year` determines the start year of the treatment for all countries. When choosing `"staggered"`, the start year of the treatment for each country is determined by when the country joined the EU ETS. | `"staggered"` **`"common"`*** |
| `ensure_common` | Only relevant when `"treatment_timing" == "common"`. If `"ensure_common" == TRUE`, countries that are not yet regulated in the year equal to `"ets_start_year"` are dropped. In our sample (EU-25), which we adopt from Bayer & Aklin (2020), no countries are dropped - neither for `"ets_start_year" = 2005` nor for `"ets_start_year" = 2008` as they all joined at the beginning of the EU ETS. | **`TRUE`*** `FALSE` |
| `damage_est_source` | The source of the social costs of pollution estimates. `"uba_eu_27` uses the EU-27 estimates mentioned in the SI Appendix. `"uba_germany_3_1"` applies cost estimates for Germany to all countries ([see here](https://www.umweltbundesamt.de/en/publikationen/methodological-convention-31-for-the-assessment-of)). | `"uba_germany_3_1"` **`"uba_eu_27"`*** |
| `per_capita_emissions` | Determines whether to use per capita emissions data. | `TRUE` **`FALSE`*** |
| `per_capita_gdp` | Determines whether to use per capita GDP data (not as an additional covariate but for the main covariates log(GDP) and log(GDP)$^2$). | `TRUE` **`FALSE`*** |
| `gdp` | Determines the type of GDP data to use. | **`"constant"`*** `"current"` |

### Direct `gsynth()` input

| Argument |  Description | Values |
| --- |  --- | --- |
| `estimator` | The estimator to use for the GSCM. Entering `gsynth()` through its `estimator` argument. | **`"ife"`*** `"mc"` |
| `covariates` | The covariates to include in the IFE model (see SI Appendix). `"standard"` includes log(GDP) and log(GDP)$^2$ (as in Bayer & Aklin, 2020). `"none"` lets the IFE model run without any covariates. The other four options add one more variable to the `"standard"` version, respectively, namely population, GDP per capita, renewable electricity capacity, and a dummy for carbon pricing schemes (see SI Appendix). Entering `gsynth()` through its `formula` argument. | **`"standard"`*** `"population"` `"gdp_pc"` `"renew_elec"` `"carbon_pricing_dummy"` `"none"` |
| `cv` | Determines whether to use the cross-validation procedure to choose the optimal number of factors. See `r` description for more details. Entering `gsynth()` through its `CV` argument. | **`TRUE`*** `FALSE` |
| `r` | Number of factors. If `cv == TRUE`, the cross-validation procedure will select the optimal number of factors from `r` to 5 (`gsynth()` documentation). If `cv == FALSE`, the number of factors will be set to `r`. Entering `gsynth()` through its `r` argument. | (integer), **`0`*** |
| `criterion` | The criterion to use when determining the number of factors. Entering `gsynth()` through its `criterion` argument. | **`"mspe"`*** `"pc"` |
| `inference_type` | The type of inference to use. `"choose"` selects `"parametric"` when the number of treated units is too small ($N_{Tr} \leq 40$) as recommended [in the gsynth tutorial](https://yiqingxu.org/packages/gsynth/articles/tutorial.html) and `"nonparametric"` otherwise. Entering `gsynth()` through its `inference` argument. | `"parametric"` `"nonparametric"` **`"choose"`*** |
| `alpha` | Sets the significance level. Entering `gsynth()` through its `alpha` argument. | $[0, 1]$; **`0.05`*** |
| `min_t0` | The minimum number of pre-treatment years, $T_0$. Entering `gsynth()` through its `min.T0` argument. | (integer); **`7`*** |

### Misc. arguments

| Argument |  Description | Values |
| --- |  --- | --- |
| `prep_data` | Determines whether the data for the analysis shall be prepared from scratch or read from a csv. Setting it to `TRUE` requires downloading two large data sets. First, download [EMEP21](https://sdi.eea.europa.eu/catalogue/srv/api/records/22accc6a-dfbd-4ed0-9c38-d0b51f86a81a) and save it under [./data/pollution/emep/CLRTAP_NVFR14_V21_GF.csv](./data/pollution/emep/CLRTAP_NVFR14_V21_GF.csv). Second, download [EMEP23](https://sdi.eea.europa.eu/catalogue/srv/api/records/2999364f-be52-4012-b4fd-f98e2cc8fab6?language=all) and save it under [./data/pollution/emep/CLRTAP_NVFR19_V23_1_GF_csv.csv](./data/pollution/emep/CLRTAP_NVFR19_V23_1_GF_csv.csv). | `TRUE` **`FALSE`*** |
| `write_files` | Only relevant when `prep_data == TRUE`. Determines whether the prepared data sets shall be saved as .csv. | `TRUE` **`FALSE`*** |
| `save_plots` | Determines whether to save plots under [./plots/](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots). | **`TRUE`*** `FALSE` |
| `annotate_plots` | Determines how to annotate plots (not the polished paper plots under [./plots/paper/](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots/paper) but the plots displayed after each run when calling `plots$att` and `plots$ct_tr`). `"att"` inserts the average ATT into the Figure, while `"no"` provides no annotation. | **`"att"`*** `"no"` |
| `show_lines` | Determines which lines to show in the plots post-analysis when running `plots$ct_tr`. `"tr"` `"ct"`, and `"co"` refer to treated, counterfactual, and control units, respectively. | `"ct"` `"co"` `"tr"`; **`c("tr", "ct")`*** |
| `conduct_analysis` | Determines whether to conduct the GSCM analysis. If `FALSE`, `execute_analysis()` will only prepare the data. | **`TRUE`*** `FALSE` |
| `write_results_table` | Determines whether to write the results tables under [./results](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/results). See [here](#accessing-results) for more information. | **`TRUE`*** `FALSE` |

---

## Accessing plots

There are two kinds of plots in this repository: (1) rough plots saved after each run of `execute_analysis()` under [./plots](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots) and (2) polished plots in the design used in the Brief Report that are saved under [./plots/paper](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots/paper).

### Plots created after each run

After each run, `execute_analysis()` creates multiple plots and saves them. The plots can be called through the following commands in [./master.r](./master.r): `plots$panelview` for a plot that shows the completeness of the panel data, `plots$att` (`plots$ct_tr`) for a plot similar to the bottom (top) panel of Figure 1 in the Brief Report.

These plots are saved under [./plots](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots) in the folders named after the three pollutants and [./plots/misc](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots/misc) if `save_plots == TRUE`. The path to the plots is based on specification choices. For instance, the main specification plot for SO$_2$ is located under [./plots/so2/trse-ets_trco-eu25/dose-all_doco-eu25/em23/cov-std/1a2em2123_bio/1990_2005_2021_p_att.png](https://github.com/ccs282/EU_ETS_Co_Benefits/blob/main/plots/so2/trse-ets_trco-eu25/dose-all_doco-eu25/em23/cov-std/1a2em2123_bio/1990_2005_2021_p_att.png). When the specification choice cannot be expressed as a file path, they are saved under [./plots/misc](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots/misc) in a folder named after a unique identifier for each run.

To make locating plots convenient, users can call `misc_parameters$uuid` after each run to obtain the unique identifier. Searching the repository for this identifier will lead to the location under which the plots were saved. In the main results table ([see below](#accessing-results)), the unique identifier is also listed.

### Plots of the Brief Report and SI Appendix

The plots displayed in the Brief Report and mentioned in the SI Appendix are in [./plots/paper/](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots/paper) and [./plots/robustness_checks/](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots/robustness_checks), respectively. Please refer to the two documents for more details. The plots of the Brief Report can be replicated in [./src/R/plots_paper.r](./src/R/plots_paper.r).

---

## Accessing results

The results of each run are saved in the list `gscm_analysis`. Please see the `gsynth()` documentation (`help("gsynth")`) for more information on the structure of the list.

Moreover, the results are saved in a .csv under [./results/results_table.csv](./results/results_table.csv), which is also available in .xlsx format under [./results/2023_11_30_results_table.xlsx](./results/2023_11_30_results_table.xlsx). The columns of the table closely mirror the function arguments of `execute_analysis()` (see [here](#function-arguments-of-execute_analysis)), output of the `gsynth()` function as well as manual calculations of the absolute effect in physical units. For details, users are encouraged to search the code under [./src/R/results_table.r](./src/R/results_table.r) for the respective column name.

The tables contain the results of all our runs. As it also lists the unique identifier of each run, users can easily locate the plots linked to the respective specification (see [here](#plots-created-after-each-run)) by searching the repository for that ID. Moreover, filtering the columns (e.g., via Excel in [./results/2023_11_30_results_table.xlsx](./results/2023_11_30_results_table.xlsx)) allows user to find the results of a specific run.

---

## Miscellaneous

### Replication information

More information on the technical details of how the analysis was conducted can be found at the top of [./master.r](./master.r).

Please note that the repository is self-contained, i.e., contains all materials necessary to replicate the findings. The only exception to this is when setting `data_prep == TRUE` (see [here](#misc-arguments)), which manually constructs the data set again. This is not necessary as the prepared data is saved under [./data/final_data/](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/data/final_data) already.

### Synthetic Difference-in-Difference Analysis

As mentioned above, the synthetic difference-in-difference analysis (SDID) was conducted in Stata. The material to replicate it can be found under [./Stata_SDID/](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/Stata_SDID). Please refer to [./Stata_SDID/README.md](./Stata_SDID/README.md) for further details.

### Contact information

For questions on the R code, please contact [Jonas Grunau](mailto:jonas.sebastian.grunau@uni-hamburg.de). For questions on the SDID analysis, please contact [Pier Basaglia](mailto:piero.basaglia@uni-hamburg.de).

### Copyright

To see the license of our repository, please see [./LICENSE](./LICENSE).
