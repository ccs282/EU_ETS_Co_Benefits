# Online Materials: Data, Code, Results, and Plots

Welcome to the online repository of the paper *"The European Union Emissions Trading System yields large co-benefits from pollution reduction"* (by Pier Basaglia, Jonas Grunau, Moritz Drupp).

The following sections lay out the structure of the repository and provide instructions on how to replicate the analysis and plots of the paper. For information on methodological details, please refer to the Brief Report and the Supplementary Information (SI) Appendix first.

Please note that this README-file explains the main analysis, robustness checks, and plotting routines performed in R. The synthetic difference-in-difference analysis (SDID) was conducted in Stata and is explained in the file [./Stata_SDID/README.md](./Stata_SDID/README.md).

---

## How to use

### Software requirements

The free software R is required and can be downloaded [here](https://www.r-project.org/). Furthermore, we recommend a code editor/IDE such as [Visual Studio Code](https://code.visualstudio.com/download) (VSC) or [RStudio](https://posit.co/downloads/).

### R Packages

The packages needed to prepare the data, run the analysis, and plot results are listed at the beginning of [./master.r](./master.r). The core package to conduct the GSCM analysis is the [gsynth package](https://yiqingxu.org/packages/gsynth/index.html) developed by [Yiqing Xu](https://yiqingxu.org/) and [Licheng Liu](https://polisci.mit.edu/people/licheng-liu).

To install them all, run:

```R
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

```R
install.packages(c(
        "patchwork",    # version 1.1.3
        "ggh4x",        # version 0.2.6
        "viridis"       # version 0.6.4
))
```

### Running the analysis

Having saved the repository locally on your computer, the entire analysis can be controlled from the custom function `execute_analysis()` in [./master.r](./master.r). Before doing so, it is important to start a fresh R process *from within the repository [EU_ETS_Co_Benefits](https://github.com/ccs282/EU_ETS_Co_Benefits.git)*. For instance, in VSC, use 'CTRL+K CTRL+O' to open the repository and only then start a new R process. Alternatively, initiate the editor/IDE of your choice, such as VSC or RStudio, by opening [./master.r](./master.r) with the respective software and then start a new R terminal.

It is not recommended to run the entire script [./master.r](./master.r) all at once as it mostly consists of repeated calls of the function `execute_analysis()` for the different specifications in the Brief Report and SI Appendix. Instead, run the code blocks from `library(tidyverse)`, $\ldots$, `here()`, $\ldots$ up until `source(here("src", "R", "functions.r"))`, which automatically load the required packages, set the starting point for relative paths, and load the custom functions required for the analysis.

The rest of [./master.r](./master.r) after `source(here("src", "R", "functions.r"))` contains specific commands that replicate the results for the specifications of our model presented in the Brief Report and SI Appendix, such as the two below, which also illustrate the ease of use of `execute_analysis()`.

```R
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

### Function arguments of `execute_analysis()`

The following table contains a description of the relevant function arguments as well as default values and alternative options. Default values are highlighted in **bold** and marked with an asterisk*. For the best understanding of what each argument is doing, users are encouraged to search the code in the repository for the respective argument name and consult the Brief Report and SI Appendix.

<details>
<summary> Click here for various data-related specification choices </summary>

| Argument |  Description | Values |
| --- |  --- | --- |
| `pollutant` | The pollutant to consider in the analysis. | `"so2"` `"pm25"` `"nox"` |
| `main_data` | The main data source to use for the analysis. `"emep23/un"` chooses EMEP (v2023; v2021 for the UK, see SI Appendix) data as in our main specification. `"edgar"` chooses EDGAR data as in our robustness check. | `"edgar"` **`"emep23/un"`*** |
| `ets_start_year` | Sets the start year of the EU ETS, i.e., the beginning of the treatment. | `2002:2008`; **`2005`*** |
| `year_last` | The last year to consider in the analysis. EMEP data is available until 2021, whereas EDGAR ends in 2018. | `2018:2021`; **`2021`*** |
| `treat_countries` | Sets the pool of treated countries. `"sdid_countries"` chooses the countries that are used for the SDID analysis (see SI Appendix). | **`"eu25_countries"`*** `"sdid_countries"` |
| `donor_countries` | Countries to include in the donor pool. `"same_as_treat"` chooses the same countries as are specified in `treat_countries`. | **`"same_as_treat"`*** |
| `include_aviation` | Determines whether to include aviation data. The sector has only been regulated since 2012. In our main specification, the sector contributes to the control units (as in Bayer & Aklin, 2020 to the best of our knowledge). In a robustness check, we exclude it from the analysis. | **`"control"`*** `"exclude"` |
| `leave_one_out` | Set to `TRUE` if one conducts a leave-one-out test. | `TRUE` **`FALSE`*** |
| `country_to_leave_out` | Only relevant when `leave_one_out == TRUE`. The country to leave out in the analysis. Provide a vector of country names. | (see Description) |
</details>

<details>
<summary> Click here for the arguments directly entering `gsynth()` </summary>

| Argument |  Description | Values |
| --- |  --- | --- |
| `estimator` | The estimator to use for the GSCM. Entering `gsynth()` through its `estimator` argument. | **`"ife"`*** `"mc"` |
| `covariates` | The covariates to include in the IFE model (see SI Appendix). `"standard"` includes log(GDP) and log(GDP)$^2$ (as in Bayer & Aklin, 2020). `"none"` lets the IFE model run without any covariates. The other four options add one more variable to the `"standard"` version, respectively, namely population, GDP per capita, renewable electricity capacity, and a dummy for carbon pricing schemes (see SI Appendix). Entering `gsynth()` through its `formula` argument. | **`"standard"`*** `"population"` `"gdp_pc"` `"renew_elec"` `"carbon_pricing_dummy"` `"none"` |
| `inference_type` | The type of inference to use. `"choose"` selects `"parametric"` when the number of treated units is too small ($N_{Tr} \leq 40$) as recommended [in the gsynth tutorial](https://yiqingxu.org/packages/gsynth/articles/tutorial.html) and `"nonparametric"` otherwise. Entering `gsynth()` through its `inference` argument. | `"parametric"` `"nonparametric"` **`"choose"`*** |
</details>

<details>
<summary> Click here for miscellaneous arguments that do not affect the analysis </summary>

| Argument |  Description | Values |
| --- |  --- | --- |
| `prep_data` | Determines whether the data for the analysis shall be prepared from scratch or read from a csv. Setting it to `TRUE` requires downloading two large data sets. First, download [EMEP21](https://sdi.eea.europa.eu/catalogue/srv/api/records/22accc6a-dfbd-4ed0-9c38-d0b51f86a81a) and save it under [./data/pollution/emep/CLRTAP_NVFR14_V21_GF.csv](./data/pollution/emep/CLRTAP_NVFR14_V21_GF.csv). Second, download [EMEP23](https://sdi.eea.europa.eu/catalogue/srv/api/records/2999364f-be52-4012-b4fd-f98e2cc8fab6?language=all) and save it under [./data/pollution/emep/CLRTAP_NVFR19_V23_1_GF_csv.csv](./data/pollution/emep/CLRTAP_NVFR19_V23_1_GF_csv.csv). | `TRUE` **`FALSE`*** |
| `write_files` | Only relevant when `prep_data == TRUE`. Determines whether the prepared data sets shall be saved as .csv. | `TRUE` **`FALSE`*** |
| `conduct_analysis` | Determines whether to conduct the GSCM analysis. If `FALSE`, `execute_analysis()` will only prepare the data. | **`TRUE`*** `FALSE` |
| `save_plots` | Determines whether to save plots under [./plots/](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots). | **`TRUE`*** `FALSE` |
| `write_results_table` | Determines whether to write the results tables under [./results](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/results). See [here](#accessing-results) for more information. | **`TRUE`*** `FALSE` |
</details>

---

## Accessing plots

There are two kinds of plots in this repository: (1) polished plots in the design used in the Brief Report that are saved under [./plots/paper](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots/paper) and (2) rough plots saved after each run of `execute_analysis()` in different sub-folders of [./plots](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots).

### Plots of the Brief Report and SI Appendix

The plots displayed in the Brief Report and mentioned in the SI Appendix are in [./plots/paper/](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots/paper) and [./plots/robustness_checks/](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots/robustness_checks), respectively. Please refer to the Brief Report and SI Appendix for more details. They can be replicated by [./src/R/plots_paper.r](./src/R/plots_paper.r).

### Plots created after each run

After each run, `execute_analysis()` creates multiple plots and saves them. The plots can be called through the following commands after each run: `plots$panelview` for a plot that shows the completeness of the panel data, `plots$att` (`plots$ct_tr`) for a plot similar to the bottom (top) panel of Figure 1 in the Brief Report (commands are also in [./master.r](https://github.com/ccs282/EU_ETS_Co_Benefits/blob/main/master.r)).

These plots are saved under [./plots](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots) in folders named after the three pollutants and [./plots/misc](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots/misc) if `save_plots == TRUE`. The path to the plots is based on specification choices. For instance, the main specification plot for SO$_2$ would be located under [./plots/so2/trse-ets_trco-eu25/dose-all_doco-eu25/em23/cov-std/1a2em2123_bio/1990_2005_2021_p_att.png](https://github.com/ccs282/EU_ETS_Co_Benefits/blob/main/plots/so2/trse-ets_trco-eu25/dose-all_doco-eu25/em23/cov-std/1a2em2123_bio/1990_2005_2021_p_att.png). When the specification choice cannot be expressed as a file path, they are saved under [./plots/misc](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/plots/misc) in a folder named after a unique identifier for each run.

To make locating plots convenient, users can call `misc_parameters$uuid` after each run to obtain the unique identifier. Searching the repository for this identifier will lead to the location under which the plots were saved. In the main results table ([see below](#accessing-results)), the unique identifier is also listed.

---

## Accessing results

The results of each run are saved in the list `gscm_analysis`. Please see the `gsynth()` documentation (`help("gsynth")`) for more information on the structure of the list.

Moreover, the results are saved in a .csv under [./results/results_table.csv](./results/results_table.csv), which is also available in .xlsx format under [./results/2023_11_30_results_table.xlsx](./results/2023_11_30_results_table.xlsx). The columns of the table closely mirror the function arguments of `execute_analysis()` (see [here](#function-arguments-of-execute_analysis)), output of the `gsynth()` function as well as manual calculations of the absolute effect in physical units. For details, users are encouraged to search the code under [./src/R/results_table.r](./src/R/results_table.r) for the respective column name.

The tables contain the results of all our runs, including the unique identifier of each run ([see above](#plots-created-after-each-run)). Filtering the table columns (e.g., via Excel in [./results/2023_11_30_results_table.xlsx](./results/2023_11_30_results_table.xlsx)) allows user to find the results of a specific run (e.g., for a particular combination of covariates). Moreover, the unique identifier (column `spec_id`) is the same as the one printed by `misc_parameters$uuid` after running `execute_analysis()`. Therefore, users can search [./results/results_table.csv](./results/results_table.csv) after a model run for the respective unique identifier and find the detailed results of that run.

---

## Miscellaneous

### Replication information

More information on the technical details of how the analysis was conducted can be found at the top of [./master.r](./master.r).

Please note that the repository is self-contained, i.e., contains all materials necessary to replicate the findings. The only exception to this is when setting `data_prep == TRUE` (see [here](#function-arguments-of-execute_analysis)), which manually constructs the data set again. This is not necessary as the prepared data is saved under [./data/final_data/](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/data/final_data) already.

### Synthetic Difference-in-Difference Analysis

As mentioned above, the synthetic difference-in-difference analysis (SDID) was conducted in Stata. The material to replicate it can be found under [./Stata_SDID/](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/Stata_SDID). Please refer to [./Stata_SDID/README.md](./Stata_SDID/README.md) for further details.

### Contact information

For questions on the R code, please contact [Jonas Grunau](mailto:jonas.sebastian.grunau@uni-hamburg.de). For questions on the Stata code for the [SDID analysis](https://github.com/ccs282/EU_ETS_Co_Benefits/tree/main/Stata_SDID), please contact [Pier Basaglia](mailto:piero.basaglia@uni-hamburg.de).

### Copyright

To see the license of our repository, please see [./LICENSE](./LICENSE).
