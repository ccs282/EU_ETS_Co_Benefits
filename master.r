# FILE INFORMATION ------------------------------------------------------------

# Project:      The European Union Emissions Trading System yields large co-benefits from pollution reduction # nolint
# Participants: Pier Basaglia, Jonas Grunau, Moritz Drupp

# Purpose:      Preparing the data for and running the GSCM analysis, creating plots and saving results through the custom function execute_analysis(). # nolint

# Data input:   Multiple files saved under './data/'
# Output files: Plots saved under './plots/' and results under './results/' # nolint
# Last update:  March 2024

# TECHNICAL DISCLAIMER
# Analysis run using R version 4.3.2 in Visual Studio Code 1.84.2 on Windows 11, 64 bit, Intel(R) Core(TM) i7-1185G7 @ 3.00GHz 3.00 GHz with 16GB RAM # nolint

# License: Please see the LICENSE file in the root directory of the repository.

# Contact:
# Email: jonas.sebastian.grunau@uni-hamburg.de

# Note: The synthetic DID analysis is in a separate folder './Stata_SDID/' # nolint



# Load packages ---------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(glue)
library(gsynth)
library(panelView)
library(countrycode)
library(here)
library(uuid)



# Set reference point for relative paths --------------------------------------
here()



# Get the necessary functions -------------------------------------------------
source(here("src", "R", "functions.r"))



# Results main paper ----------------------------------------------------------

# Main specification
# Fig. 1 & Col. 2 of Fig. 2 in the Brief Report
execute_analysis(
        pollutant = "so2" # "nox", "so2", "pm25"
)

# Main specification without covariates
# Fig.2, Col. 1 in the Brief Report
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        covariates = "none"
)

# Main specification with concurrent policy covariates
# Fig.2, Col. 3 in the Brief Report
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        covariates = c(
                "renew_elec",
                "carbon_pricing_dummy"
        )
)

# Main specification with matrix completion estimator
# Fig.2, Col. 4 in the Brief Report
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        estimator = "mc"
)



# Show different plots and uuid -----------------------------------------------

plots$panelview # panelview plot; inspect the (in)completeness of the data

plots$att # main results figure (one line)
plots$ct_tr # counterfactual vs. treated figure (two lines)

misc_parameters$uuid # identifier to find plots easily (if save_plots == TRUE)



# Results Supplementary Information -------------------------------------------

# Additional covariate: carbon pricing dummy
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        covariates = "carbon_pricing_dummy"
)

# Additional covariate: renewable electricity capacity
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        covariates = "renew_elec"
)

# Additional covariate: population
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        covariates = "population"
)

# Additional covariate: GDP per capita
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        covariates = "gdp_pc"
)

# Additional covariate: Retired coal capacity
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        covariates = "lcp_90_05_na"
)

# Specification focusing only on power sector reductions
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        treat_sectors = c(
                "1.A.1.a",
                "1A1a"
        )
)

# EMEP data; analysis ending in 2018 (to make it comparable to EDGAR estimate)
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        year_last = 2018
)

# EDGAR data; analysis ending in 2018
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        year_last = 2018,
        main_data = "edgar"
)

# EMEP data; ETS starting in 2008 (as in Bayer & Aklin, 2020)
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        ets_start_year = 2008
)

# Placebo test: ETS starting in 2002
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        ets_start_year = 2002
)

# GSCM with SDID sample (dropping countries and changing final year)
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        treat_countries = "sdid_countries",
        year_last = 2019
)

# Aviation emissions excluded from analysis (otherwise they contribute to control units) # nolint
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        include_aviation = "exclude"
)

# Leave-one-out analysis
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        leave_one_out = TRUE,
        country_to_leave_out = "Germany" # change to EU-25 country of your choice # nolint
)

# Power sector analysis
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        treat_sectors = c(
                "1.A.1.a",
                "1A1a"
        )
)

# Analysis ending in 2019 (to address COVID-19 concerns)
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        year_last = 2019
)

# Analysis ending in 2016 (to make it more comparable to Bayer & Aklin, 2020)
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        year_last = 2016
)

# Add fuel prices as covariates
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        covariates = c(
                "coal_price_weighted",
                "oil_price_weighted",
                "gas_price_weighted"
        )
)



# How to obtain the data for SDID and paper plots -----------------------------
# creating data input for './Stata_SDID/dofiles/EUETS_SDID_SCM_DiD.do' and
# './src/R/plots_paper.r'

execute_analysis(
        pollutant = "so2" # "nox", "so2", "pm25"
)

# Data needed for SDID analysis ('./Stata_SDID/')
write_csv(
        gscm,
        here(
                "Stata_SDID",
                "data_in",
                glue("{specification_choices$pollutant}_gscm_data.csv")
        )
)

# Data needed to create the plots in the Brief Report
write_csv(plots$data, here(
        "plots",
        "data",
        glue("{specification_choices$pollutant}.csv")
))



# GSCM with custom arguments --------------------------------------------------

# description of all arguments under './README.md'
execute_analysis(

        # specification choices # nolint

        pollutant = "so2",
        # main_data = "emep23/un",
        # ets_start_year = 2005,
        # year_last = 2021,
        # covariates = c(
        #         "carbon_pricing_dummy"
        # ),
        # treat_countries = "sdid_countries",
        # treat_sectors = c(
        #         "1.A.1.a",
        #         "1A1a"
        # ),
        # donor_countries = "same_as_treat",
        # leave_one_out = FALSE,
        # country_to_leave_out = "Germany",
        # include_aviation = "control",
        # damage_est_source = "uk_2023",

        # # direct gsynth() input

        # estimator = "ife",
        # covariates = "standard",
        # inference_type = "choose",

        # # misc. arguments

        # prep_data = TRUE,
        # write_files = TRUE,
        # save_plots = FALSE,
        # conduct_analysis = FALSE,
        # write_results_table = FALSE
)