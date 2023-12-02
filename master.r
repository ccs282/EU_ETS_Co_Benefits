# FILE INFORMATION ------------------------------------------------------------

# Project:      The European Union Emissions Trading System yields large co-benefits from pollution reduction # nolint
# Participants: Pier Basaglia, Jonas Grunau, Moritz Drupp

# Purpose:      Preparing the data for and running the GSCM analysis, creating plots and saving results through the custom function execute_analysis(). # nolint

# Data input:   Multiple files saved under './data/...'
# Output files: Plots saved under './plots/...' and results under './results/...' # nolint
# Last update:  December 2023

# TECHNICAL DISCLAIMER
# Analysis run using R version 4.3.2 in VSC 1.84.2 on Windows 11, 64 bit, Intel(R) Core(TM) i7-1185G7 @ 3.00GHz 3.00 GHz with 16GB RAM # nolint

# License: Please see the LICENSE file in the root directory of the repository.

# Contact:
# Email: jonas.sebastian.grunau@uni-hamburg.de

# Note: The synthetic DID analysis is in a separate folder './Stata_SDID/...' # nolint



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

# Main specification without covariates
# Fig.2, Col. 1 in the Brief Report
execute_analysis(
        pollutant = "so2", # "nox", "so2", "pm25"
        covariates = "none"
)

# Main specification
# Fig. 1 & Col. 2 of Fig. 2 in the Brief Report
execute_analysis(
        pollutant = "so2" # "nox", "so2", "pm25"
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



# GSCM with custom arguments -------------------------------------------------

# description of all arguments under './README.md'
execute_analysis(

        # specification choices # nolint

        pollutant = "so2",
        # main_data = "emep23/un",
        # ets_start_year = 2005,
        # year_first = 1990,
        # year_last = 2021,
        # treat_countries = "eu25_countries",
        # treat_sectors = "ets_sectors",
        # donor_countries = "same_as_treat",
        # donor_sectors = "all_available",
        # leave_one_out = FALSE,
        # country_to_leave_out = "Germany",
        # include_1a2 = "emep21+23/un",
        # include_bio = "yes",
        # include_uk = TRUE,
        # include_aviation = "control",
        # balanced_panel = FALSE,
        # gaps_in_years = "interpolate",
        # treatment_timing = "common",
        # ensure_common = TRUE,
        # damage_est_source = "uba_eu_27",
        # per_capita_emissions = FALSE,
        # per_capita_gdp = FALSE,
        # gdp = "constant",

        # # direct gsynth() input

        # estimator = "ife",
        # covariates = "standard",
        # cv = TRUE,
        # r = 0,
        # criterion = "mspe",
        # inference_type = "choose",

        # # misc. arguments

        # prep_data = TRUE,
        # write_files = TRUE,
        # save_plots = TRUE,
        # annotate_plots = "att",
        # show_lines = c("tr", "ct"),
        # conduct_analysis = TRUE,
        # write_results_table = TRUE
)