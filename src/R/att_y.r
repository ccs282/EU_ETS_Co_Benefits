# This file loads the yearly estimated ATT values that are needed for the back-of-the-envelope calculation # nolint



# Load packages ---------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(glue)
library(here)

here()
source(here("src", "R", "functions.r"))


att_y <- read_csv(here(
        "results",
        "results_table_y.csv"
)) %>%
        filter(
                year_last == 2021,
                main_data == "emep23/un",
                include_aviation == "control",
                ets_start_year == 2005,
                treat_countries %in% c("eu25_countries"),
                covariates %in% c(
                        "log_gdp + log_gdp_2",
                        "carbon_pricing_dummy + log_gdp + log_gdp_2 + log_renew_elec", # nolint
                        "none"
                ),
                inference %in% c("parametric"),
                estimator %in% c("ife"),
                damage_est_source %in% "uba_eu_27",
                treat_sectors %in% "ets_sectors",
                leave_one_out == FALSE,
                ntr %in% c(25)
        ) %>%
        mutate(
                specification = case_when(
                        covariates %in% c("log_gdp + log_gdp_2") ~
                                "main",
                        covariates %in% c("carbon_pricing_dummy + log_gdp + log_gdp_2 + log_renew_elec") ~ # nolint
                                "concurrent_policies",
                        covariates %in% c("none") ~
                                "no_covs",
                        .default = NA
                ),
                .keep = "unused"
        ) %>%
        select(
                pollutant,
                specification,
                scp,
                eff_mon,
                year_y,
                att_avg_y,
                spec_id
        )

write_csv(
        att_y,
        here(
                "Stata_SDID",
                "data_in",
                "att_y.csv"
        )
)