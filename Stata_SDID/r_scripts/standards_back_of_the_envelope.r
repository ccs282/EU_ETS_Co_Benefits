# This file conducts the back-of-the-envelope calculations described in the SI Appendix that are plotted in Figure 2 of the Brief Report. # nolint

# Load packages ---------------------------------------------------------------
# refer to att_y.r file
library(tidyverse)
library(magrittr)
library(glue)
library(here)

here()
source(here("src", "R", "functions.r"))


# Read E-PRTR data ------------------------------------------------------------
eea_data <- read_csv(
        here(
                "Stata_SDID",
                "data_in",
                "F1_4_Detailed releases at facility level with E-PRTR Sector and Annex I Activity detail into Air.csv" # nolint
        ),
        col_types = list(releasesConfidentialityReason = col_character())
) %>%
        rename_with(
                .fn = str_to_lower,
                .cols = everything()
        ) %>%
        filter(
                # Extract emissions from combustion plants which are jointly subject to the EU ETS and standards # nolint
                eprtranneximainactivitylabel %in% c(
                        "Thermal power stations and other combustion installations" # nolint
                ),
                # Focus on the pollutants covered in our analyses
                pollutant %in% c(
                        "Nitrogen oxides (NOX)",
                        "Sulphur oxides (SOX)",
                        "Particulate matter (PM10)"
                ),
                # Focus on EU-25 to use a consistent sample as in our empirical analyses # nolint
                !countryname %in% c(
                        "Bulgaria",
                        "Croatia",
                        "Romania",
                        "Serbia",
                        "Switzerland",
                        "Norway"
                ),
                # Focus on the binding period of the LCPD and IED 2008-2021 in our estimation sample # nolint
                !reportingyear %in% c(2007, 2022),
                !(countryname == "United Kingdom" & reportingyear >= 2020)
        ) %>%
        # As we do not observe PM2.5 but only PM10, we follow the assumption in the UBA report (UBA, 2012) that 70% of PM10 is PM2.5. # nolint
        mutate(
                emissions = case_when(
                        pollutant == "Particulate matter (PM10)" ~
                                emissions * 0.70,
                        .default = emissions
                )
        )

# Collapse data by pollutant and reporting year
eea_data <- eea_data %>%
        reframe(
                pollutant = pollutant,
                reportingyear = reportingyear,
                emissions = sum(emissions),
                .by = c("pollutant", "reportingyear")
        ) %>%
        distinct() %>%
        mutate(
                pollutant = case_when(
                        pollutant == "Nitrogen oxides (NOX)" ~
                                "nox",
                        pollutant == "Sulphur oxides (SOX)" ~
                                "so2",
                        pollutant == "Particulate matter (PM10)" ~
                                "pm25",
                        .default = NA
                )
        )

# Import estimates from our main estimations (see Brief Report); data set created in src/R/att_y.r # nolint
att_y <- read_csv(
        here(
                "Stata_SDID",
                "data_in",
                "att_y.csv"
        )
)

# Note: The variables att_avg_y and eff_mon represent the estimated treatment effects on the treated (ATT) and the estimated co-benefits from the Brief Report, respectively. # nolint

eea_data <- left_join(
        eea_data,
        att_y,
        by = c("pollutant", "reportingyear" = "year_y")
) %>%
        distinct() %>%
        # reverse percentage transformation
        mutate(
                reduction = -att_avg_y / 100,
                .keep = "unused"
        ) %>%
        pivot_wider(
                names_from = specification,
                values_from = c(reduction, eff_mon)
        ) %>%
        arrange(pollutant, reportingyear)

# We assume their emissions in each post-treatment period (i..e, from 2008) would have been higher by a percentage equivalent to the ATTs presented in the Report (cf. SI Appendix). # nolint

eea_data <- eea_data %>%
        # Emissions are in kilograms. Transform from relative reduction to tonnes and leverage the ATTs from the Brief Report # nolint
        mutate(across(
                .cols = contains("reduction"),
                .fns = ~ ((emissions / (1 - .x)) - emissions) / 1e3
        ))

eea_collapsed <- eea_data %>%
        # Get cumulative emissions by pollutant over the regulated period
        reframe(
                pollutant = pollutant,
                scp = scp,
                across(
                        .cols = starts_with("eff_"),
                        .fns = ~.x
                ),
                across(
                        .cols = starts_with("reduction"),
                        .fns = ~sum(., na.rm = TRUE)
                ),
                spec_id = spec_id,
                .by = c("pollutant")
        ) %>%
        distinct() %>%
        mutate(
                across(
                        .cols = starts_with("reduction"),
                        .fns = ~ (.x * scp) / 1e9,
                        .names = "mon_{.col}"
                ),
                .keep = "unused"
        ) %>%
        pivot_longer(
                # cols = -c(pollutant),
                cols = -c(pollutant, spec_id),
                names_to = c(".value", "specification"),
                names_pattern = "(eff_mon_|mon_reduction_)(.*)"
        ) %>%
        filter(
                !is.na(eff_mon_)
        ) %>%
        mutate(
                residual_health = eff_mon_ - mon_reduction_
        )

write_csv(
        eea_collapsed,
        here(
                "Stata_SDID",
                "data_out",
                "standards_back_of_the_envelope.csv"
        )
)

total <- eea_collapsed %>%
        summarise(
                across(
                        .cols = c(contains("mon_"), "residual_health"),
                        .fns = sum
                ),
                .by = c("specification")
        )

total

# Note: "eff_mon_" represents the estimated co-benefits as described in the Brief Report. "mon_reduction_" represents the estimated reductions in combustion facilities based on E-PRTR data that we subtract in our back-of-the-envelope calculations (see SI Appendix). Residual health is the difference between the two. Values in billions of €. # nolint
