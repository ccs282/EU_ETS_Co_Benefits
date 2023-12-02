# Create vectors and df that are needed in other files ------------------------

# create a vector of all sector codes -----------------------------------------
while (!all(sapply(
        c("all_sectors_codes"),
        is.element,
        names(sectors_list)
))) {
        edgar <- read_csv(here(
                "data",
                "final_data",
                "final_data_edgar_bio.csv"
        )) %>%
                distinct(sector_code) %>%
                pull()

        emep <- read_csv(here(
                "data",
                "final_data",
                "final_data_emep_23.csv"
        )) %>%
                distinct(sector_code) %>%
                pull()

        sectors_list$all_sectors_codes <- union(edgar, emep) %>%
                sort()

        rm(edgar, emep)
}



# mark country groups (EU, ETS, OECD) -----------------------------------------

# EU-10 countries
countries_list$eu10_countries <- c(
        "Cyprus",
        "Czechia",
        "Estonia",
        "Hungary",
        "Latvia",
        "Lithuania",
        "Malta",
        "Poland",
        "Slovakia",
        "Slovenia"
)

# EU-15 countries
countries_list$eu15_countries <- c(
        "Austria",
        "Belgium",
        "Denmark",
        "Finland",
        "France",
        "Germany",
        "Greece",
        "Ireland",
        "Italy",
        "Luxembourg",
        "Netherlands",
        "Portugal",
        "Spain",
        "Sweden",
        "United Kingdom"
)

# EU-25 countries
countries_list$eu25_countries <- c(
        countries_list$eu10_countries,
        countries_list$eu15_countries
)

# EU-25 minus non-complete units
countries_list$sdid_countries <- setdiff(
        countries_list$eu25_countries,
        c(
                "Estonia",
                "Hungary",
                "Latvia",
                "Lithuania",
                "Slovakia",
                "Slovenia"#,
                # "United Kingdom"
        )
)

# EU-28 countries
countries_list$eu28_countries <- c(
        countries_list$eu25_countries,
        "Bulgaria",
        "Croatia",
        "Romania"
)

countries_list$ets_countries <- c(
        countries_list$eu28_countries,
        "Iceland",
        "Liechtenstein", # does not exist in EDGAR
        "Norway"
)

countries_list$oecd_countries <- c(
        "Australia",
        "Austria",
        "Belgium",
        "Canada",
        "Chile",
        "Colombia",
        "Costa Rica",
        "Czechia",
        "Denmark",
        "Estonia",
        "Finland",
        "France",
        "Germany",
        "Greece",
        "Hungary",
        "Iceland",
        "Ireland",
        "Israel",
        "Italy",
        "Japan",
        "South Korea",
        "Latvia",
        "Lithuania",
        "Luxembourg",
        "Mexico",
        "Netherlands",
        "New Zealand",
        "Norway",
        "Poland",
        "Portugal",
        "Slovakia",
        "Slovenia",
        "Spain",
        "Sweden",
        "Switzerland",
        "Turkey",
        "United Kingdom",
        "United States"
)

countries_list$oecd_ets_countries <- union(
        countries_list$oecd_countries,
        countries_list$ets_countries
)

countries_list$oecd_eu25_countries <- union(
        countries_list$oecd_countries,
        countries_list$eu25_countries
)

country_class <- read_csv(here(
        "data",
        "socioeconomic",
        "World Bank",
        "csv_clean",
        "country_classifications.csv"
))

countries_list$hic_countries <- country_class %>%
        filter(income_group == "High income") %>%
        pull(country)

countries_list$up_mid_inc_countries <- country_class %>%
        filter(income_group == "Upper middle income") %>%
        pull(country)

countries_list$up_mid_high_inc_countries <- reduce(
        list(
                countries_list$oecd_ets_countries,
                countries_list$hic_countries,
                countries_list$up_mid_inc_countries
        ),
        union
)

all_countries <- read_csv(here(
        "data",
        "misc",
        "all_countries.csv"
))

countries_list$all_countries <- all_countries %>%
        pull(country) %>%
        sort()

countries_list$all_countries_codes <- all_countries %>%
        pull(country_code) %>%
        sort()

rm(country_class, all_countries)



# Determine ETS sectors (taken from EUSED sector mappings) --------------------
sectors_list$energy_sectors <- c(
        # Electricity and Heat Production
        "1.A.1.a",
        "1A1a",
        # Petroleum Refining - Manufacture of Solid Fuels
        "1.A.1.bc",
        "1A1b",
        "1A1c",
        "1.A.1.b",
        "1.A.1.c"
)

sectors_list$metals_sectors <- c(
        # Manufacturing industries and construction – iron and steel
        "1.A.2.a",
        # Manufacturing industries and construction – non-ferrous metals (1.A.2.b) # nolint
        "1.A.2.b",
        # Metal Industry
        "2.C",
        "2C1",
        "2C2",
        "2C3",
        "2C4",
        "2C5",
        "2C6",
        "2C7a",
        "2C7b",
        "2C7c",
        "2C7d",
        "2.C.1",
        "2.C.2",
        "2.C.3",
        "2.C.4",
        "2.C.5",
        "2.C.6",
        "2.C.7"
)

sectors_list$minerals_sectors <- c(
        # Manufacturing industries and construction- Non-metallic minerals
        "1.A.2.f",
        # cement production
        "2.A.1",
        "2A1",
        # Lime Industry
        "2.A.2",
        "2A2",
        # Glass Industry
        "2.A.3",
        "2A3",
        # other uses of carbonates
        "2.A.4",
        # Quarrying and mining of minerals other than coal
        "2A5a",
        # Construction and demolition
        "2A5b",
        # Storage, handling and transport of mineral products
        "2A5c",
        # Other mineral products
        "2A6"
)

sectors_list$paper_sectors <- c(
        # Pulp, Paper and Print
        "1.A.2.d"
)

sectors_list$chemicals_sectors <- c(
        # Manufacturing industries and construction – chemicals
        "1.A.2.c",
        # Chemical Industry
        "2.B",
        "2.B.1",
        "2B1",
        "2B2",
        "2.B.3",
        "2B3",
        "2.B.4",
        "2.B.5",
        "2B5",
        "2.B.6",
        "2B6",
        "2.B.7",
        "2B7",
        "2.B.8",
        "2.B.10",
        "2B10a",
        "2B10b"
)

# all ETS sectors
sectors_list$ets_sectors <- sort(c(
        sectors_list$energy_sectors,
        sectors_list$metals_sectors,
        sectors_list$minerals_sectors,
        sectors_list$paper_sectors,
        sectors_list$chemicals_sectors
))

# sectors that do not allow disentangling ETS and non-ETS emissions in EDGAR
sectors_list$mixed_ets_sectors <- c(
        # manufacturing industries and construction
        "1.A.2",
        # civil aviation
        "1.A.3.a",
        "1A3ai(i)",
        "1A3ai(ii)",
        "1A3aii(i)",
        "1A3aii(ii)"
)

# If include_aviation == "control", it will not be removed but added to control units # nolint
# Mixed ETS sectors get removed from data set later on
if (specification_choices$include_aviation %in% c("control")) {
        sectors_list$mixed_ets_sectors <- setdiff(
                sectors_list$mixed_ets_sectors,
                c(
                        "1.A.3.a",
                        "1A3ai(i)",
                        "1A3ai(ii)",
                        "1A3aii(i)",
                        "1A3aii(ii)"
                )
        )
}

# add sector 1.A.2 to ets sectors (no specific sector); consider it fully treated # nolint
# relevant as there is no sub-1.A.2 level data in EDGAR
if (specification_choices$include_1a2 %in% c("edgar_fully_treated")) {

        # remove 1.A.2 from mixed_ets_sectors
        sectors_list$mixed_ets_sectors <- setdiff(
                sectors_list$mixed_ets_sectors,
                "1.A.2"
        )

        # add 1.A.2 to ets_sectors (treated)
        sectors_list$ets_sectors <- union(
                sectors_list$ets_sectors,
                "1.A.2"
        )
} else if (specification_choices$include_1a2 %in% c("edgar_fully_control")) {

        # remove 1.A.2 from mixed_ets_sectors; consider it as control
        sectors_list$mixed_ets_sectors <- setdiff(
                sectors_list$mixed_ets_sectors,
                "1.A.2"
        )

}

# non-ETS sectors
sectors_list$non_ets_sectors <- sectors_list$all_sectors_codes %>%
        setdiff(sectors_list$ets_sectors) %>%
        setdiff(sectors_list$mixed_ets_sectors)

# Sector codes to be excluded from UN data (parent sectors)
sectors_list$exclude_un <- c(
        "1",
        "1.A.1",
        "1.A.2",
        "1.A.3",
        "1.A.3.b",
        "1.A.4",
        "1.A.5",
        "1.AA",
        "1.B",
        "1.D.1",
        "2",
        "2.A",
        "2.B",
        "2.C",
        "2.D",
        "3",
        "4",
        "4.A",
        "4.B",
        "4.C",
        "4.D",
        "4.E",
        "4.F",
        "5",
        "5.A",
        "5.C"
)



# Estimates social cost of pollution ------------------------------------------
hicp <- read_csv(here(
        "data",
        "socioeconomic",
        "Eurostat",
        "prc_hicp_midx__custom_8402993_linear.csv.gz"
)) %>%
        separate_wider_delim(
                cols = TIME_PERIOD,
                delim = "-",
                names = c("year", "month")
        ) %>%
        mutate(
                year = as.numeric(year),
                month = as.numeric(month)
        ) %>%
        arrange(year, month) %>%
        filter(
                geo == "EU27_2020",
                !is.na(OBS_VALUE)
        )

misc_parameters$inflation_adjust <- list()

misc_parameters$inflation_adjust <- list(
        "most_recent" = hicp %>%
                slice_tail(n = 12) %>%
                summarise(mean = mean(OBS_VALUE)) %>%
                pull(),
        "2010" = hicp %>%
                filter(year == 2010) %>%
                summarise(mean = mean(OBS_VALUE)) %>%
                pull(),
        "2020" = hicp %>%
                filter(year == 2020) %>%
                summarise(mean = mean(OBS_VALUE)) %>%
                pull()
)

misc_parameters$inflation_adjust <- list(
        misc_parameters$inflation_adjust,
        "2010_to_most_recent" =
                misc_parameters$inflation_adjust[["most_recent"]] /
                misc_parameters$inflation_adjust[["2010"]],
        "2020_to_most_recent" =
                misc_parameters$inflation_adjust[["most_recent"]] /
                misc_parameters$inflation_adjust[["2020"]]
)

rm(hicp)

# uba_germany_3_1 in 2020 EUR; uba_eu_27 in 2010 EUR
# both converted using the average CPI of the last 12 months
social_cost_est$so2 <- list(
        uba_germany_3_1 = 15800 *
                misc_parameters$inflation_adjust[["2020_to_most_recent"]],
        uba_eu_27 = 10100 *
                misc_parameters$inflation_adjust[["2010_to_most_recent"]]
)
social_cost_est$co2 <- list(
        uba_germany_3_1 = 195 *
                misc_parameters$inflation_adjust[["2020_to_most_recent"]]
)
social_cost_est$pm25 <- list(
        uba_germany_3_1 = 61500 *
                misc_parameters$inflation_adjust[["2020_to_most_recent"]],
        uba_eu_27 = 40600 *
                misc_parameters$inflation_adjust[["2010_to_most_recent"]]
)
social_cost_est$nox <- list(
        uba_germany_3_1 = 19000 *
                misc_parameters$inflation_adjust[["2020_to_most_recent"]],
        uba_eu_27 = 10300 *
                misc_parameters$inflation_adjust[["2010_to_most_recent"]]
)

misc_parameters$scp <- social_cost_est[[
        specification_choices$pollutant
]][[
        specification_choices$damage_est_source
]]



# Argument validation vectors -------------------------------------------------
misc_parameters$arg_validation <- list()

misc_parameters$arg_validation <- modifyList(
        misc_parameters$arg_validation,
        list(
                pollutant = c(
                        "so2",
                        "pm25",
                        "nox"
                ),
                prep_data = c(
                        "TRUE",
                        "FALSE"
                ),
                write_files = c(
                        "TRUE",
                        "FALSE"
                ),
                include_1a2 = c(
                        "no",
                        "edgar_fully_treated",
                        "edgar_fully_control",
                        # "emep21/un",
                        # "emep23/un",
                        "emep21+23/un"
                ),
                include_bio = c(
                        "yes",
                        "no"
                ),
                include_uk = c(
                        "TRUE",
                        "FALSE"
                ),
                include_aviation = c(
                        "control",
                        "exclude"
                ),
                ensure_common = c(
                        "TRUE",
                        "FALSE"
                ),
                main_data = c(
                        "edgar",
                        # "emep21/un",
                        "emep23/un"
                ),
                per_capita_emissions = c(
                        "TRUE",
                        "FALSE"
                ),
                per_capita_gdp = c(
                        "TRUE",
                        "FALSE"
                ),
                gdp = c(
                        "constant",
                        "current"
                ),
                year_first = c(
                        1970:1995
                ),
                year_last = c(
                        2018:2021
                ),
                balanced_panel = c(
                        "TRUE",
                        "FALSE"
                ),
                gaps_in_years = c(
                        "interpolate",
                        "drop"
                ),
                damage_est_source = c(
                        "uba_germany_3_1",
                        "uba_eu_27"
                ),
                time_var_damages = c(
                        #"TRUE",
                        "FALSE"
                ),
                ets_start_year = c(
                        2002:2008
                ),
                unit_of_analysis = c(
                        "country_treat"
                ),
                treatment_timing = c(
                        "staggered",
                        "common"
                ),
                treat_countries = c(
                        "ets_countries",
                        "eu28_countries",
                        "eu10_countries",
                        "eu15_countries",
                        "eu25_countries",
                        "sdid_countries",
                        countries_list$ets_countries
                ),
                treat_sectors = c(
                        "ets_sectors",
                        "energy_sectors",
                        "metals_sectors",
                        "minerals_sectors",
                        "chemicals_sectors",
                        "paper_sectors",
                        sectors_list$ets_sectors
                ),
                donor_countries = c(
                        "eu28_countries",
                        "eu10_countries",
                        "eu15_countries",
                        "eu25_countries",
                        "sdid_countries",
                        "ets_countries",
                        # "oecd_countries",
                        # "oecd_ets_countries",
                        # "oecd_eu25_countries",
                        # "hic_countries",
                        # "up_mid_inc_countries",
                        # "up_mid_high_inc_countries",
                        # "all_countries",
                        "same_as_treat",
                        countries_list$all_countries
                ),
                donor_sectors = c(
                        "all_available",
                        # "hybrid",
                        # "non_ETS_sectors",
                        # "ETS_sectors",
                        # "same_as_treat",
                        sectors_list$all_sectors_codes
                ),
                leave_one_out = c(
                        "TRUE",
                        "FALSE"
                ),
                country_to_leave_out = c(
                        "NA",
                        countries_list$all_countries
                ),
                covariates = c(
                        "population",
                        "gdp_pc",
                        "renew_elec",
                        "carbon_pricing_dummy",
                        # "elv",
                        "standard",
                        "none"
                ),
                min_t0 = c(
                        7:20
                ),
                criterion = c(
                        "mspe",
                        "pc"
                ),
                alpha = c(0.05, 0.5),
                estimator = c(
                        "ife",
                        "mc"
                ),
                em = c(
                        "TRUE",
                        "FALSE"
                ),
                cv = c(
                        "TRUE",
                        "FALSE"
                ),
                r = c(
                        0:10
                ),
                inference_type = c(
                        "parametric",
                        "nonparametric",
                        "choose"
                ),
                annotate_plots = c(
                        "att",
                        # "detailed",
                        "no"
                ),
                save_plots = c(
                        "TRUE",
                        "FALSE"
                ),
                show_lines = c(
                        "ct",
                        "co",
                        "tr"
                ),
                loop_mode = c(
                        # "TRUE",
                        "FALSE"
                ),
                conduct_analysis = c(
                        "TRUE",
                        "FALSE"
                ),
                write_results_table = c(
                        "TRUE",
                        "FALSE"
                )
        )
)