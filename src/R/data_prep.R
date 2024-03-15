# Load and clean EDGAR data ---------------------------------------------------
edgar <- map(
        misc_parameters$pollutants,
        ~ functions$clean_edgar_data(
                pollutant = .x
        )
) %>%
        reduce(full_join) %>%
        # remove former countries and international sectors
        filter(!(country_code %in% c("AIR", "ANT", "SCG", "SEA"))) %>%
        mutate(
                pollutant = case_when(
                        pollutant == "SO2" ~ "so2",
                        pollutant == "NOx" ~ "nox",
                        pollutant == "PM2.5" ~ "pm25",
                        pollutant == "CO2" ~ "co2",
                        .default = NA
                ),
                country = countrycode(
                        sourcevar = country_code,
                        origin = "iso3c",
                        destination = "country.name"
                ),
                data_source = "EDGAR"
        ) %>%
        arrange(pollutant, country, year, sector_code, fossil_bio)



# Deal with bio vs. fossil emissions ------------------------------------------
edgar %<>%
        pivot_wider(
                names_from = "fossil_bio",
                names_prefix = "emissions_",
                values_from = "emissions"
        ) %>%
        filter(!if_all(
                .cols = c("emissions_fossil", "emissions_bio"),
                .fns = ~ is.na(.)
        )) %>%
        mutate(emissions_total = rowSums(
                pick(emissions_fossil, emissions_bio),
                na.rm = TRUE
        )) %>%
        pivot_longer(
                cols = starts_with("emissions_"),
                names_to = "emissions_type",
                names_prefix = "emissions_",
                values_to = "emissions"
        )

edgar %<>%
        select(-any_of(c(
                "country_code"
        )))



# Reshape into wide format to make merging with other data easier -------------
edgar_wide <- edgar %>%
        pivot_wider(
                names_from = c(
                        pollutant,
                        sector_code,
                        data_source,
                        emissions_type
                ),
                names_sep = "_",
                values_from = emissions
        ) %>%
        select(
                -where(~ all(is.na(.)))
        ) %>%
        mutate(year = as.numeric(year))

rm(edgar)



# Load World Bank data --------------------------------------------------------

wb_data_names <- c(
        "population",
        "gdp_pc_constant",
        "gdp_pc_current",
        "gdp_constant",
        "gdp_current"
)

wb_file_names <- c(
        "population_SP_POP_TOTL.csv",
        "GDP_pc_constant_NY_GDP_PCAP_KD.csv",
        "GDP_pc_current_NY_GDP_PCAP_CD.csv",
        "GDP_constant_NY_GDP_MKTP_KD.csv",
        "GDP_current_NY_GDP_MKTP_CD.csv"
)

wb_file_paths <- here(
        "data",
        "socioeconomic",
        "World Bank",
        "csv_raw",
        glue("{wb_file_names}")
)

map2(wb_data_names, wb_file_paths, ~ assign(
        x = .x,
        value = read_csv(.y),
        envir = .GlobalEnv
))



# Cleaning of World Bank data -------------------------------------------------

restructure_the_world_bank <- function(data_set_wb) {

        data_output <- get(data_set_wb) %>%
                select(-c(
                        "Indicator Name",
                        "Indicator Code"
                )) %>%
                select(
                        -("1960":"1969"),
                        -("2022")
                ) %>%
                rename(
                        country = "Country Name",
                        country_code = "Country Code"
                ) %>%
                pivot_longer(
                        cols = ("1970":"2021"),
                        names_to = "year",
                        values_to = data_set_wb
                )

        assign(
                x = glue("{data_set_wb}"),
                value = data_output,
                envir = .GlobalEnv
        )
}

map(
        wb_data_names,
        ~ restructure_the_world_bank(
                data_set_wb = .x
        )
)



# Merge World Bank data with each other ---------------------------------------
socioeconomic <- map(
        glue("{wb_data_names}"),
        ~ get(.x)
) %>%
        reduce(
                full_join,
                by = c("country", "year", "country_code")
        ) %>%
        mutate(
                country = countrycode(
                        sourcevar = country_code,
                        origin = "wb",
                        destination = "country.name"
                ),
                year = as.numeric(year)
        ) %>%
        filter(!is.na(country)) %>%
        select(-country_code)

rm(list = ls(
        pattern =
                "^(gdp|population|wb|edgar_c|names|clean|restructure)"
))



# Create variables for participation in groups --------------------------------
socioeconomic %<>%
        mutate(
                eu_country = if_else(
                        country %in% countries_list$eu_countries,
                        1,
                        0
                ),
                ets_country = if_else(
                        country %in% countries_list$ets_countries,
                        1,
                        0
                ),
                oecd_country = if_else(
                        country %in% countries_list$oecd_countries,
                        1,
                        0
                ),
                .after = year
        ) %>%
        mutate(
                eu_country = case_when(
                        year < 1993 ~ 0,
                        year < 1995 &
                                (country %in% c(
                                        "Austria",
                                        "Finland",
                                        "Sweden"
                                )) ~ 0,
                        year < 2004 & (country %in% c(
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
                        )) ~ 0,
                        year < 2007 & (country %in% c(
                                "Bulgaria",
                                "Romania"
                        )) ~ 0,
                        year < 2013 & (country == "Croatia") ~ 0,
                        year < 2021 & (country == "United Kingdom") ~ 0,
                        .default = eu_country
                ),
                ets_country = case_when(
                        year < 2005 ~ 0,
                        year < 2007 & (country %in% c(
                                "Bulgaria",
                                "Romania"
                        )) ~ 0,
                        year < 2008 & (country %in% c(
                                "Liechtenstein",
                                "Norway"
                        )) ~ 0,
                        year < 2013 & (country %in% c(
                                "Croatia",
                                "Iceland"
                        )) ~ 0,
                        year > 2020 & (country == "United Kingdom") ~ 0,
                        .default = ets_country
                ),
                oecd_country = case_when(
                        year < 1971 & (country %in% c(
                                "Australia"
                        )) ~ 0,
                        year < 1973 & (country %in% c(
                                "New Zealand"
                        )) ~ 0,
                        year < 1994 & (country %in% c(
                                "Mexico"
                        )) ~ 0,
                        year < 1995 & (country %in% c(
                                "Czechia"
                        )) ~ 0,
                        year < 1996 & (country %in% c(
                                "Hungary",
                                "Poland",
                                "South Korea"
                        )) ~ 0,
                        year < 2000 & (country %in% c(
                                "Slovakia"
                        )) ~ 0,
                        year < 2010 & (country %in% c(
                                "Chile",
                                "Slovenia",
                                "Israel",
                                "Estonia"
                        )) ~ 0,
                        year < 2016 & (country %in% c(
                                "Latvia"
                        )) ~ 0,
                        year < 2018 & (country %in% c(
                                "Lithuania"
                        )) ~ 0,
                        year < 2020 & (country %in% c(
                                "Colombia"
                        )) ~ 0,
                        year < 2021 & (country %in% c(
                                "Costa Rica"
                        )) ~ 0,
                        .default = oecd_country
                )
        )



# add renewable electricity data ----------------------------------------------
renew <- read_csv(
        here(
                "data",
                "socioeconomic",
                "Eurostat",
                "renew_elec.csv"
        )
) %>%
        pivot_longer(
                cols = "1990":"2021",
                names_to = "year",
                values_to = "renew_elec" # in GWh
        ) %>%
        mutate(
                country = countrycode(
                        sourcevar = country,
                        origin = "country.name",
                        destination = "country.name"
                ),
                year = as.numeric(year)
        ) %>%
        filter(!is.na(country)) %>%
        # Change code here to decide how to deal with log(0) values
        mutate(renew_elec = renew_elec + 1e-10)

socioeconomic <- left_join(
        x = socioeconomic,
        y = renew,
        by = c("country", "year")
) %>%
        relocate(renew_elec, .before = population)

rm(renew)



# Carbon pricing dummy --------------------------------------------------------
carbon_pricing <- read_csv(here(
        "data",
        "socioeconomic",
        "World Bank",
        "csv_raw",
        "carbon_pricing.csv"
)) %>%
        rename_with(
                .fn = ~ str_replace_all(
                        str_to_lower(.),
                        " ",
                        "_"
                ),
                .cols = everything()
        ) %>%
        rename_with(
                .fn = ~ str_replace_all(
                        .,
                        "juridiction",
                        "jurisdiction"
                )
        ) %>%
        filter(
                status %in% c(
                        "Implemented",
                        "Abolished"
                ),
                type %in% c(
                        "ETS",
                        "Carbon tax"
                ),
                type_of_jurisdiction_covered %in% c(
                        "National",
                        "Regional"
                )
        ) %>%
        mutate(country = countrycode(
                sourcevar = jurisdiction_covered,
                origin = "country.name",
                destination = "country.name"
        ), .before = name_of_the_initiative) %>%
        filter(!is.na(country)) %>%
        mutate(
                year_of_abolishment = as.numeric(year_of_abolishment),
                year_of_implementation = as.numeric(year_of_implementation)
        ) %>%
        mutate(year_of_abolishment = if_else(
                year_of_abolishment == 0,
                9999,
                year_of_abolishment
        )) %>%
        select(
                country,
                type,
                year_of_implementation,
                year_of_abolishment
        ) %>%
        group_by(country) %>%
        mutate(
                implement = min(year_of_implementation),
                abolish = max(year_of_abolishment)
        ) %>%
        ungroup() %>%
        distinct(pick(country, implement, abolish))

socioeconomic <- left_join(
        socioeconomic,
        carbon_pricing,
        by = c("country")
) %>%
        mutate(carbon_pricing_dummy = if_else(
                year >= implement &
                        year <= abolish,
                1,
                0
        ), .after = renew_elec) %>%
        mutate(carbon_pricing_dummy = if_else(
                is.na(carbon_pricing_dummy),
                0,
                carbon_pricing_dummy
        )) %>%
        select(-c(implement, abolish)) %>%
        arrange(country, year)

rm(carbon_pricing)



# OECD policy stringency index ------------------------------------------------
oecd_eps <- read_csv(here(
        "data",
        "socioeconomic",
        "OECD",
        "policy_stringency.csv"
)) %>%
        rename_with(
                .fn = str_to_lower,
                .cols = everything()
        ) %>%
        mutate(country = countrycode(
                sourcevar = cou,
                origin = "iso3c",
                destination = "country.name"
        )) %>%
        select(-any_of(c(
                "cou",
                "yea",
                "powercode code",
                "powercode",
                "variable"
        ))) %>%
        select(-where(~ all(is.na(.)))) %>%
        select(country, year, everything()) %>%
        arrange(country, year) %>%
        filter(var %in% c(
                "ELV_SOX",
                "ELV_NOX",
                "ELV_PM",
                "TAXSOX",
                "TAXNOX",
                "TAXDIESEL",
                "EPS"
        )) %>%
        mutate(var = str_to_lower(var)) %>%
        mutate(var = case_when(
                var == "taxsox" ~ "tax_sox",
                var == "taxnox" ~ "tax_nox",
                var == "taxdiesel" ~ "tax_diesel",
                .default = var
        )) %>%
        pivot_wider(
                names_from = var,
                values_from = value
        )

socioeconomic <- left_join(
        socioeconomic,
        oecd_eps,
        by = c("country", "year")
)

rm(oecd_eps)



# Add data from the Global Coal Plant Tracker ---------------------------------
path_gcpt_common <- c(
        "data",
        "socioeconomic",
        "Global Energy Monitor"
)

gcpt_raw <- read_csv(file = functions$here_vec(c(
        path_gcpt_common,
        "Global-Coal-Plant-Tracker-July-2023.csv"
))) %>%
        functions$clean_plant_data(countrycode = TRUE) %>%
        mutate(
                capacity_plant = sum(capacity__mw_, na.rm = TRUE),
                .by = c(plant_name),
                .after = capacity__mw_
        )

gcpt_extended <- read_csv(file = functions$here_vec(c(
        path_gcpt_common,
        "July 2023 GCPT Status Changes - 2014 - 2023.csv"
))) %>%
        functions$clean_plant_data(countrycode = TRUE) %>%
        full_join(
                x = gcpt_raw,
                y = .,
                by = c(
                        "gem_unit_phase_id"
                ),
                suffix = c("", "_s_c"),
                relationship = "one-to-one"
        ) %>%
        select(-ends_with("_s_c")) %>%
        pivot_longer(
                cols = h2_2014:h1_2023,
                names_to = c("year_half", "year"),
                names_sep = "_",
                values_to = "status_hist"
        ) %>%
        mutate(
                year_half = case_when(
                        year_half == "h1" ~ "1",
                        year_half == "h2" ~ "2",
                        .default = year_half
                ) %>% as.numeric(),
                year = as.numeric(year)
        ) %>%
        mutate(
                time = glue("{year}.{year_half}") %>% as.numeric(),
                .after = status_hist
        )

gcpt_reduced <- gcpt_extended %>%
        filter(
                # change if the GCPT specification shall work for samples other than the one used in the main specification (EU-25) # nolint
                country %in% countries_list$eu25_countries
        ) %>%
        rename(status_current = status) %>%
        select(c(
                gem_unit_phase_id,
                country,
                plant_name,
                unit_name,
                capacity__mw_:capacity_plant,
                status_hist:time,
                year,
                year_half,
                status_current,
                start_year:planned_retirement,
                latitude:location_accuracy,
                remaining_plant_lifetime__years_,
                wiki_url
        )) %>%
        arrange(country, plant_name, unit_name, time) %>%
        mutate(
                plant_id = as.numeric(fct(plant_name)),
                unit_id = as.numeric(fct(gem_unit_phase_id)),
                .before = gem_unit_phase_id
        ) %>%
        mutate(across(
                .cols = c(
                        status_hist,
                        status_current
                ),
                .fns = ~ str_to_lower(.)
        )) %>%
        group_by(unit_id) %>%
        mutate(
                lag_status_hist = lag(status_hist),
                .after = status_hist
        ) %>%
        filter(
                case_when(
                        status_hist == lag_status_hist ~ FALSE,
                        .default = TRUE
                )
        ) %>%
        ungroup() %>%
        select(-matches("(lead_)|(lag_)"))

gcpt_retired_capacity <- gcpt_reduced %>%
        filter(
                capacity_plant >= 50
        ) %>%
        group_by(unit_id) %>%
        # fill in missing values for retired_year using the GEM Wiki and mothballed status etc. # nolint
        mutate(retired_year = case_when(
                # only came back in 2022 due to the energy crisis
                gem_unit_phase_id %in% c("G109639") ~
                        2016,
                gem_unit_phase_id %in% c("G106635") ~
                        2020,
                gem_unit_phase_id %in% c("G100064", "G100065") ~
                        2020,
                gem_unit_phase_id %in% c("G107597", "G107598") ~
                        2007,
                gem_unit_phase_id %in% c("G103105") ~
                        2021,
                # retired then, brought back later in 2022
                gem_unit_phase_id %in% c("G106424") ~
                        2020,
                # mothballed in 2019; brought back in 2022
                gem_unit_phase_id %in% c("G104971") ~
                        2019,
                # mothballed in 2018; brought back in 2022
                gem_unit_phase_id %in% c("G104972") ~
                        2018,
                # mothballed in 2021; brought back in 2022
                gem_unit_phase_id %in% c("G106802") ~
                        2021,
                # mothballed in 2019; brought back in 2022
                gem_unit_phase_id %in% c("G107370") ~
                        2019,
                # mothballed in 2018; brought back in 2022
                gem_unit_phase_id %in% c("G107426", "G107427") ~
                        2018,
                # mothballed in 2019; brought back in 2022
                gem_unit_phase_id %in% c("G106799") ~
                        2019,
                # mothballed in 2018; brought back in 2022
                gem_unit_phase_id %in% c(
                        "G107677",
                        "G107678",
                        "G107679",
                        "G107680"
                ) ~
                        2018,
                .default = retired_year
        )) %>%
        filter(
                any(
                        status_hist %in% c(
                                "operating",
                                "mothballed",
                                "retired"
                        )
                ),
                time == last(time),
                if_any(
                        .cols = c(
                                retired_year
                        ),
                        .fns = ~ !is.na(.)
                )
        ) %>%
        ungroup()

gcpt <- gcpt_retired_capacity %>%
        mutate(
                start_group = case_when(
                        start_year <= 1987 ~
                                "leq_87",
                        start_year > 1987 & start_year <= 1990 ~
                                "88_90",
                        start_year > 1990 & start_year <= 2002 ~
                                "91_02",
                        start_year > 2002 & start_year <= 2005 ~
                                "03_05",
                        start_year > 2005 ~
                                "geq_06",
                        .default = NA
                ),
                .after = c(start_year)
        ) %>%
        mutate(
                retired_capacity = sum(capacity__mw_),
                .by = c(country, retired_year, start_group),
                .after = retired_year
        ) %>%
        distinct(
                country,
                start_group,
                retired_year,
                retired_capacity
        ) %>%
        pivot_wider(
                names_from = start_group,
                values_from = retired_capacity
        ) %>%
        rowwise() %>%
        mutate(
                leq_90 = sum(
                        pick(c(
                                "leq_87",
                                "88_90"
                        )),
                        na.rm = TRUE
                ),
                total = sum(
                        pick(-c(
                                country,
                                retired_year,
                                leq_90
                        )),
                        na.rm = TRUE
                )
        ) %>%
        ungroup() %>%
        mutate(
                lcp_87_08_na = case_when(
                        retired_year < 2008 ~ NA,
                        .default = leq_87
                ),
                lcp_90_05_na = case_when(
                        retired_year < 2005 ~ NA,
                        .default = leq_90
                ),
                lcp_90_08_na = case_when(
                        retired_year < 2008 ~ NA,
                        .default = leq_90
                ),
                lcp_90_08_16all = case_when(
                        retired_year < 2008 ~ NA,
                        retired_year >= 2016 ~ total,
                        .default = leq_90
                )
        ) %>%
        complete(
                country = countries_list$eu25_countries,
                retired_year = c(1990:2021)
        ) %>%
        arrange(country, retired_year) %>%
        mutate(
                across(
                        .cols = c(
                                lcp_87_08_na:lcp_90_08_16all
                        ),
                        .fns = ~ order_by(
                                retired_year,
                                cumsum(replace_na(
                                        .x,
                                        0
                                ))
                        )
                ),
                .by = c(country)
        ) %>%
        select(country, retired_year, matches("lcp_")) %>%
        mutate(across(
                .cols = starts_with("lcp_"),
                # Change code here to decide how to deal with log(0) values
                .fns = ~ .x + 1e-10
        ))

socioeconomic <- left_join(
        socioeconomic,
        gcpt,
        by = c("country", "year" = "retired_year")
)

rm(list = ls(pattern = "gcpt"))



# Load and clean emep_data ----------------------------------------------------

# 2021 emep data (includes data on the UK)
emep_data_21 <- read_tsv(
        here(
                "data",
                "pollution",
                "emep",
                "CLRTAP_NVFR14_V21_GF.csv"
        ),
        na = c("")
) %>%
        functions$clean_emep_data() %>%
        mutate(data_source = "EMEP_21")

# most recent emep data (2023) - no data on the UK
emep_data_23 <- read_tsv(
        here(
                "data",
                "pollution",
                "emep",
                "CLRTAP_NVFR19_V23_1_GF_csv.csv"
        ),
        na = c("")
) %>%
        functions$clean_emep_data() %>%
        mutate(data_source = "EMEP_23")

emep_data_21_wide <- emep_data_21 %>%
        functions$widen_emep_data()

emep_data_23_wide <- emep_data_23 %>%
        functions$widen_emep_data()

rm(emep_data_21, emep_data_23)



# merge all data sets ---------------------------------------------------------

final_data_wide <- list(
        edgar_wide,
        socioeconomic,
        emep_data_21_wide,
        emep_data_23_wide
) %>%
        reduce(
                full_join,
                by = c(
                        "country",
                        "year"
                )
        ) %>%
        arrange(country, year)



# Get final_data into long format ---------------------------------------------
final_data <- final_data_wide %>%
        pivot_longer(
                cols = matches("^(pm25|so2|nox|co2)_.*"),
                names_to = c(
                        "pollutant",
                        "sector_code",
                        "data_source",
                        "emissions_type"
                ),
                names_pattern = "(pm25|so2|nox|co2)_(.*)_(EDGAR|EMEP_21|EMEP_23|UN)_(fossil|bio|total|NA)", # nolint
                values_to = "emissions"
        ) %>%
        arrange(country, year, data_source, sector_code) %>%
        filter(case_when(
                data_source == "UN" &
                        sector_code %in%
                                sectors_list$exclude_un ~ FALSE,
                data_source == "UN" &
                        str_detect(
                                sector_code,
                                "(^Sectors)|(^ind_)"
                        ) ~ FALSE,
                data_source %in% c("EMEP_21", "EMEP_23") &
                        str_detect(
                                sector_code,
                                "^[:alpha:]"
                        ) ~ FALSE,
                data_source %in% c("EMEP_21", "EMEP_23") &
                        sector_code %in% c(
                                "1A3"
                        ) ~ FALSE,
                .default = TRUE
        ))

rm(
        final_data_wide, socioeconomic, edgar_wide,
        emep_data_21_wide, emep_data_23_wide
)


# final_data EDGAR only -------------------------------------------------------
final_data_edgar <- final_data %>%
        filter(data_source == "EDGAR") %>%
        select(-c(data_source)) %>%
        functions$remove_empty_sector_country_pairs()

final_data_edgar_bio <- final_data_edgar %>%
        filter(emissions_type == "total") %>%
        select(-c(emissions_type))

final_data_edgar %<>%
        filter(emissions_type == "fossil") %>%
        select(-c(emissions_type))



# final data EMEP only --------------------------------------------------------
final_data_emep_21 <- final_data %>%
        filter(data_source == "EMEP_21") %>%
        select(-any_of(c(
                "data_source",
                "emissions_type"
        ))) %>%
        functions$remove_empty_sector_country_pairs()

final_data_emep_21_uk <- final_data_emep_21 %>%
        filter(country == "United Kingdom")

final_data_emep_23 <- final_data %>%
        filter(data_source == "EMEP_23") %>%
        select(-any_of(c(
                "data_source",
                "emissions_type"
        ))) %>%
        functions$remove_empty_sector_country_pairs() %>%
        full_join(
                x = .,
                y = final_data_emep_21_uk
        ) %>%
        arrange(country, year, sector_code)



# final data EDGAR + EMEP/UN proportions --------------------------------------

final_data_1a2_emep21 <- functions$create_emep_un_1a2_proportions(
        df = final_data,
        data_source_var = "EMEP_21"
)

final_data_1a2_emep21_uk <- final_data_1a2_emep21 %>%
        filter(country == "United Kingdom")

final_data_1a2_emep23 <- functions$create_emep_un_1a2_proportions(
        df = final_data,
        data_source_var = "EMEP_23"
) %>%
        full_join(
                x = .,
                y = final_data_1a2_emep21_uk
        ) %>%
        arrange(country, year, sector_code_emep_un)

rm(final_data, final_data_emep_21_uk, final_data_1a2_emep21_uk)



# write final_data csv files --------------------------------------------------
if (misc_parameters$write_csv_files) {

        file_names <- c(
                "final_data_edgar.csv",
                "final_data_edgar_bio.csv",
                "final_data_emep_21.csv",
                "final_data_emep_23.csv",
                "final_data_1a2_emep21.csv",
                "final_data_1a2_emep23.csv"
        )

        data_list <- list(
                final_data_edgar,
                final_data_edgar_bio,
                final_data_emep_21,
                final_data_emep_23,
                final_data_1a2_emep21,
                final_data_1a2_emep23
        )

        for (i in seq_along(file_names)) {
                write_csv(
                        data_list[[i]],
                        here("data", "final_data", file_names[i])
                )
        }

        rm(file_names, data_list, i)
}



# Prep country classifications ------------------------------------------------
country_class_groups <- read_csv(here(
        "data",
        "socioeconomic",
        "World Bank",
        "csv_raw",
        "country_classifications_groups.csv"
)) %>%
        group_by(CountryName) %>%
        filter(
                any(str_detect(
                        GroupName,
                        "small states"
                ) |
                        GroupName %in% c(
                                "Fragile and conflict affected situations",
                                "Heavily indebted poor countries (HIPC)"
                        )
                ),
                !any(GroupName %in% c(
                        "Euro area",
                        "European Union",
                        "OECD members"
                ))
        ) %>%
        ungroup() %>%
        distinct(CountryCode) %>%
        pull(CountryCode)


country_class <- read_csv(here(
        "data",
        "socioeconomic",
        "World Bank",
        "csv_raw",
        "country_classifications.csv"
)) %>%
        rename_with(
                .fn = str_to_lower,
                .cols = everything()
        ) %>%
        rename_with(
                .fn = str_replace_all,
                .cols = everything(),
                pattern = " ",
                replacement = "_"
        ) %>%
        filter(
                !code %in% country_class_groups
        ) %>%
        mutate(country = countrycode(
                sourcevar = code,
                origin = "wb",
                destination = "country.name"
        )) %>%
        arrange(income_group, country) %>%
        select(c(country, income_group)) %>%
        filter(
                if_all(
                        .cols = everything(),
                        .fns = ~ !is.na(.x)
                ),
                income_group %in% c(
                        "High income",
                        "Upper middle income"
                )
        )

if (misc_parameters$write_csv_files) {
        write_csv(
                country_class,
                here(
                        "data",
                        "socioeconomic",
                        "World Bank",
                        "csv_clean",
                        "country_classifications.csv"
                )
        )
}

rm(country_class, country_class_groups)