# Load relevant data sets -----------------------------------------------------
if (!misc_parameters$prep_data) { # nolint
        if (specification_choices$main_data %in% c("emep21/un", "emep23/un")) {
                if (specification_choices$pollutant == "co2") {
                        gscm <- read_csv(here(
                                "data",
                                "final_data",
                                "final_data_un.csv"
                        ))
                } else {
                        if (specification_choices$main_data %in%
                                c("emep21/un")) { # nolint
                                gscm <- read_csv(here(
                                        "data",
                                        "final_data",
                                        "final_data_emep_21.csv"
                                ))
                        } else if (specification_choices$main_data %in%
                                c("emep23/un")) { # nolint
                                gscm <- read_csv(here(
                                        "data",
                                        "final_data",
                                        "final_data_emep_23.csv"
                                ))
                        }
                }
        } else if (specification_choices$main_data == "edgar") {
                if (specification_choices$include_bio == "yes") {
                        gscm <- read_csv(here(
                                "data",
                                "final_data",
                                "final_data_edgar_bio.csv"
                        ))
                } else if (specification_choices$include_bio == "no") {
                        gscm <- read_csv(here(
                                "data",
                                "final_data",
                                "final_data_edgar.csv"
                        ))
                }

                if (specification_choices$include_1a2 %in%
                        c("emep23/un", "emep21/un", "emep21+23/un")) { # nolint

                        if (specification_choices$include_1a2 %in%
                                c("emep21/un")) { # nolint
                                final_data_1a2 <- read_csv(here(
                                        "data",
                                        "final_data",
                                        "final_data_1a2_emep21.csv"
                                ))
                        } else if (specification_choices$include_1a2 %in%
                                c("emep23/un")) { # nolint
                                final_data_1a2 <- read_csv(here(
                                        "data",
                                        "final_data",
                                        "final_data_1a2_emep23.csv"
                                )) %>%
                                        # UK data was taken from emep_21
                                        filter(country != "United Kingdom")
                        } else if (specification_choices$include_1a2 %in%
                                c("emep21+23/un")) { # nolint
                                final_data_1a2 <- read_csv(here(
                                        "data",
                                        "final_data",
                                        "final_data_1a2_emep23.csv"
                                ))
                        }
                }
        }
} else {
        if (specification_choices$main_data %in% c("emep21/un", "emep23/un")) {
                if (specification_choices$pollutant == "co2") {
                        gscm <- final_data_un
                } else {
                        if (specification_choices$main_data %in%
                                c("emep21/un")) { # nolint
                                gscm <- final_data_emep_21
                        } else if (specification_choices$main_data %in%
                                c("emep23/un")) { # nolint
                                gscm <- final_data_emep_23
                        }
                }
        } else if (specification_choices$main_data == "edgar") {
                if (specification_choices$include_bio == "yes") {
                        gscm <- final_data_edgar_bio
                } else if (specification_choices$include_bio == "no") {
                        gscm <- final_data_edgar
                }
        }
}

gscm  %<>%
        filter(pollutant == specification_choices$pollutant)



# add emep/un 1.A.2.x information to gscm -------------------------------------
if (specification_choices$main_data == "edgar" &&
        specification_choices$include_1a2 %in% # nolint
                        c("emep23/un", "emep21/un", "emep21+23/un")) { # nolint
        final_data_1a2 %<>%
                filter(pollutant == specification_choices$pollutant) %>%
                select(-any_of(c(
                        "pollutant",
                        "data_source"
                )))

        gscm %<>%
                full_join(
                        x = .,
                        y = final_data_1a2,
                        by = c("country", "year",
                                "sector_code" = "parent_sector_code"
                        )
                ) %>%
                mutate(
                        emissions = case_when(
                                sector_code == "1.A.2" ~
                                        emissions * emission_shares, # nolint
                                .default = emissions
                        ),
                        sector_code = case_when(
                                sector_code == "1.A.2" ~
                                        sector_code_emep_un,
                                .default = sector_code
                        )
                ) %>%
                select(-c(sector_code_emep_un)) %>%
                filter(if_else(
                        str_detect(sector_code, "^1\\.A\\.2.*"),
                        !is.na(emissions),
                        TRUE
                ))

        rm(final_data_1a2)
}



# Per capita emissions ---------------------------------------------------
if (specification_choices$per_capita_emissions) {
        gscm %<>%
                # convert data from t to t/population
                mutate(emissions = emissions / population)
}



# More filtering and creation of vars for gscm --------------------------------

functions$create_variable_vectors()

gscm %<>%
        mutate(gdp = get(glue("gdp{misc_parameters$gdp_pc}_{specification_choices$gdp}"))) %>% # nolint
        mutate(across(
                .cols = misc_parameters$vars_to_log,
                .fns = log,
                .names = "log_{.col}"
        )) %>%
        filter(
                year >= specification_choices$year_first,
                year <= specification_choices$year_last
        )

if (all(!specification_choices$covariates %in% c("none"))) {
        gscm %<>%
                mutate(log_gdp_2 = log_gdp^2)
}

if (specification_choices$gaps_in_years == "interpolate" &&
        !specification_choices$balanced_panel) { # nolint
        gscm_unfiltered_temp <- gscm
}

gscm %<>%
        # gsynth() does not tolerate missing data
        filter(if_all(
                .cols = c(misc_parameters$vars_to_analyse),
                .fns = ~ !is.na(.)
        )) %>%
        select(
                country,
                year,
                sector_code,
                ets_country,
                misc_parameters$vars_to_analyse,
                misc_parameters$vars_to_log
        ) %>%
        arrange(country, sector_code, year) %>%
        # create id variables; unit_id used for fixed effects
        mutate(
                country_id = fct(country) %>% as.numeric(),
                unit_id = fct(glue("{country}_{sector_code}")),
                .after = country
        )



# generate treatment and post variables ---------------------------------------

# staggered treatment
if (specification_choices$treatment_timing == "staggered") {
        gscm %<>%
                mutate(
                        treat = if_else(
                                country %in% countries_list$ets_countries &
                                        sector_code %in% sectors_list$ets_sectors, # nolint
                                1,
                                0
                        ),
                        treat_post = if_else(
                                # note that the var "ets_country" is time-varying, making it different from the vector "countries_list$ets_countries" #nolint
                                ets_country == 1 &
                                        sector_code %in% sectors_list$ets_sectors, # nolint
                                1,
                                0
                        ),
                        .after = year
                )
}

# uniform treatment period
if (specification_choices$treatment_timing %in% c("common")) {

        gscm %<>%
                mutate(
                        treat = if_else(
                                country %in% countries_list$ets_countries &
                                        sector_code %in% sectors_list$ets_sectors, #nolint
                                1,
                                0
                        ),
                        post = if_else(
                                year >= specification_choices$ets_start_year,
                                1,
                                0
                        ),
                        .after = year
                ) %>%
                mutate(
                        treat_post = treat * post,
                        .after = post
                )

        # drop countries that are not yet regulated at the beginning
        if (specification_choices$ensure_common) {
                if (specification_choices$ets_start_year < 2007) {
                        gscm %<>%
                                filter(if_else(
                                        treat == 1,
                                        !(country %in% c(
                                                "Croatia",
                                                "Iceland",
                                                "Liechtenstein",
                                                "Norway",
                                                "Bulgaria",
                                                "Romania"
                                        )),
                                        TRUE
                                ))
                } else if (specification_choices$ets_start_year < 2008) {
                        gscm %<>%
                                filter(if_else(
                                        treat == 1,
                                        !(country %in% c(
                                                "Croatia",
                                                "Iceland",
                                                "Liechtenstein",
                                                "Norway"
                                        )),
                                        TRUE
                                ))
                } else if (specification_choices$ets_start_year < 2013) {
                        gscm %<>%
                                filter(if_else(
                                        treat == 1,
                                        !(country %in% c(
                                                "Croatia",
                                                "Iceland"
                                        )),
                                        TRUE
                                ))
                }
        }

        # exclude sectors for which treatment did not start in ets_start_year
        # ... e.g., for data reasons
        exclude <- gscm %>%
                filter(treat_post == 1) %>%
                group_by(unit_id) %>%
                filter(year == min(year)) %>%
                filter(year != specification_choices$ets_start_year) %>%
                pull(unit_id)

        gscm %<>%
                filter(!(unit_id %in% exclude))
}



# Filter data according to treat pool and donor pool choices ------------------
gscm %<>%
        functions$filter_treat_countries(
                df = .,
                keep_treat_countries = specification_choices$treat_countries
        ) %>%
        functions$filter_treat_sectors(
                df = .,
                keep_treat_sectors = specification_choices$treat_sectors
        ) %>%
        functions$filter_donor_countries(
                df = .,
                keep_donor_countries = specification_choices$donor_countries
        ) %>%
        functions$filter_donor_sectors(
                df = .,
                keep_donor_sectors = specification_choices$donor_sectors
        ) %>%
        # remove those sectors that are only partially regulated
        filter(!(sector_code %in% sectors_list$mixed_ets_sectors))



# Remove country for leave-one-out analysis -----------------------------------
if (specification_choices$leave_one_out) {
        gscm %<>%
                filter(!country %in% specification_choices$country_to_leave_out)
}



# remove UK if include_uk == FALSE --------------------------------------------
# EMEP data after 2021 does not include the UK anymore
if (!specification_choices$include_uk) {
        gscm %<>%
                filter(country != "United Kingdom")
}



# exclude regulated units for which there is no post-treatment data -----------
exclude_no_post_treat <- gscm %>%
        filter(treat == 1) %>%
        mutate(missing_post_data = if_else(
                max(year) < specification_choices$ets_start_year,
                1,
                0
        ), .by = c(unit_id)) %>%
        filter(missing_post_data == 1) %>%
        select(unit_id) %>%
        distinct() %>%
        pull(unit_id)

gscm %<>%
        filter(!(unit_id %in% exclude_no_post_treat))

rm(list = ls(pattern = "exclude"))



# split sectors into regulated/unregulated ------------------------------------
if (specification_choices$unit_of_analysis == "country_treat") { # nolint
        gscm %<>%
                # per unit
                mutate(
                        n_years_unit = n(),
                        min_year_unit = min(year),
                        max_year_unit = max(year),
                        .by = c(unit_id)
                ) %>%
                # number of years missing *between* min(year) and max(year)
                # min and max at the unit level!
                mutate(
                        n_na_years_within_unit =
                                max_year_unit - min_year_unit + 1 -
                                        n_years_unit, # nolint
                        .by = c(unit_id)
                ) %>%
                # per country-treat
                mutate(
                        max_n_years_country_treat = max(n_years_unit),
                        min_year_country_treat = min(min_year_unit),
                        max_year_country_treat = max(max_year_unit),
                        .by = c(country, treat)
                ) %>%
                # across *all* observations
                mutate(
                        max_n_years_overall = max(n_years_unit),
                        min_year_overall = min(min_year_unit),
                        max_year_overall = max(max_year_unit)
                )

        # keep only complete units?
        if (specification_choices$balanced_panel) {
                gscm %<>%
                        filter(
                                n_years_unit == max_n_years_overall,
                                min_year_unit == min_year_overall,
                                max_year_unit == max_year_overall,
                                n_na_years_within_unit == 0
                        )
        } else {
                # make sure that within each country-treat, the sectors used are consistent across years # nolint
                gscm %<>%
                        filter(
                                n_years_unit == max_n_years_country_treat,
                                min_year_unit == min_year_country_treat,
                                max_year_unit == max_year_country_treat
                        )

                misc_parameters$n_problematic_units <- gscm %>%
                        filter(n_na_years_within_unit != 0) %>%
                        distinct(country_id, treat) %>%
                        nrow()

                if (specification_choices$gaps_in_years == "drop" &&
                        misc_parameters$n_problematic_units != 0) { # nolint
                        gscm %<>%
                                filter(n_na_years_within_unit == 0)
                } else if (specification_choices$gaps_in_years == "interpolate" && # nolint
                        misc_parameters$n_problematic_units != 0) { # nolint
                        gscm %<>%
                                filter(n_na_years_within_unit <= 1)
                }
        }

        # aggregate emissions by country-treat-year
        gscm %<>%
                group_by(country, treat, year) %>%
                mutate(emissions_agg = sum(emissions)) %>%
                ungroup() %>%
                # take logs of aggregated values
                mutate(log_emissions_agg = log(emissions_agg)) %>%
                select(
                        country,
                        country_id,
                        year,
                        treat,
                        treat_post,
                        ends_with("country"),
                        contains("_agg"),
                        misc_parameters$vars_to_log %>%
                                discard(. %in% c("emissions")),
                        misc_parameters$vars_to_analyse %>%
                                discard(. %in% c("log_emissions")),
                        n_na_years_within_unit,
                        min_year_country_treat,
                        max_year_country_treat
                ) %>%
                # keep only distinct rows
                distinct() %>%
                rename(ets_coverage = treat) %>%
                mutate(ets_coverage = if_else(
                        ets_coverage == 1,
                        "Regulated",
                        "Unregulated"
                )) %>%
                arrange(country, year) %>%
                rename_with(
                        .fn = ~ str_remove(.x, "_agg"),
                        .cols = contains("agg")
                ) %>%
                relocate(log_emissions, .after = last_col()) %>%
                # create unit_id per country-treat pair
                mutate(
                        unit_id = fct(glue("{country}_{ets_coverage}")),
                        .after = country_id
                )

        if (!specification_choices$balanced_panel &&
                specification_choices$gaps_in_years == "interpolate" && # nolint
                misc_parameters$n_problematic_units != 0) { # nolint


                problematic_units_temp <- gscm %>%
                        filter(n_na_years_within_unit == 1) %>%
                        # within each country-etc_coverage pair, determine which year is missing between min_year_country_treat and max_year_country_treat # nolint
                        mutate(
                                missing_year = if_else(
                                        (lead(year) - year) != 1,
                                        year + 1,
                                        NA
                                )
                        ) %>%
                        filter(!is.na(missing_year)) %>%
                        distinct(
                                country, ets_coverage, missing_year,
                                .keep_all = TRUE
                        ) %>%
                        # change all values in selected cols to NA
                        mutate(across(
                                .cols = c(
                                        year,
                                        treat_post,
                                        ends_with("_country"),
                                        misc_parameters$vars_to_log,
                                        misc_parameters$vars_to_analyse
                                ),
                                .fns = ~ as(NA, class(.))
                        )) %>%
                        mutate(
                                year = missing_year,
                                .keep = "unused"
                        )

                gscm_unfiltered_temp %<>%
                        select(
                                any_of(names(problematic_units_temp)),
                                # emissions are not included as they are at the sectoral level in gscm_unfiltered_temp, whereas problematic_units_temp works with emissions aggregated to the country-treat level # nolint
                                -contains("emissions"),
                                -starts_with("log_")
                        ) %>%
                        distinct()

                problematic_units_temp %<>%
                        rows_patch(
                                x = .,
                                y = gscm_unfiltered_temp,
                                by = c("country", "year"),
                                unmatched = "ignore"
                        )

                gscm <- full_join(
                        problematic_units_temp,
                        gscm
                ) %>%
                        arrange(country, ets_coverage, year) %>%
                        mutate(across(
                                .cols = misc_parameters$vars_to_log,
                                .fns = ~ if_else(
                                        is.na(.),
                                        (lead(.) + lag(.)) / 2,
                                        .
                                )
                        )) %>%
                        mutate(across(
                                .cols = misc_parameters$vars_to_log,
                                .fns = log,
                                .names = "log_{.col}"
                        ))

                if (all(!specification_choices$covariates %in% c("none"))) {
                        gscm %<>%
                                mutate(log_gdp_2 = log_gdp^2)
                }

                gscm %<>%
                        filter(if_all(
                                .cols = c(misc_parameters$vars_to_analyse),
                                .fns = ~ !is.na(.)
                        )) %>%
                        mutate(treat_post = case_when(
                                specification_choices$treatment_timing == "common" & # nolint
                                        ets_coverage == "Regulated" &
                                        year >= specification_choices$ets_start_year ~ # nolint
                                        1,
                                specification_choices$treatment_timing == "staggered" & # nolint
                                        ets_coverage == "Regulated" &
                                        ets_country == 1 ~
                                        1,
                                .default = 0
                        )) %>%
                        select(-any_of(c(
                                "n_na_years_within_unit",
                                "min_year_country_treat",
                                "max_year_country_treat"
                        )))

        }

        rm(list = ls(pattern = "temp$"))

        # filter out data if balanced_panel == TRUE ----------------------
} else if (specification_choices$unit_of_analysis == "country_sector" &&
        specification_choices$balanced_panel) { # nolint
        gscm %<>%
                # per unit, count for how many years data exists
                mutate(
                        n_years_unit = n(),
                        min_year_unit = min(year),
                        max_year_unit = max(year),
                        .by = c(unit_id)
                ) %>%
                # keep only complete units
                filter(
                        n_years_unit == max(n_years_unit),
                        min_year_unit == min(min_year_unit),
                        max_year_unit == max(max_year_unit)
                ) %>%
                select(-c(n_years_unit, max_year_unit))
}