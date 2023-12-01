# Collect functions in list ---------------------------------------------------
functions <- list()



# Master function -------------------------------------------------------------
execute_analysis <- function(
                # data prep options
                prep_data = FALSE,
                write_files = FALSE,

                # what data to use
                per_capita_emissions = FALSE,
                per_capita_gdp = FALSE,
                gdp = "constant",
                year_first = 1990,
                year_last = 2021,
                main_data = "emep23/un",
                include_1a2 = "emep21+23/un", # "edgar_fully_treated" or "emep/un" or "no" # nolint
                include_bio = "yes",
                include_uk = TRUE,
                include_aviation = "control", # following Bayer & Aklin (2020)
                balanced_panel = FALSE,
                gaps_in_years = "interpolate", # "interpolate" or "drop"
                damage_est_source = "uba_eu_27",
                time_var_damages = FALSE,

                # specification choices
                pollutant,
                ets_start_year = 2005,
                treatment_timing = "common", # staggered or common
                ensure_common = TRUE,
                unit_of_analysis = "country_treat", # "country_sector" (disaggregated), "country_treat" (aggregated) #nolint
                donor_countries = "same_as_treat",
                donor_sectors = "all_available", # "all_available", "non_ETS_sectors", "ETS_sectors", "same_as_treat" # nolint
                treat_countries = "eu25_countries",
                treat_sectors = "ets_sectors",
                leave_one_out = FALSE,
                country_to_leave_out = NA,

                #gsynth
                covariates = "standard",
                min_t0 = 7,
                estimator = "ife",
                em = FALSE,
                r = 0,
                cv = TRUE,
                inference_type = "choose",
                criterion = "mspe",
                alpha = 0.05,

                # plot options
                save_plots = TRUE,
                annotate_plots = "att", # "detailed", "att" or anything
                show_lines = c("tr", "ct"),
                loop_mode = FALSE,

                # misc parameters
                conduct_analysis = TRUE,
                write_results_table = TRUE) {

        # create empty lists for vectors, parameters, functions, ...
        sectors_list <<- list()
        countries_list <<- list()
        misc_parameters <<- list()
        specification_choices <<- list()
        social_cost_est <<- list()
        plots <<- list()


        # move function arguments into global environment:
        misc_parameters <<- modifyList(misc_parameters, list(
                # uuid
                uuid = UUIDgenerate(), # unique identifier for each run
                # data
                prep_data = prep_data,
                write_csv_files = write_files,
                # plots
                annotate_plots = annotate_plots,
                save_plots = save_plots,
                show_lines = functions$sort_collapse(show_lines, sep = "_"),
                loop_mode = loop_mode,
                # misc
                conduct_analysis = conduct_analysis,
                write_results_table = write_results_table
        ))

        # move function arguments into global environment:
        specification_choices <<- modifyList(specification_choices, list(
                # what data to use
                per_capita_emissions = per_capita_emissions,
                per_capita_gdp = per_capita_gdp,
                gdp = gdp,
                year_last = year_last,
                main_data = main_data,
                include_1a2 = include_1a2,
                include_bio = include_bio,
                include_uk = include_uk,
                include_aviation = include_aviation,
                balanced_panel = balanced_panel,
                gaps_in_years = gaps_in_years,
                pollutant = pollutant,
                damage_est_source = damage_est_source,
                time_var_damages = time_var_damages,
                # specification choices
                ets_start_year = ets_start_year,
                treatment_timing = treatment_timing,
                ensure_common = ensure_common,
                unit_of_analysis = unit_of_analysis,
                treat_countries = treat_countries,
                treat_sectors = treat_sectors,
                donor_countries = donor_countries,
                donor_sectors = donor_sectors,
                leave_one_out = leave_one_out,
                country_to_leave_out = country_to_leave_out,
                # direct gsynth input
                covariates = str_replace_all(
                        covariates,
                        "gdp_pc",
                        glue("gdp_pc_{gdp}")
                ),
                min_t0 = min_t0,
                estimator = estimator,
                r = r,
                cv = cv,
                em = em,
                inference_type = inference_type,
                criterion = criterion,
                alpha = alpha
        ))

        specification_choices$covariates <<- switch(
                specification_choices$pollutant,
                pm25 = str_replace_all(
                        specification_choices$covariates,
                        "^elv$",
                        "elv_pm"
                ),
                so2 = str_replace_all(
                        specification_choices$covariates,
                        "^elv$",
                        "elv_sox"
                ),
                nox = str_replace_all(
                        specification_choices$covariates,
                        "^elv$",
                        "elv_nox"
                ),
                specification_choices$covariates
        )

        if (specification_choices$per_capita_gdp) {
                misc_parameters$gdp_pc <<- "_pc"
        } else {
                misc_parameters$gdp_pc <<- ""
        }

        if (specification_choices$main_data == "edgar" &&
                (specification_choices$include_1a2 %in% # nolint
                        c("emep23/un", "emep21/un", "emep21+23/un"))) { # nolint
                specification_choices$year_first <<- 1990
        } else {
                specification_choices$year_first <<- year_first
        }

        # sort list alphabetically
        specification_choices <<- specification_choices[
                order(names(specification_choices))
        ]

        # Define various vectors/df for all scripts
        source(here("src", "R", "vectors_df.r"))

        # argument validation
        validate_argument <- function(argument_name) {
                match.arg(
                        # get argument from parent environment
                        as.character(get(argument_name)),
                        misc_parameters$arg_validation[[
                                argument_name
                        ]],
                        several.ok = TRUE
                )
        }

        for (argument in names(formals(execute_analysis))) {
                validate_argument(argument)
        }

        # manually prep data
        if (misc_parameters$prep_data) {
                misc_parameters$pollutants <<- c("pm25", "so2", "nox", "co2") # nolint

                source(here("src", "R", "data_prep.r"))
        }

        if ((!misc_parameters$loop_mode)) {
                # set the data up for the analysis
                source(here("src", "R", "gscm_set_up.r"))

                if (misc_parameters$conduct_analysis) {
                        # run the gscm analysis
                        tryCatch(
                                {
                                        error_occurred <- FALSE
                                        gscm_analysis <<- functions$gscm_run()
                                },
                                error = function(e) {
                                        print(e)
                                        print(glue("[Jonas Grunau] There was an error in gsynth.")) # nolint

                                        error_occurred <<- TRUE
                                }
                        )

                        # create and save plots
                        if (!error_occurred) {
                                source(here(
                                        "src",
                                        "R",
                                        "results_table.r"
                                ))

                                source(here("src", "R", "plots.r"))
                        }

                        print(glue(
                                "ATT: {round(functions$interpret_log_linear(gscm_analysis$est.avg[1]), 2)}% [{round(functions$interpret_log_linear(gscm_analysis$est.avg[3]), 2)}%, {round(functions$interpret_log_linear(gscm_analysis$est.avg[4]), 2)}%]\n", # nolint
                                "p-value: {round(gscm_analysis$est.avg[5], 3)}\n", # nolint
                                "Absolute change: {round(abs_emissions$eff / 1e6, 1)} million t\n", # nolint
                                "Absolute change as share of total emissions: {round(abs_emissions$eff_per_tot, 2)}%\n", # nolint
                                "Social benefits: {round(abs_emissions$eff * -misc_parameters$scp / 1e9, 0)} billion €\n", # nolint
                                "Unique identifier: {misc_parameters$uuid}"
                        ))
                }
        }
}



# Misc helper functions -------------------------------------------------------
functions[["sort_collapse"]] <- function(vector, sep = " + ") {
        vector %>%
                sort() %>%
                glue_collapse(sep = sep)
}

functions[["here_vec"]] <- function(path_vec) {
        do.call(here, as.list(path_vec))
}



# Data prep functions ---------------------------------------------------------
functions[["clean_edgar_data"]] <- function(pollutant) {
        data_out <- read_csv(here(
                "data",
                "pollution",
                "edgar_csv_raw",
                glue("{pollutant}_1970_2018.csv")
        )) %>%
                select(-any_of(c(
                        "IPCC_annex",
                        "C_group_IM24_sh",
                        "ipcc_code_2006_for_standard_report_name"
                ))) %>%
                pivot_longer(
                        cols = Y_1970:Y_2018,
                        names_to = "year",
                        names_prefix = "Y_",
                        values_to = "emissions"
                ) %>%
                rename(
                        pollutant = Substance,
                        country = Name,
                        country_code = Country_code_A3,
                        sector_code = ipcc_code_2006_for_standard_report
                ) %>%
                # convert emissions from kt to t
                mutate(emissions = emissions * 1000)
}

# only needed when using EMEP/UN data
functions[["clean_emep_data"]] <- function(df) {
        df <- df %>%
                filter(Pollutant_name %in% c(
                        "PM2.5", "NOx", "SOx"
                )) %>%
                select(
                        Country_Code, Country, Pollutant_name,
                        Sector_code, Year, Emissions, Unit, Notation
                ) %>%
                rename_with(
                        .fn = str_to_lower,
                        .cols = everything()
                ) %>%
                rename(sector_code_emep = sector_code) %>%
                mutate(
                        country = countrycode(
                                sourcevar = country_code,
                                origin = "iso2c",
                                destination = "country.name"
                        )
                ) %>%
                filter(!is.na(country)) %>%
                # convert from Gg (or kt) to t
                mutate(
                        emissions = emissions * 1000,
                        unit = "t"
                ) %>%
                mutate(
                        pollutant_name = case_when(
                                pollutant_name == "PM2.5" ~ "pm25",
                                pollutant_name == "PM10" ~ "pm10",
                                pollutant_name == "NOx" ~ "nox",
                                pollutant_name == "SOx" ~ "so2",
                                pollutant_name == "CO" ~ "co",
                                TRUE ~ pollutant_name
                        ),
                        sector_code_emep = case_when(
                                sector_code_emep == "1A2a" ~ "1.A.2.a",
                                sector_code_emep == "1A2b" ~ "1.A.2.b",
                                sector_code_emep == "1A2c" ~ "1.A.2.c",
                                sector_code_emep == "1A2d" ~ "1.A.2.d",
                                sector_code_emep == "1A2e" ~ "1.A.2.e",
                                sector_code_emep == "1A2f" ~ "1.A.2.f",
                                sector_code_emep == "1A2gvii" ~ "1.A.2.g.vii",
                                sector_code_emep == "1A2gviii" ~ "1.A.2.g.viii", # nolint
                                .default = sector_code_emep
                        )
                ) %>%
                rename(
                        pollutant = pollutant_name
                ) %>%
                select(-any_of(c("country_code", "unit", "notation"))) %>%
                mutate(emissions_type = "NA")
}

functions[["widen_emep_data"]] <- function(df) {
        df <- df %>%
                pivot_wider(
                        names_from = c(
                                pollutant,
                                sector_code_emep,
                                data_source,
                                emissions_type
                        ),
                        names_sep = "_",
                        values_from = emissions
                ) %>%
                select(-where(~all(is.na(.))))
}

# only needed when using EMEP/UN data
functions[["mutate_notation"]] <- function(df) {
        df <- df %>%
                mutate(notation = case_when(
                        notation == "NA" ~ "not_applicable",
                        notation == "IE" ~ "included_elsewhere",
                        notation == "C" ~ "confidential",
                        notation == "NE" ~ "not_estimated",
                        notation == "NO" ~ "not_occurring",
                        notation == "NR" ~ "not_reported",
                        notation == "NO,IE" ~
                                "not_occurring/included_elsewhere",
                        .default = notation
                ))
}

functions[["remove_empty_sector_country_pairs"]] <- function(df) {
        df %>%
                group_by(country, sector_code) %>%
                mutate(
                        emissions_sum = sum(emissions, na.rm = TRUE)
                ) %>%
                ungroup() %>%
                filter(emissions_sum != 0) %>%
                select(-any_of(c("emissions_sum")))
}

functions[["create_emep_un_1a2_proportions"]] <- function(df, data_source_var) {
        df %>%
                filter(
                        case_when(
                                pollutant == "co2" &
                                        data_source == "UN" ~ TRUE,
                                pollutant != "co2" &
                                        data_source == data_source_var ~ TRUE,
                                .default = FALSE
                        ),
                        str_detect(sector_code, "^1\\.A\\.2\\.")
                ) %>%
                functions$remove_empty_sector_country_pairs() %>%
                mutate(parent_sector_code = "1.A.2") %>%
                select(
                        country,
                        year,
                        data_source,
                        pollutant,
                        sector_code,
                        parent_sector_code,
                        emissions
                ) %>%
                group_by(country, year, pollutant, data_source) %>%
                mutate(
                        emission_shares =
                                emissions / sum(emissions, na.rm = TRUE)
                ) %>%
                ungroup() %>%
                rename(sector_code_emep_un = sector_code) %>%
                select(-c(emissions))
}


# GSCM set up helpers ---------------------------------------------------------
# creating two vectors: one with vars to take the log of and another one
        # containing the variables used in the analysis # nolint
functions[["create_variable_vectors"]] <- function() {
        misc_parameters$vars_to_log <<- c(
                "gdp",
                "emissions",
                specification_choices$covariates
        ) %>%
                discard(. %in% c("standard", "carbon_pricing_dummy", "none"))

        misc_parameters$vars_to_analyse <<- c(
                "log_emissions",
                "log_gdp",
                "log_gdp_2",
                glue("log_{specification_choices$covariates}")
        ) %>%
                discard(. %in% c("log_standard", "log_none")) %>%
                # add to this line other covariates that need no log
                str_replace_all(c(
                        "log_carbon_pricing_dummy" = "carbon_pricing_dummy",
                        "log_elv_sox" = "elv_sox",
                        "log_elv_nox" = "elv_nox",
                        "log_elv_pm" = "elv_pm"
                ))

        # remove gdp from specification with no covs
        if (all(specification_choices$covariates == "none")) {
                misc_parameters$vars_to_log <<- misc_parameters$vars_to_log %>%
                        discard(. %in% c("gdp"))

                misc_parameters$vars_to_analyse <<- misc_parameters$vars_to_analyse %>% # nolint
                        discard(. %in% c("log_gdp", "log_gdp_2"))
        }
}

# functions to apply the filters to the treatment and donor groups
functions[["filter_treat_countries"]] <- function(df, keep_treat_countries) {
        if (all(keep_treat_countries %in% names(countries_list))) {
                df %>%
                        filter(if_else(
                                treat == 1,
                                country %in%
                                        countries_list[[keep_treat_countries]],
                                TRUE
                        ))
        } else if (all(keep_treat_countries %in%
                countries_list$ets_countries)) { # nolint
                df %>%
                        filter(if_else(
                                treat == 1,
                                country %in% keep_treat_countries,
                                TRUE
                        ))
        }
}

functions[["filter_treat_sectors"]] <- function(df, keep_treat_sectors) {
        if (all(keep_treat_sectors %in% names(sectors_list))) {
                df %>%
                        filter(if_else(
                                treat == 1,
                                sector_code %in%
                                        sectors_list[[keep_treat_sectors]],
                                TRUE
                        ))
        } else if (all(keep_treat_sectors %in% sectors_list$ets_sectors)) {
                df %>%
                        filter(if_else(
                                treat == 1,
                                sector_code %in%
                                        keep_treat_sectors,
                                TRUE
                        ))
        }
}

functions[["filter_donor_countries"]] <- function(df, keep_donor_countries) {
        if (all(keep_donor_countries %in% names(countries_list))) {
                df %>%
                        filter(if_else(
                                treat == 0,
                                country %in%
                                        countries_list[[keep_donor_countries]],
                                TRUE
                        ))
        } else if (all(keep_donor_countries %in%
                countries_list$all_countries)) { # nolint
                df %>%
                        filter(if_else(
                                treat == 0,
                                country %in%
                                        keep_donor_countries,
                                TRUE
                        ))
        } else if (all(keep_donor_countries %in% c("same_as_treat"))) {
                if (all(specification_choices$treat_countries %in%
                        names(countries_list))) { # nolint
                        df %>%
                                filter(if_else(
                                        treat == 0,
                                        country %in%
                                                countries_list[[specification_choices$treat_countries]], # nolint
                                        TRUE
                                ))
                } else if (all(specification_choices$treat_countries %in%
                        countries_list$ets_countries)) { # nolint
                        df %>%
                                filter(if_else(
                                        treat == 0,
                                        country %in% specification_choices$treat_countries, # nolint
                                        TRUE
                                ))
                }
        }
}

functions[["filter_donor_sectors"]] <- function(df, keep_donor_sectors) {
        if (all(keep_donor_sectors %in% names(sectors_list))) {
                df %>%
                        filter(if_else(
                                treat == 0,
                                sector_code %in%
                                        sectors_list[[keep_donor_sectors]],
                                TRUE
                        ))
        } else if (all(keep_donor_sectors == "same_as_treat")) {
                if (all(specification_choices$treat_sectors %in%
                        names(sectors_list))) { # nolint
                        df %>%
                                filter(if_else(
                                        treat == 0,
                                        sector_code %in%
                                                sectors_list[[specification_choices$treat_sectors]], # nolint
                                        TRUE
                                ))
                } else if (all(specification_choices$treat_sectors %in%
                        sectors_list$ets_sectors)) { # nolint
                        df %>%
                                filter(if_else(
                                        treat == 0,
                                        sector_code %in%
                                                specification_choices$treat_sectors, # nolint
                                        TRUE
                                ))
                }
        } else if (all(keep_donor_sectors == "all_available")) {
                df
        } else if (all(keep_donor_sectors == "hybrid")) {
                # it basically chooses "all_available" in treat_countries and "same_as_treat" in the subgroup of donor countries that is not overlapping with treat_countries; hybrid option # nolint
                # this one is a bit more tricky as we need to write options for all combinations of treat_sectors and treat_countries # nolint

                if (all(specification_choices$treat_countries %in%
                        names(countries_list))) { # nolint
                        if (all(specification_choices$treat_sectors %in%
                                names(sectors_list))) { # nolint

                                df %<>%
                                        filter(
                                                if_else(
                                                        treat == 0 &
                                                                (!country %in%
                                                                        countries_list[[specification_choices$treat_countries]]), # nolint
                                                        sector_code %in%
                                                                sectors_list[[specification_choices$treat_sectors]], # nolint
                                                        TRUE
                                                )
                                        )

                        } else if (all(specification_choices$treat_sectors %in%
                                sectors_list$ets_sectors)) { # nolint

                                df %<>%
                                        filter(
                                                if_else(
                                                        treat == 0 &
                                                                (!country %in%
                                                                        countries_list[[specification_choices$treat_countries]]), # nolint
                                                        sector_code %in%
                                                                specification_choices$treat_sectors, # nolint
                                                        TRUE
                                                )
                                        )

                        }
                } else if (all(specification_choices$treat_countries %in%
                        countries_list$ets_countries)) { # nolint
                        if (all(specification_choices$treat_sectors %in%
                                names(sectors_list))) { # nolint

                                df %<>%
                                        filter(
                                                if_else(
                                                        treat == 0 &
                                                                (!country %in%
                                                                        specification_choices$treat_countries), # nolint
                                                        sector_code %in%
                                                                sectors_list[[specification_choices$treat_sectors]], # nolint
                                                        TRUE
                                                )
                                        )

                        } else if (all(specification_choices$treat_sectors %in%
                                sectors_list$ets_sectors)) { # nolint

                                df %<>%
                                        filter(
                                                if_else(
                                                        treat == 0 &
                                                                (!country %in%
                                                                        specification_choices$treat_countries), # nolint
                                                        sector_code %in%
                                                                specification_choices$treat_sectors, # nolint
                                                        TRUE
                                                )
                                        )
                        }
                }

        } else if (all(keep_donor_sectors %in%
                sectors_list$all_sectors_codes)) { # nolint
                df %>%
                        filter(if_else(
                                treat == 0,
                                sector_code %in%
                                        keep_donor_sectors,
                                TRUE
                        ))
        }
}



# GSCM run helpers ------------------------------------------------------------
functions[["gscm_run"]] <- function() {

        misc_parameters$formula <<- log_emissions ~
                treat_post + log_gdp + log_gdp_2

        # add covariates to the formula if needed
        if (any(!specification_choices$covariates %in% c("standard", "none"))) {
                covs <- misc_parameters$vars_to_analyse[
                        !misc_parameters$vars_to_analyse %in% c(
                                "log_emissions",
                                "log_gdp",
                                "log_gdp_2"
                        )
                ]

                misc_parameters$formula <<- as.formula(paste(
                        as.character(misc_parameters$formula[2]),
                        as.character(misc_parameters$formula[1]),
                        as.character(misc_parameters$formula[3]),
                        "+",
                        paste(covs, collapse = "+")
                ))
                # case of no covs
        } else if (all(specification_choices$covariates %in% c("none"))) {
                misc_parameters$formula <<- log_emissions ~
                        treat_post
        }

        if (specification_choices$inference_type == "choose") {
                misc_parameters$number_treated <<- gscm %>%
                        filter(treat_post == 1) %>%
                        distinct(unit_id) %>%
                        nrow()

                if (misc_parameters$number_treated < 41) {
                        specification_choices$inference_type <<- "parametric"
                } else {
                        specification_choices$inference_type <<- "nonparametric"
                }

                if (specification_choices$estimator == "mc") {
                        specification_choices$inference_type <<- "nonparametric"
                }
        }

        gsynth(
                formula = misc_parameters$formula,
                data = gscm,
                min.T0 = specification_choices$min_t0,
                index = c("unit_id", "year"),
                force = "two-way",
                CV = specification_choices$cv,
                r = specification_choices$r,
                estimator = specification_choices$estimator,
                EM = specification_choices$em,
                se = TRUE,
                nboots = 1000,
                inference = specification_choices$inference_type,
                criterion = specification_choices$criterion,
                alpha = specification_choices$alpha,
                seed = 1234
        )
}



# Results tables --------------------------------------------------------------
functions[["reshape_tr_ct_co"]] <- function(data, type) {
        as_tibble(data) %>%
                rename_with(
                        .cols = everything(),
                        .fn = ~ str_remove(., pattern = "_.*")
                ) %>%
                mutate(year = gscm_analysis$time, .before = 1) %>%
                pivot_longer(
                        cols = -year,
                        names_to = "country",
                        values_to = as.character(glue("log_emissions_", type))
                )
}

functions[["aggregate_abs_emissions"]] <- function(agg_by,
                                                   ending,
                                                   drop_var,
                                                   millions = FALSE) {
        eff <- glue("eff_{ending}")
        emissions_tr <- glue("emissions_tr_{ending}")
        emissions_tot <- glue("emissions_tot_{ending}")
        eff_per_tr <- glue("eff_per_tr_{ending}")
        eff_per_tot <- glue("eff_per_tot_{ending}")

        df <- abs_emissions_c_y %>%
                mutate(
                        {{ eff }} :=
                                sum(eff_c_y),
                        {{ emissions_tr }} :=
                                sum(emissions_tr_c_y),
                        {{ emissions_tot }} :=
                                sum(emissions_tot_c_y),
                        .by = {{ agg_by }},
                        .keep = "unused"
                ) %>%
                select(-c(ends_with("_c_y"))) %>%
                distinct(
                        pick(-c({{ drop_var }}))
                ) %>%
                mutate(
                        {{ eff_per_tr }} :=
                                round(get(eff) / get(emissions_tr), 3) * 100,
                        {{ eff_per_tot }} :=
                                round(get(eff) / get(emissions_tot), 3) * 100
                )

        if (millions) {
                df %<>%
                        mutate(across(
                                .cols = -contains(c(
                                        "per",
                                        "country",
                                        "year"
                                )),
                                .fns = ~ . / 1e6
                        ))
        }

        df
}

functions[["lengthen_eff_vector"]] <- function(var) {
        c(
                rep(
                        x = NA,
                        times =
                                length(gscm_analysis$time) -
                                        length(abs_emissions_y$year) # nolint
                ),
                abs_emissions_y[[var]]
        )
}

functions[["tons_to_billions_eur"]] <- function(df) {
        df %>%
                mutate(across(
                        .cols = matches(
                                "(^eff.{0,2}$)|(^eff_low.{0,2}$)|(^eff_high.{0,2}$)" # nolint
                        ),
                        .fns = ~ . * -scp / 1e9,
                        .names = "{.col}_mon"
                ), .before = matches("^eff_per_tr.{0,2}$"))
}



# Plotting function ATT -------------------------------------------------------

functions[["interpret_log_linear"]] <- function(coef_est) {
        (exp(coef_est) - 1) * 100
}

functions[["plot_results_att"]] <- function(results) {
        min_y <- min(
                functions$interpret_log_linear(results$est.att[, 3]),
                na.rm = TRUE
        )
        min_y <- ceiling(abs(min_y / 10)) * 10 *
                if_else(min_y < 0, -1, 1)

        max_y <- max(
                functions$interpret_log_linear(results$est.att[, 4]),
                na.rm = TRUE
        )
        max_y <- ceiling(abs(max_y / 10)) * 10 *
                if_else(max_y < 0, -1, 1)

        name <- deparse(substitute(results))

        att_avg <- round(
                functions$interpret_log_linear(results$est.avg[1]),
                3
        )
        att_avg_p <- round(
                results$est.avg[5],
                3
        )

        pollutant <- switch(specification_choices$pollutant,
                pm25 = "PM2.5",
                so2 = "SO2",
                nox = "NOx",
                co2 = "CO2"
        )

        if (specification_choices$treatment_timing == "staggered") {
                x_values <- names(results$att) %>%
                        as.numeric()
                x_lab_name <- "Deviation from treatment (years)"
                plot_title <- glue("ATT Estimates for {pollutant} up to {specification_choices$year_last}") # nolint

        } else if (specification_choices$treatment_timing %in% c("common")) { #nolint
                x_values <- results$time %>%
                        as.numeric()
                x_lab_name <- "Year"
                plot_title <- glue("ATT Estimates for {pollutant}, {specification_choices$ets_start_year}-{specification_choices$year_last}") #nolint
        }


        plot <- ggplot() +
                geom_ribbon(
                        aes(
                                x = x_values,
                                ymin = functions$interpret_log_linear(results$est.att[, 3]), #nolint
                                ymax = functions$interpret_log_linear(results$est.att[, 4]) #nolint
                        ),
                        fill = "#999999"
                ) +
                geom_line(
                        aes(
                                x = x_values,
                                y = functions$interpret_log_linear(results$est.att[, 1]) #nolint
                        ),
                        color = "#56B4E9",
                        linewidth = 1.5
                ) +
                labs(
                        x = x_lab_name,
                        y = "Difference in emissions (percent)",
                        title = plot_title,
                        subtitle = "Generalized synthetic control"
                ) +
                geom_hline(
                        yintercept = 0,
                        linetype = "dashed",
                        linewidth = 0.7
                ) +
                scale_y_continuous(
                        # limits = c(-40, 30),
                        breaks = seq(min_y, max_y, by = 10)
                ) +
                theme_bw() +
                theme(
                        plot.margin = unit(c(.5, .5, .5, .5), "cm"),
                        plot.title = element_text(
                                size = 18,
                                face = "bold"
                        ),
                        plot.subtitle = element_text(size = 16),
                        axis.title = element_text(size = 14),
                        axis.text = element_text(size = 12)
                )

        if (misc_parameters$annotate_plots == "detailed") {
                plot <- plot +
                        annotate(
                                "label",
                                x = min(x_values),
                                y = max_y,
                                label = glue(
                                        "Pollutant: {specification_choices$pollutant}\n", # nolint
                                        "Sectors treated: {specification_choices$treat_sectors}; countries treated: {specification_choices$treat_countries}\n", # nolint
                                        "Sectors donor: {specification_choices$donor_sectors}; countries donor: {specification_choices$donor_countries}\n", # nolint
                                        "p.c. analysis: {specification_choices$per_capita_emissions}; GDP type: {specification_choices$gdp}\n", # nolint
                                        "Include Sector 1.A.2? {specification_choices$include_1a2}; Complete units only? {specification_choices$balanced_panel}\n", # nolint
                                        "ETS start year: {specification_choices$ets_start_year}; Treatment timing: {specification_choices$treatment_timing}\n", # nolint
                                        "Covariates: {specification_choices$covariates}; unit of analysis: {specification_choices$unit_of_analysis}\n", # nolint
                                        "ATT.avg (p-value): {att_avg} ({att_avg_p})" # nolint
                                ),
                                hjust = 0,
                                vjust = 1,
                                size = 3,
                                fill = "#d6d3d3",
                                label.padding = unit(0.5, "lines")
                        )
        } else if (misc_parameters$annotate_plots == "att") {
                plot <- plot +
                        annotate(
                                "label",
                                x = min(x_values),
                                y = max_y,
                                label = glue(
                                        "Pollutant: {specification_choices$pollutant}\n", # nolint
                                        "ATT.avg (p-value): {att_avg} ({att_avg_p})" # nolint
                                ),
                                hjust = 0,
                                vjust = 1,
                                size = 3,
                                fill = "#d6d3d3",
                                label.padding = unit(0.5, "lines")
                        )
        }


        # plot for staggered version ------------------------------------------
        if (specification_choices$treatment_timing == "staggered") {
                plot <- plot +
                        geom_vline(
                                aes(xintercept = 0),
                                linewidth = 1.2
                        ) +
                        scale_x_continuous(
                                breaks = seq(
                                        ceiling(abs(min(x_values) / 10)) * -10,
                                        ceiling(abs(max(x_values) / 10)) * 10,
                                        by = 5
                                )
                        )

                # plot for common treatment timing ----------------------------
        } else if (specification_choices$treatment_timing %in% c("common")) { # nolint
                plot <- plot +
                        # 2005 ETS start line
                        geom_vline(
                                aes(xintercept = 2005),
                                # linetype = "longdash",
                                linewidth = 0.7
                        ) +
                        geom_vline(
                                aes(
                                        xintercept = specification_choices$ets_start_year # nolint
                                ),
                                linewidth = 1.2
                        ) +
                        scale_x_continuous(
                                breaks = seq(
                                        min(x_values),
                                        max(x_values),
                                        by = 5
                                )
                        )
        }

        plot
}



# Plotting function ct vs tr --------------------------------------------------
functions[["plot_results_ct_tr"]] <- function(results) {

        att_avg <- round(
                functions$interpret_log_linear(results$est.avg[1]),
                3
        )
        att_avg_p <- round(
                results$est.avg[5],
                3
        )

        pollutant <- switch(specification_choices$pollutant,
                pm25 = "PM2.5",
                so2 = "SO2",
                nox = "NOx",
                co2 = "CO2"
        )

        if (misc_parameters$show_lines == "ct_tr") {
                scale_color_manual_values <- c(
                        "#E69F00",
                        "black"
                )
                scale_color_manual_breaks <- c(
                        "counterfactual",
                        "treated"
                )
                scale_color_manual_labels <- c(
                        "Counterfactual average Y(0)",
                        "Treated average Y(1)"
                )
                plot_title <- glue(
                                "Treated and Counterfactual Emission Paths ({pollutant})" # nolint
                        )
        } else if (misc_parameters$show_lines == "co_ct_tr") {
                scale_color_manual_values <- c(
                        "#E69F00",
                        "black",
                        "blue"
                )
                scale_color_manual_breaks <- c(
                        "counterfactual",
                        "treated",
                        "control"
                )
                scale_color_manual_labels <- c(
                        "Counterfactual average Y(0)",
                        "Treated average Y(1)",
                        "Control average"
                )
                plot_title <- glue(
                                "Treated, Counterfactual, and Control Emission Paths ({pollutant})" # nolint
                        )
        } else if (misc_parameters$show_lines == "co_tr") {
                scale_color_manual_values <- c(
                        "black",
                        "blue"
                )
                scale_color_manual_breaks <- c(
                        "treated",
                        "control"
                )
                scale_color_manual_labels <- c(
                        "Treated average Y(1)",
                        "Control average"
                )
                plot_title <- glue(
                                "Treated and Control Emission Paths ({pollutant})" # nolint
                        )
        } else if (misc_parameters$show_lines == "tr") {
                scale_color_manual_values <- c(
                        "black"
                )
                scale_color_manual_breaks <- c(
                        "treated"
                )
                scale_color_manual_labels <- c(
                        "Treated average Y(1)"
                )
                plot_title <- glue(
                                "Treated Emission Paths ({pollutant})" # nolint
                        )
        }

        if (specification_choices$treatment_timing == "staggered") {

                # show_lines argument not implement for staggered version

                plot_data <- results$Y.bar %>%
                        as_tibble() %>%
                        mutate(
                                time = as.numeric(names(results$att)),
                                treated = results$Y.tr.cnt,
                                counterfactual = results$Y.ct.cnt
                        ) %>%
                        pivot_longer(
                                cols = c("treated", "counterfactual"),
                                names_to = "type",
                                values_to = "outcome"
                        ) %>%
                        select(time, type, outcome)

                x_lab_name <- "Deviation from treatment (years)"

        } else if (specification_choices$treatment_timing %in% c("common")) { # nolint

                # create a function for that later

                if (misc_parameters$show_lines == "ct_tr") {
                        plot_data <- results$Y.bar %>%
                                as_tibble() %>%
                                mutate(
                                        time =
                                                as.numeric(names(results$att))
                                ) %>%
                                pivot_longer(
                                        cols = c(
                                                "Y.tr.bar",
                                                "Y.ct.bar"
                                        ),
                                        names_to = "type",
                                        values_to = "outcome"
                                ) %>%
                                mutate(type = case_when(
                                        type == "Y.tr.bar" ~ "treated",
                                        type == "Y.ct.bar" ~ "counterfactual",
                                        TRUE ~ type
                                ))
                } else if (misc_parameters$show_lines == "co_ct_tr") {
                        plot_data <- results$Y.bar %>%
                                as_tibble() %>%
                                mutate(
                                        time =
                                                as.numeric(names(results$att))
                                ) %>%
                                pivot_longer(
                                        cols = c(
                                                "Y.tr.bar",
                                                "Y.ct.bar",
                                                "Y.co.bar"
                                        ),
                                        names_to = "type",
                                        values_to = "outcome"
                                ) %>%
                                mutate(type = case_when(
                                        type == "Y.tr.bar" ~ "treated",
                                        type == "Y.ct.bar" ~ "counterfactual",
                                        type == "Y.co.bar" ~ "control",
                                        TRUE ~ type
                                ))
                } else if (misc_parameters$show_lines == "co_tr") {
                        plot_data <- results$Y.bar %>%
                                as_tibble() %>%
                                mutate(
                                        time =
                                                as.numeric(names(results$att))
                                ) %>%
                                pivot_longer(
                                        cols = c(
                                                "Y.tr.bar",
                                                "Y.co.bar"
                                        ),
                                        names_to = "type",
                                        values_to = "outcome"
                                ) %>%
                                mutate(type = case_when(
                                        type == "Y.tr.bar" ~ "treated",
                                        type == "Y.co.bar" ~ "control",
                                        TRUE ~ type
                                ))
                } else if (misc_parameters$show_lines == "tr") {
                        plot_data <- results$Y.bar %>%
                                as_tibble() %>%
                                mutate(
                                        time =
                                                as.numeric(names(results$att))
                                ) %>%
                                pivot_longer(
                                        cols = c(
                                                "Y.tr.bar"
                                        ),
                                        names_to = "type",
                                        values_to = "outcome"
                                ) %>%
                                mutate(type = case_when(
                                        type == "Y.tr.bar" ~ "treated",
                                        TRUE ~ type
                                ))
                }

                plot_data %<>%
                        select(time, type, outcome)

                x_lab_name <- "Year"

        }

        x_values <- plot_data$time

        plot <- ggplot() +
                geom_line(
                        aes(
                                x = x_values,
                                y = plot_data$outcome,
                                color = plot_data$type
                        ),
                        linewidth = 1.2
                ) +
                scale_color_manual(
                        values = scale_color_manual_values,
                        name = "",
                        breaks = scale_color_manual_breaks,
                        labels = scale_color_manual_labels
                ) +
                labs(
                        x = x_lab_name,
                        y = "Log emissions",
                        title = plot_title,
                        subtitle = "Sample averages" # nolint
                ) +
                theme_bw() +
                theme(
                        plot.title = element_text(
                                size = 18,
                                face = "bold"
                        ),
                        plot.subtitle = element_text(size = 16),
                        axis.title = element_text(size = 14),
                        axis.text = element_text(size = 12),
                        plot.margin = unit(c(.5, .5, .5, .5), "cm"),
                        legend.text = element_text(size = 12),
                        legend.position = c(0.25, 0.17),
                        legend.background = element_rect("transparent"),
                        legend.key = element_rect(
                                colour = NA,
                                fill = NA
                        )
                )

        if (misc_parameters$annotate_plots == "detailed") {
                plot <- plot +
                        annotate(
                                "label",
                                x = max(x_values),
                                y = max(plot_data$outcome),
                                label = glue(
                                        "Pollutant: {specification_choices$pollutant}\n", # nolint
                                        "Sectors treated: {specification_choices$treat_sectors}; countries treated: {specification_choices$treat_countries}\n", # nolint
                                        "Sectors donor: {specification_choices$donor_sectors}; countries donor: {specification_choices$donor_countries}\n", # nolint
                                        "p.c. analysis: {specification_choices$per_capita_emissions}; GDP type: {specification_choices$gdp}\n", # nolint
                                        "Include Sector 1.A.2? {specification_choices$include_1a2}; Complete units only? {specification_choices$balanced_panel}\n", # nolint
                                        "ETS start year: {specification_choices$ets_start_year}; Treatment timing: {specification_choices$treatment_timing}\n", # nolint
                                        "Covariates: {specification_choices$covariates}; unit of analysis: {specification_choices$unit_of_analysis}\n", # nolint
                                        "ATT.avg (p-value): {att_avg} ({att_avg_p})" # nolint
                                ),
                                hjust = 1,
                                vjust = 1,
                                size = 3,
                                fill = "#d6d3d3",
                                label.padding = unit(0.5, "lines")
                        )
        } else if (misc_parameters$annotate_plots == "att") {
                plot <- plot +
                        annotate(
                                "label",
                                x = max(x_values),
                                y = max(plot_data$outcome),
                                label = glue(
                                        "ATT.avg (p-value): {att_avg} ({att_avg_p})" # nolint
                                ),
                                hjust = 1,
                                vjust = 1,
                                size = 3,
                                fill = "#d6d3d3",
                                label.padding = unit(0.5, "lines")
                        )
        }

        if (specification_choices$treatment_timing == "staggered") {
                plot <- plot +
                        geom_vline(
                                aes(xintercept = 0),
                                linewidth = 1.3,
                                linetype = "dashed"
                        ) +
                        scale_x_continuous(
                                breaks = seq(
                                        ceiling(abs(min(x_values) / 10)) * -10,
                                        ceiling(abs(max(x_values) / 10)) * 10,
                                        by = 5
                                )
                        )

        } else if (specification_choices$treatment_timing %in% c("common")) { # nolint
                plot <- plot +
                        geom_vline(aes(xintercept = 2005)) +
                        geom_vline(
                                aes(xintercept = specification_choices$ets_start_year), # nolint
                                linewidth = 1.3
                        ) +
                        scale_x_continuous(
                                breaks = seq(
                                        min(plot_data$time),
                                        max(plot_data$time),
                                        by = 5
                                )
                        )
        }

        plot
}



# save plot -------------------------------------------------------------------
functions[["save_plot"]] <- function(plot_name, # nolint
                                     file_format = "png",
                                     width_plot = 6.5,
                                     height_plot = 6) {

        name <- deparse(substitute(plot_name)) %>%
                str_remove("^plots\\$")

        # shorten names of specification choices so that it can be saved
        if (all(specification_choices$treat_sectors %in% names(sectors_list))) {
                ts <- specification_choices$treat_sectors %>%
                        str_remove("_sectors$")
        }

        if (all(specification_choices$treat_countries %in%
                names(countries_list))) { # nolint
                tc <- specification_choices$treat_countries %>%
                        str_remove("_countries$")
        } else {
                tc <- NULL
        }

        if (all(specification_choices$donor_sectors %in% names(sectors_list))) {
                ds <- specification_choices$donor_sectors %>%
                        str_remove("_sectors$")
        } else if (all(specification_choices$donor_sectors == "all_available")) { # nolint
                ds <- "all"
        } else if (all(specification_choices$donor_sectors == "same_as_treat")) { # nolint
                ds <- ts
        } else if (all(specification_choices$donor_sectors == "hybrid")) {
                ds <- "hyb"
        }

        if (all(specification_choices$donor_countries %in% names(countries_list))) { # nolint
                dc <- specification_choices$donor_countries %>%
                        str_remove("_countries$")
        } else if (all(specification_choices$donor_countries %in%
                c("same_as_treat"))) { # nolint
                dc <- tc
        }

        if (specification_choices$include_1a2 == "no") {
                i1a2 <- "no1a2"
        } else if (specification_choices$include_1a2 == "edgar_fully_treated") {
                i1a2 <- "1a2edtr"
        } else if (specification_choices$include_1a2 == "edgar_fully_control") {
                i1a2 <- "1a2edco"
        } else if (specification_choices$include_1a2 == "emep21/un") {
                i1a2 <- "1a2em21"
        } else if (specification_choices$include_1a2 == "emep23/un") {
                i1a2 <- "1a2em23"
        } else if (specification_choices$include_1a2 == "emep21+23/un") {
                i1a2 <- "1a2em2123"
        }

        if (specification_choices$include_bio == "yes") {
                bio <- "bio"
        } else if (specification_choices$include_bio == "no") {
                bio <- "nobio"
        }

        if (specification_choices$main_data == "edgar") {
                data <- "ed"
        } else if (specification_choices$main_data == "emep21/un") {
                data <- "em21"
        } else if (specification_choices$main_data == "emep23/un") {
                data <- "em23"
        }

        covariates <- str_replace_all(
                specification_choices$covariates,
                "elv.*",
                "elv"
        ) %>%
                functions$sort_collapse(., sep = "") %>%
                str_replace_all(
                        .,
                        c(
                                "population" = "pop",
                                "gdp_pc_constant" = "gpc",
                                "gdp_pc_current" = "gpc",
                                "renew_elec" = "ren",
                                "carbon_pricing_dummy" = "cp",
                                "standard" = "std"
                        )
                )


        if (gscm_analysis$inference == "nonparametric") {
                inference <- "np"
        } else if (gscm_analysis$inference == "parametric") {
                inference <- "p"
        }

        default_options <- all(c(
                specification_choices$per_capita_emissions %in% c(FALSE),
                specification_choices$per_capita_gdp %in% c(FALSE),
                specification_choices$gdp %in% c("constant"),
                specification_choices$gaps_in_years %in% c("interpolate"),
                specification_choices$damage_est_source %in%
                        c("uba_eu_27"),
                specification_choices$time_var_damages %in% c(FALSE),
                specification_choices$unit_of_analysis %in% c("country_treat"),
                specification_choices$treatment_timing %in% c("common"),
                specification_choices$ensure_common %in% c(TRUE),
                specification_choices$include_uk %in% c(TRUE),
                specification_choices$include_aviation %in% c("control"),
                specification_choices$balanced_panel %in% c(FALSE),
                is.numeric(gscm_analysis$MSPE),
                all(
                        specification_choices$treat_countries %in%
                                names(countries_list)
                ),
                all(
                        specification_choices$treat_sectors %in%
                                names(sectors_list)
                ),
                all(
                        specification_choices$donor_countries %in%
                                c(
                                        names(countries_list),
                                        "same_as_treat"
                                )
                ),
                all(
                        specification_choices$donor_sectors %in%
                                c(
                                        names(sectors_list),
                                        "same_as_treat",
                                        "all_available",
                                        "hybrid"
                                )
                ),
                specification_choices$min_t0 %in% c(7),
                specification_choices$estimator %in% c("ife"),
                specification_choices$criterion %in% c("mspe"),
                specification_choices$cv %in% c(TRUE),
                specification_choices$r %in% c(0),
                specification_choices$em %in% c(FALSE),
                specification_choices$alpha %in% c(0.05),
                specification_choices$leave_one_out %in% c(FALSE)
        ))

        if (default_options) {

                dir_name <- c(
                        "plots",
                        glue("{specification_choices$pollutant}"),
                        glue("trse-{ts}_trco-{tc}"),
                        glue("dose-{ds}_doco-{dc}"),
                        glue("{data}"),
                        glue("cov-{covariates}"),
                        glue("{i1a2}_{bio}")
                )

                gg_filename <- functions$here_vec(c(
                        dir_name,
                        # file name
                        glue(
                                "{specification_choices$year_first}_",
                                "{specification_choices$ets_start_year}_",
                                "{specification_choices$year_last}_",
                                "{inference}_",
                                "{name}.{file_format}"
                        )
                ))

                dir.create(functions$here_vec(c(
                        dir_name
                )), recursive = TRUE, showWarnings = FALSE)

                file.create(functions$here_vec(c(
                        dir_name,
                        # file name
                        glue("{misc_parameters$uuid}.txt")
                )))

        } else {

                dir_name <- c(
                        "plots",
                        "misc",
                        glue("{misc_parameters$uuid}")
                )

                dir.create(functions$here_vec(c(
                        dir_name
                )), recursive = TRUE, showWarnings = FALSE)

                gg_filename <- functions$here_vec(c(
                        dir_name,
                        # file name
                        glue(
                                "{specification_choices$year_first}_",
                                "{specification_choices$ets_start_year}_",
                                "{specification_choices$year_last}_",
                                "{inference}_",
                                "{name}.{file_format}"
                        )
                ))
        }

        ggsave(
                filename = gg_filename, # nolint
                plot = plot_name,
                width = width_plot,
                height = height_plot
        )
}