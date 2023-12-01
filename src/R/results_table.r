# Transform log values to absolute emissions ----------------------------------
tr <- functions$reshape_tr_ct_co(
        data = gscm_analysis$Y.tr,
        type = "tr"
)

ct <- functions$reshape_tr_ct_co(
        data = gscm_analysis$Y.ct,
        type = "ct"
)

co <- functions$reshape_tr_ct_co(
        data = gscm_analysis$Y.co,
        type = "co"
)

if (!is.null(gscm_analysis$est.ind)) {
        eff_ind <- gscm_analysis$est.ind %>%
                as_tibble() %>%
                select(-contains(c("S.E.", "p.value"))) %>%
                mutate(year = gscm_analysis$time, .before = 1) %>%
                rename_with(
                        .cols = everything(),
                        .fn = ~ str_remove(., pattern = "_.*")
                ) %>%
                rename_with(
                        .cols = everything(),
                        .fn = ~ str_replace_all(., c(
                                "Eff." = "Eff_",
                                "CI.lower." = "CI.lower_",
                                "CI.upper." = "CI.upper_"
                        ))
                ) %>%
                pivot_longer(
                        cols = -year,
                        names_to = c(".value", "country"),
                        names_pattern = "(.*)_(.*)"
                )

        abs_emissions_c_y <- list(tr, ct, co, eff_ind) %>%
                reduce(
                        full_join,
                        by = c("year", "country")
                )
} else {
        abs_emissions_c_y <- list(tr, ct, co) %>%
                reduce(
                        full_join,
                        by = c("year", "country")
                ) %>%
                mutate(
                        Eff = log_emissions_tr - log_emissions_ct,
                        CI.lower = NA,
                        CI.upper = NA
                )
}

# _c_y means country/year
abs_emissions_c_y %<>%
        filter(if_all(
                .cols = c(
                        year,
                        country,
                        log_emissions_tr,
                        log_emissions_ct,
                        Eff
                ),
                .fns = ~ !is.na(.)
        )) %>%
        arrange(country, year) %>%
        select(-c(Eff, CI.lower, CI.upper)) %>%
        # abs unregulated emissions country/year
        mutate(
                emissions_co_c_y = exp(log_emissions_co),
                emissions_tr_c_y = exp(log_emissions_tr),
                emissions_tot_c_y =
                        exp(log_emissions_co) + exp(log_emissions_tr),
                eff_c_y =
                        exp(log_emissions_tr) - exp(log_emissions_ct),
                .keep = "unused"
        )

if (gscm_analysis$sameT0) {
        abs_emissions_c_y %<>%
                filter(year >= specification_choices$ets_start_year)
} else {
        abs_emissions_c_y %<>%
                filter(year >= 0)
}

# get effects per country, summed across years
abs_emissions_c <- functions$aggregate_abs_emissions(
        agg_by = c("country"),
        ending = "c",
        drop_var = "year"
)

# get effects per year, summed across countries
abs_emissions_y <- functions$aggregate_abs_emissions(
        agg_by = c("year"),
        ending = "y",
        drop_var = "country"
)

# get effects averaged across countries and years
abs_emissions <- abs_emissions_c %>%
        summarise(
                eff = sum(eff_c),
                emissions_tr = sum(emissions_tr_c),
                emissions_tot = sum(emissions_tot_c)
        ) %>%
        mutate(
                eff_per_tr = round(eff / emissions_tr, 3) * 100,
                eff_per_tot = round(eff / emissions_tot, 3) * 100
        )


rm(co, ct, tr, eff_ind)



# Prep columns of results csv  ------------------------------------------------

# Specification choices
misc_parameters$results_tab_spec <- list(
        # Specification
        pollutant = specification_choices$pollutant,
        per_capita_emissions =
                specification_choices$per_capita_emissions,
        per_capita_gdp =
                specification_choices$per_capita_gdp,
        gdp = specification_choices$gdp,
        year_first = min(gscm$year),
        year_last = max(gscm$year),
        main_data = specification_choices$main_data,
        include_1a2 = specification_choices$include_1a2,
        include_bio = specification_choices$include_bio,
        include_uk = specification_choices$include_uk,
        include_aviation = specification_choices$include_aviation,
        balanced_panel = specification_choices$balanced_panel,
        gaps_in_years = specification_choices$gaps_in_years,
        damage_est_source = specification_choices$damage_est_source,
        scp = misc_parameters$scp,
        time_var_damages = specification_choices$time_var_damages,
        ets_start_year = specification_choices$ets_start_year,
        treatment_timing = specification_choices$treatment_timing,
        ensure_common = specification_choices$ensure_common,
        unit_of_analysis = specification_choices$unit_of_analysis,
        donor_countries =
                functions$sort_collapse(
                        specification_choices$donor_countries
                ),
        donor_sectors = functions$sort_collapse(
                specification_choices$donor_sectors
        ),
        treat_countries = functions$sort_collapse(
                specification_choices$treat_countries
        ),
        treat_sectors = functions$sort_collapse(
                specification_choices$treat_sectors
        ),
        leave_one_out = specification_choices$leave_one_out,
        country_to_leave_out = specification_choices$country_to_leave_out,
        covariates = ifelse(
                is.null(gscm_analysis$X),
                "none",
                functions$sort_collapse(
                        gscm_analysis$X
                )
        ),
        min_t0 = specification_choices$min_t0,
        inference = gscm_analysis$inference,
        alpha = specification_choices$alpha,
        estimator = specification_choices$estimator,
        cv = specification_choices$cv,
        r = specification_choices$r,
        em = specification_choices$em,
        criterion = specification_choices$criterion,
        n_problematic_units = ifelse(
                !specification_choices$balanced_panel &
                        specification_choices$unit_of_analysis == "country_treat", # nolint
                misc_parameters$n_problematic_units,
                NA
        ),
        y = gscm_analysis$Y,
        d = gscm_analysis$D,
        samet0 = gscm_analysis$sameT0,
        n_t = gscm_analysis$T,
        n_t_post = length(abs_emissions_y$year),
        index = functions$sort_collapse(gscm_analysis$index),
        n = gscm_analysis$N,
        ntr = gscm_analysis$Ntr,
        nco = gscm_analysis$Nco,
        id_tr = functions$sort_collapse(str_remove(gscm_analysis$id.tr, "_.*")),
        id_co = functions$sort_collapse(str_remove(gscm_analysis$id.co, "_.*")),
        n_removed = gscm_analysis$removed.id %>% length(),
        r_cv = gscm_analysis$r.cv,
        sigma2 = gscm_analysis$sigma2,
        ic = gscm_analysis$IC,
        pc = gscm_analysis$PC,
        mspe = gscm_analysis$MSPE,
        spec_id = misc_parameters$uuid,

        # Date and time
        date_time = ymd_hms(
                Sys.time(),
                tz = Sys.timezone(location = TRUE)
        ) %>% with_tz(tzone = "Europe/Berlin") %>% as.character() # nolint
)

# results one-dimensional
misc_parameters$results_tab <- list(
        # Results ATT
        att_avg =
                functions$interpret_log_linear(
                        gscm_analysis$est.avg[, 1]
                ),
        att_low =
                functions$interpret_log_linear(
                        gscm_analysis$est.avg[, 3]
                ),
        att_high =
                functions$interpret_log_linear(
                        gscm_analysis$est.avg[, 4]
                ),
        att_p = gscm_analysis$est.avg[, 5],

        # Effects as un-logged emission changes
        eff = abs_emissions$eff,
        eff_per_tr = abs_emissions$eff_per_tr,
        eff_per_tot = abs_emissions$eff_per_tot,
        emissions_tr = abs_emissions$emissions_tr,
        emissions_tot = abs_emissions$emissions_tot
)

# results where length is equal to number of years
misc_parameters$results_tab_y <- list(
        year_y = gscm_analysis$time,
        y_tr_avg_y = gscm_analysis$Y.bar[, 1], # still in log form
        y_ct_avg_y = gscm_analysis$Y.bar[, 2], # still in log form
        att_avg_y = functions$interpret_log_linear(gscm_analysis$att),
        eff_y = functions$lengthen_eff_vector("eff_y")
)

# results where length is equal to number of countries
# values averaged from ets_start_year to year_last
misc_parameters$results_tab_c <- list(
        country = pull(abs_emissions_c, country),
        eff_c = abs_emissions_c$eff_c,
        eff_per_tr_c = abs_emissions_c$eff_per_tr_c,
        eff_per_tot_c = abs_emissions_c$eff_per_tot_c,
        emissions_tr_c = abs_emissions_c$emissions_tr_c,
        emissions_tot_c = abs_emissions_c$emissions_tot_c
)



# create results tables -------------------------------------------------------
if (all(file.exists(c(
        here("results", "results_table_y.csv"),
        here("results", "results_table.csv"),
        here("results", "results_table_c.csv")
)))) {
        results_table_y <- read_csv(
                here("results", "results_table_y.csv"),
                col_types = list(date_time = col_character())
        )

        new_row_y <- tibble(!!!c(
                misc_parameters$results_tab_spec,
                misc_parameters$results_tab,
                misc_parameters$results_tab_y
        )) %>%
                mutate(
                        att_avg_ci_low_high =
                                glue(
                                        "{round(att_avg, 2)} ",
                                        "[{round(att_low, 2)}, ",
                                        "{round(att_high, 2)}]"
                                ),
                        .after = att_p
                ) %>%
                functions$tons_to_billions_eur()

        results_table_y %<>%
                add_row(new_row_y) %>%
                relocate(c(spec_id, date_time), .after = last_col()) %>%
                distinct(pick(c(pollutant:r_cv, year_y)), .keep_all = TRUE) %>%
                arrange(pollutant, main_data, ets_start_year, spec_id)

        results_table <- results_table_y %>%
                select(-ends_with("_y")) %>%
                distinct(pick(c(pollutant:r_cv)), .keep_all = TRUE)

        results_table_c <- read_csv(
                here("results", "results_table_c.csv"),
                col_types = list(date_time = col_character())
        )

        new_row_c <- tibble(!!!c(
                misc_parameters$results_tab_spec,
                misc_parameters$results_tab_c
        )) %>%
                functions$tons_to_billions_eur()

        results_table_c %<>%
                add_row(new_row_c) %>%
                relocate(c(spec_id, date_time), .after = last_col()) %>%
                distinct(pick(c(pollutant:r_cv, country)), .keep_all = TRUE) %>%
                arrange(pollutant, main_data, ets_start_year, spec_id)

        if (misc_parameters$write_results_table == TRUE) {
                write_csv(
                        results_table_y,
                        here("results", "results_table_y.csv")
                )

                write_csv(
                        results_table,
                        here("results", "results_table.csv")
                )

                write_csv(
                        results_table_c,
                        here("results", "results_table_c.csv")
                )
        }

} else {
        # create table for the first time
        results_table_y <- tibble(!!!c(
                misc_parameters$results_tab_spec,
                misc_parameters$results_tab,
                misc_parameters$results_tab_y
        )) %>%
                mutate(
                        att_avg_ci_low_high =
                                glue(
                                        "{round(att_avg, 2)} ",
                                        "[{round(att_low, 2)}, ",
                                        "{round(att_high, 2)}]"
                                ),
                        .after = att_p
                ) %>%
                relocate(c(spec_id, date_time), .after = last_col()) %>%
                functions$tons_to_billions_eur() %>%
                distinct(pick(c(pollutant:r_cv, year_y)), .keep_all = TRUE) %>%
                arrange(pollutant, main_data, ets_start_year)

        results_table <- results_table_y %>%
                select(-ends_with("_y")) %>%
                distinct(pick(c(pollutant:r_cv)), .keep_all = TRUE)

        results_table_c <- tibble(!!!c(
                misc_parameters$results_tab_spec,
                misc_parameters$results_tab_c
        )) %>%
                relocate(c(spec_id, date_time), .after = last_col()) %>%
                functions$tons_to_billions_eur() %>%
                distinct(pick(c(pollutant:r_cv, country)), .keep_all = TRUE) %>%
                arrange(pollutant, main_data, ets_start_year)

        if (misc_parameters$write_results_table == TRUE) {
                write_csv(
                        results_table_y,
                        here("results", "results_table_y.csv")
                )

                write_csv(
                        results_table,
                        here("results", "results_table.csv")
                )

                write_csv(
                        results_table_c,
                        here("results", "results_table_c.csv")
                )
        }
}

rm(list = ls(pattern = "(^abs_emissions_)|(new_row)|(results_table_)"))