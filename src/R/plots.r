# Prep data used for ATT plot -------------------------------------------------
if (specification_choices$treatment_timing == "staggered") {
        x_values <- names(gscm_analysis$att) %>%
                as.numeric()
} else if (specification_choices$treatment_timing %in% c("common")) { # nolint
        x_values <- gscm_analysis$time %>%
                as.numeric()
}

plots_data_att <- tibble(
        time = x_values,
        att = functions$interpret_log_linear(gscm_analysis$est.att[, 1]),
        att_low =
                functions$interpret_log_linear(gscm_analysis$est.att[, 3]),
        att_high =
                functions$interpret_log_linear(gscm_analysis$est.att[, 4])
)



# Prep data used for CT vs. TR plot -------------------------------------------
if (specification_choices$treatment_timing == "staggered") {

        plots_data_ct_tr <- gscm_analysis$Y.bar %>%
                as_tibble() %>%
                mutate(
                        time = as.numeric(names(gscm_analysis$att)),
                        treated = gscm_analysis$Y.tr.cnt,
                        counterfactual = gscm_analysis$Y.ct.cnt
                ) %>%
                pivot_longer(
                        cols = c("treated", "counterfactual"),
                        names_to = "type",
                        values_to = "outcome"
                ) %>%
                select(time, type, outcome)

} else if (specification_choices$treatment_timing %in% c("common")) { # nolint

        plots_data_ct_tr <- gscm_analysis$Y.bar %>%
                as_tibble() %>%
                mutate(
                        time =
                                as.numeric(names(gscm_analysis$att))
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
                )) %>%
                select(time, type, outcome)

}

plots$data <- full_join(
        x = plots_data_ct_tr,
        y = plots_data_att,
        by = "time"
) %>%
        mutate(pollutant = specification_choices$pollutant)

rm(x_values, plots_data_ct_tr, plots_data_att)



# get a visual overview of the completeness of the data -----------------------
plots$panelview <- panelview(
        formula = misc_parameters$formula,
        data = gscm,
        index = c("unit_id", "year"),
        pre.post = TRUE,
        by.group = TRUE,
        by.timing = TRUE,
        axis.lab.angle = 90,
        xlab = "Year",
        ylab = "Units",
        axis.lab.gap = c(4, 0),
        #color = c("#999999", "#E69F00", "#56B4E9", "#0072B2"),
        cex.axis = 7,
        cex.legend = 7,
        cex.lab = 10
)



# Plot difference figure ------------------------------------------------------
plots$att <- functions$plot_results_att(gscm_analysis)



# Plot counterfactual vs. treated figure --------------------------------------
plots$ct_tr <- functions$plot_results_ct_tr(gscm_analysis)



# save plots ------------------------------------------------------------------
if (misc_parameters$save_plots) {
        functions$save_plot(plots$panelview)
        functions$save_plot(plots$att)
        functions$save_plot(plots$ct_tr)
}