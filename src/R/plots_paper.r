# Load packages ---------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(glue)
library(here)
library(patchwork)
library(ggh4x)
library(viridis)
library(ggtext)

here()



# Figure 1 --------------------------------------------------------------------

# read plot data
# see 'master.r' for how the data was created # nolint
so2 <- read_csv(here(
        "plots",
        "data",
        glue("so2.csv")
))

pm25 <- read_csv(here(
        "plots",
        "data",
        glue("pm25.csv")
))

nox <- read_csv(here(
        "plots",
        "data",
        glue("nox.csv")
))

plot_data <- list(nox, pm25, so2) %>%
        reduce(
                .x = .,
                .f = full_join
        ) %>%
        arrange(time, type, pollutant) %>%
        mutate(
                pollutant = fct(
                        pollutant,
                        levels = c("so2", "pm25", "nox")
                ),
                type = fct(
                        type,
                        levels = c("counterfactual", "treated")
                )
        )

rm(so2, pm25, nox)

# Define values/titles/...
ets_start <- 2005

y_lab <- "Average treatment effect (ATT)"

att_legend_name <- "Mean estimate + 95% confidence interval: "

pollutant_labels <- c(
        so2 = "bold(SO[2])",
        pm25 = "bold(PM[2.5])",
        nox = "bold(NO[x])"
)

pollutant_labels_g <- c(
        expression(SO[2]),
        expression(PM[2.5]),
        expression(NO[x])
)

# Construct plot components
strip <- strip_themed(
        background_x = elem_list_rect(
                fill = c(
                        viridis(
                                n = nlevels(plot_data$pollutant),
                                alpha = 0.50
                        )[1],
                        viridis(
                                n = nlevels(plot_data$pollutant),
                                alpha = 0.75
                        )[2:3]
                )
        )
)

common_theme <- theme(
        axis.title = element_text(size = 14),
        axis.title.y = element_text(
                margin = margin(r = 10)
        ),
        axis.text = element_text(size = 12),
        panel.spacing.x = unit(1.5, "lines"),
        legend.position = "bottom"
)

# ATT plot (bottom panel of Fig. 1)
att <- ggplot(
        data = select(plot_data, -c(outcome, type)) %>% distinct(),
        aes(
                x = time,
                y = att
        )
) +
        geom_ribbon(
                aes(
                        ymin = att_low,
                        ymax = att_high,
                        fill = pollutant
                ),
                colour = "black",
                linewidth = 0.1
        ) +
        geom_line(
                aes(
                        color = pollutant
                ),
                linewidth = 1.5
        ) +
        geom_hline(
                aes(yintercept = 0),
                linetype = "dashed",
                linewidth = 0.7
        ) +
        geom_vline(
                aes(xintercept = 2005),
                linewidth = 0.7
        ) +
        geom_vline(
                aes(xintercept = ets_start),
                linewidth = 1
        ) +
        scale_color_viridis_d(
                name = att_legend_name,
                labels = pollutant_labels_g
        ) +
        scale_fill_viridis_d(
                name = att_legend_name,
                labels = pollutant_labels_g,
                alpha = 0.1
        ) +
        scale_x_continuous(
                name = "Year"
        ) +
        scale_y_continuous(
                name = y_lab,
                breaks = scales::breaks_width(20),
                labels = scales::label_percent(scale = 1)
        ) +
        facet_wrap2(
                facets = ~pollutant,
                labeller = as_labeller(pollutant_labels, label_parsed),
                strip = strip
        ) +
        theme_bw() +
        common_theme

# CT vs. TR plot (top panel of Fig. 1)
ct_tr <- ggplot(
        data = plot_data,
        aes(
                x = time,
                y = outcome,
                colour = type
        )
) +
        geom_line(
                linewidth = 1.2
        ) +
        geom_vline(
                aes(xintercept = 2005),
                linewidth = 0.7
        ) +
        geom_vline(
                aes(xintercept = ets_start),
                linewidth = 1
        ) +
        scale_x_continuous(
                name = "Year"
        ) +
        scale_y_continuous(
                name = "Average log(emissions)"
        ) +
        scale_color_manual(
                name = "Units: ",
                values = c(
                        "#E69F00",
                        "black"
                ),
                limits = c(
                        "counterfactual",
                        "treated"
                ),
                labels = c(
                        "Counterfactual average Y(0)",
                        "Treated average Y(1)"
                )
        ) +
        facet_wrap2(
                facets = ~pollutant,
                scales = "free_y",
                labeller = as_labeller(pollutant_labels, label_parsed),
                strip = strip
        ) +
        theme_bw() +
        common_theme +
        theme(
                legend.position = "bottom"
        )

# Merge plots in patchwork
fig1 <- (ct_tr) / (att) &
        plot_annotation(
                tag_levels = "a",
                tag_suffix = ")"
        ) &
        plot_layout() &
        # 88/609/EEC emissions limits (not plant level) amendment in 1994
        # LCPD 2001/80/EC starting in 2008
        # IED 2010/75/EU starting in 2013 (new plants) or 2016 (existing plants) # nolint
        geom_vline(
                xintercept = c(1994, 2008, 2016),
                linewidth = 0.3,
                linetype = "dashed"
        ) &
        theme(
                title = element_text(size = 16),
                legend.title = element_text(
                        size = 12,
                        hjust = 0
                ),
                legend.text = element_text(
                        size = 12,
                        hjust = 0
                ),
                plot.tag = element_text(
                        size = 16
                )
        )

fig1[[1]] <- fig1[[1]] +
        labs(
                title = "Average log(emissions) for treated and counterfactual units" # nolint
        ) +
        annotate(
                "text",
                x = c(1994.3),
                y = -Inf,
                label = glue("Changes to\nLCP standards"),
                hjust = "left",
                vjust = -2,
                size = 3
        ) +
        annotate(
                "text",
                x = 2004.7,
                y = Inf,
                label = glue("Start of the\nEU ETS"),
                hjust = "right",
                vjust = 2,
                size = 3
        ) +
        theme(
                plot.title = element_text(
                        size = 14
                ),
                panel.spacing.x = unit(0.5, "lines"),
                legend.margin = margin(t = 0, r = 50, b = 0, l = 0),
                strip.text = element_text(
                        size = 12
                )
        )

fig1[[2]] <- fig1[[2]] +
        labs(title = "Average treatment effect across treated sectors") +
        theme(
                plot.title = element_text(
                        size = 14
                ),
                strip.background = element_blank(),
                strip.text = element_blank()
        )

fig1

ggsave(
        here("plots", "paper", "Fig1", "fig1.pdf"), # change ending to .png if desired # nolint
        plot = fig1,
        width = 13,
        height = 8
)



# Figure 2 --------------------------------------------------------------------

# read plot data from results table
plot_data_all <- read_csv(
        here("results", "results_table.csv"),
        col_types = list(date_time = col_character())
) %>%
        select(where(~ n_distinct(.) > 1))

# Define plot limits, labels, ...
pollutant_lims <- c("so2", "pm25", "nox")

pollutant_labels <- c(
        expression(SO[2]),
        expression(PM[2.5]),
        expression(NO[x])
)

specification_labels <- c(
        "No covariates" = "No~covariates",
        "Main specification" = "bold(Main~specification)",
        "Additional policies" = "Additional~policies"
)

specification_lims <- c(
        "No covariates",
        "Main specification",
        "Additional policies"
)

# prepare specific plot data
base_spec <- function(data,
                      year_l = 2021,
                      main_d = "emep23/un",
                      aviation = "control",
                      ets_start_y = 2005,
                      treat_c = "eu25_countries",
                      covs = "log_gdp + log_gdp_2",
                      inf = "parametric",
                      est = "ife",
                      damage_source = "uba_eu_27",
                      sectors_treat = "ets_sectors") {
        data$year_last == year_l &
                data$main_data == main_d &
                data$include_aviation == aviation &
                data$ets_start_year == ets_start_y &
                data$treat_countries %in% c(treat_c) &
                data$covariates == covs &
                data$inference %in% c(inf) &
                data$estimator %in% c(est) &
                data$damage_est_source %in% damage_source &
                data$treat_sectors %in% sectors_treat
}

spec_chart_data <- plot_data_all %>%
        filter(
                case_when(
                        pollutant != "co2" &
                                year_last %in% c(2016, 2018, 2019, 2021) &
                                leave_one_out == FALSE &
                                ets_start_year %in% c(2005, 2008) &
                                ntr %in% c(19, 25) &
                                treat_countries %in% c(
                                        "eu25_countries",
                                        "sdid_countries"
                                ) ~ TRUE,
                        .default = FALSE
                )
        ) %>%
        select(where(~ n_distinct(.) > 1)) %>%
        mutate(specification = case_when(
                # Main specification
                base_spec(.) ~ "Main specification",
                # Specification with all additional covariates
                base_spec(.,
                        covs = "carbon_pricing_dummy + log_gdp + log_gdp_2 + log_gdp_pc_constant + log_population + log_renew_elec" # nolint
                ) ~ "All additional covariates",
                # Specification with gdp pc and population as covs
                base_spec(.,
                        covs = "log_gdp + log_gdp_2 + log_gdp_pc_constant + log_population" # nolint
                ) ~ "Additional population and GDP pc covariates",
                # Specification with policy covariates
                base_spec(.,
                        covs = "carbon_pricing_dummy + log_gdp + log_gdp_2 + log_renew_elec" # nolint
                ) ~ "Additional policies",
                # carbon pricing dummy
                base_spec(.,
                        covs = "carbon_pricing_dummy + log_gdp + log_gdp_2"
                ) ~ "Carbon pricing<br>dummy",
                # Renewable electricity production
                base_spec(.,
                        covs = "log_gdp + log_gdp_2 + log_renew_elec"
                ) ~ "Renewable electricity<br>production",
                # Retired coal capacity added to main
                base_spec(.,
                        covs = "log_gdp + log_gdp_2 + log_lcp_90_05_na" # nolint
                ) ~ "Main specification +<br>retired LCP capacity",
                # Fuel prices
                base_spec(.,
                        covs = "coal_price_weighted + gas_price_weighted + log_gdp + log_gdp_2 + oil_price_weighted" # nolint
                ) ~ "Fuel prices",
                # Population
                base_spec(.,
                        covs = "log_gdp + log_gdp_2 + log_population"
                ) ~ "Population",
                # GDP pc
                base_spec(.,
                        covs = "log_gdp + log_gdp_2 + log_gdp_pc_constant"
                ) ~ "GDP pc",
                # No covariates
                base_spec(.,
                        covs = "none"
                ) ~ "No covariates",
                # ETS start in 2008
                base_spec(.,
                        ets_start_y = 2008
                ) ~ "ETS start in 2008",
                # MC estimator
                base_spec(.,
                        inf = "nonparametric",
                        est = "mc"
                ) ~ "Matrix completion",
                # GSCM with SDID sample; ending in 2019; incl. UK
                base_spec(.,
                        treat_c = "sdid_countries",
                        year_l = 2019
                ) ~ "GSCM with<br>SDID sample",
                # SDID analysis; ending in 2019; incl. UK
                base_spec(.,
                        treat_c = "sdid_countries",
                        est = "sdid",
                        year_l = 2019
                ) ~ "SDID analysis",
                # analysis ending in 2018 and emep
                base_spec(.,
                        year_l = 2018
                ) ~ "Analysis ending in 2018:<br>EMEP data",
                # analysis ending in 2018 and edgar
                base_spec(.,
                        year_l = 2018,
                        main_d = "edgar"
                ) ~ "Analysis ending in 2018:<br>EDGAR data",
                # analysis ending in 2019 and emep
                base_spec(.,
                        year_l = 2019
                ) ~ "Analysis ending in 2019",
                # analysis ending in 2016 and emep
                base_spec(.,
                        year_l = 2016
                ) ~ "Analysis ending in 2016",
                # Aviation excluded from sample
                base_spec(.,
                        aviation = "exclude"
                ) ~ "Aviation excluded",
                .default = NA
        )) %>%
        filter(specification %in% specification_lims) %>%
        group_by(specification) %>%
        mutate(
                pollutant = fct(
                        pollutant,
                        levels = pollutant_lims
                ),
                sum_eff_mon = sum(eff_mon)
        ) %>%
        ungroup() %>%
        mutate(
                specification = fct(
                        specification,
                        levels = specification_lims
                )
        )

# diagnostics: should only be 3 per specification
spec_chart_data %>%
        count(specification)

# Import back-of-the-envelope data; created in Stata_SDID/r_scripts/standards_back_of_the_envelope.r # nolint
standards <- read_csv(here(
        "Stata_SDID",
        "data_out",
        "standards_back_of_the_envelope.csv"
)) %>%
        select(
                spec_id,
                residual_health
        )

spec_chart_data <- full_join(
        x = spec_chart_data,
        y = standards
) %>%
        pivot_longer(
                cols = c(eff_mon, residual_health),
                names_to = "estimation_type",
                values_to = "eff_mon"
        ) %>%
        mutate(
                estimation_type = case_when(
                        estimation_type == "eff_mon" ~
                                "Joint",
                        estimation_type == "residual_health" ~
                                "Bounded",
                        .default = NA
                ),
                estimation_type = fct(
                        estimation_type,
                        levels = c(
                                "Joint",
                                "Bounded"
                        )
                )
        )

# Build plot components
spec_chart_base <- ggplot(
        data = spec_chart_data,
        aes(
                x = estimation_type
        )
) +
        geom_hline(
                yintercept = 0,
                linetype = "dotted"
        ) +
        labs(
                x = "Different specifications",
                y = "Average effect across treated sectors"
        ) +
        scale_y_continuous(
                labels = scales::label_percent(scale = 1),
                breaks = scales::breaks_width(10)
        ) +
        theme_bw() +
        theme(
                plot.title = element_text(
                        size = 16
                ),
                plot.subtitle = element_text(
                        size = 14
                ),
                axis.text = element_text(
                        size = 12
                ),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_text(
                        size = 14,
                        margin = margin(t = 10)
                ),
                axis.title.y = element_text(
                        size = 14,
                        margin = margin(r = 10)
                ),
                legend.text = element_text(
                        hjust = 0
                ),
                legend.position = "bottom",
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.spacing = unit(0, "lines"),
                strip.background = element_rect(color = NA, fill = NA),
                strip.placement = "outside",
                strip.text = element_text(
                        size = 12,
                        margin = margin(t = 5, b = 5)
                )
        )

# Upper panel of Fig. 2
col_chart <- spec_chart_base +
        geom_col(
                mapping = aes(
                        y = att_avg,
                        fill = pollutant
                ),
                data = spec_chart_data %>%
                        filter(
                                estimation_type == "Joint"
                        ),
                position = position_dodge(),
                width = 0.5,
                linewidth = 0.1,
                colour = "black"
        ) +
        geom_errorbar(
                aes(
                        ymin = att_low,
                        ymax = att_high,
                        group = pollutant
                ),
                position = position_dodge(width = 0.5),
                width = 0.25,
                linewidth = 0.5
        ) +
        scale_fill_viridis_d(
                name = "Pollutant:",
                limits = pollutant_lims,
                labels = pollutant_labels,
                alpha = 0.7
        ) +
        scale_x_discrete(
                limits = c("Joint")
        ) +
        facet_wrap2(
                facets = ~specification,
                strip.position = "bottom",
                labeller = as_labeller(specification_labels, label_parsed)
        )

# Lower panel of Fig. 2
mon <- spec_chart_base +
        geom_col(
                aes(
                        y = eff_mon,
                        fill = pollutant
                ),
                width = 0.75,
                linewidth = 0.1,
                colour = "black"
        ) +
        labs(
                x = "Different specifications",
                y = "Monetized Effect"
        ) +
        scale_x_discrete(
                expand = expansion(mult = c(1))
        ) +
        scale_y_continuous(
                labels = scales::label_dollar(
                        prefix = "€",
                        suffix = " bn"
                ),
                minor_breaks = NULL,
                expand = expansion(add = c(0, 20))
        ) +
        scale_fill_viridis_d(
                name = "Pollutant:",
                limits = pollutant_lims,
                labels = pollutant_labels,
                alpha = 0.7
        ) +
        facet_wrap2(
                facets = ~specification,
                strip.position = "bottom",
                labeller = as_labeller(specification_labels, label_parsed)
        ) +
        theme_bw() +
        theme(
                axis.title.y = element_text(
                        size = 14,
                        margin = margin(r = 10)
                ),
                legend.text = element_text(
                        hjust = 0
                ),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.spacing = unit(0, "lines"),
                strip.background = element_rect(color = NA, fill = NA),
                strip.placement = "outside",
                strip.text = element_text(
                        size = 12,
                        margin = margin(t = 5, b = 5)
                )
        )

# Merge plots in patchwork
fig2 <- (col_chart / mon) +
        plot_layout(
                guides = "collect",
                heights = c(3, 1)
        ) +
        plot_annotation(
                tag_levels = "a",
                tag_suffix = ")"
        ) +
        theme(
                axis.text.x = element_markdown(
                        size = 14
                ),
                plot.caption = element_markdown(
                        hjust = 0
                ),
                strip.text = element_text(
                        size = 16
                )
        ) &
        theme(
                plot.tag = element_text(
                        margin = margin(r = 10),
                        size = 16
                ),
                legend.position = "bottom",
                legend.text = element_text(
                        size = 14
                ),
                legend.title = element_text(
                        size = 14
                ),
                legend.spacing.x = unit(0.2, "cm"),
                panel.border = element_rect(
                        linewidth = 0.25
                ),
                axis.title.y = element_text(
                        size = 16
                ),
                axis.text.y = element_text(
                        size = 14
                ),
                axis.ticks = element_line(
                        linewidth = 0.75
                )
        ) &
        labs(
                x = NULL
        )

fig2[[1]] <- fig2[[1]] +
        scale_y_continuous(
                labels = scales::label_percent(scale = 1),
                breaks = scales::breaks_width(10)
        ) +
        labs(
                title = NULL,
                subtitle = NULL
        ) +
        theme(
                axis.title.x = element_blank(),
                strip.text = element_blank()
        )

fig2

ggsave(
        filename = here(
                "plots",
                "paper",
                "Fig2",
                "fig2.pdf"
        ),
        plot = fig2,
        width = 10,
        height = 7
)