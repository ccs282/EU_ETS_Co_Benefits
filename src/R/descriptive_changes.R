library(tidyverse)
library(magrittr)
library(glue)
library(gsynth)
library(panelView)
library(countrycode)
library(here)
library(uuid)
library(patchwork)
library(ggh4x)
library(viridis)
library(ggtext)

here()

source(here("src", "R", "functions.r"))



# Calculate percentage changes from 1990 to 2021 ------------------------------
path_common <- c(
        "data",
        "main_specification"
)

# read in data from the main specification
so2 <- read_csv(functions$here_vec(c(
        path_common,
        "so2_gscm_data.csv"
))) %>%
        mutate(pollutant = "so2")

pm25 <- read_csv(functions$here_vec(c(
        path_common,
        "pm25_gscm_data.csv"
))) %>%
        mutate(pollutant = "pm25")

nox <- read_csv(functions$here_vec(c(
        path_common,
        "nox_gscm_data.csv"
))) %>%
        mutate(pollutant = "nox")

descriptive_changes_90_21 <- list(so2, pm25, nox) %>%
        reduce(full_join) %>%
        mutate(emissions = emissions / 1e6) %>%
        mutate(
                sum_emissions_y = sum(emissions),
                .by = c("pollutant", "year")
        ) %>%
        mutate(
                sum_emissions_y_reg = sum(emissions),
                .by = c("pollutant", "year", "ets_coverage")
        ) %>%
        distinct(
                pollutant,
                year,
                ets_coverage,
                sum_emissions_y,
                sum_emissions_y_reg
        ) %>%
        filter(
                year %in% c(1990, 2021),
                ets_coverage %in% c("Regulated")
        ) %>%
        select(-sum_emissions_y) %>%
        pivot_wider(
                names_from = year,
                names_prefix = "y_",
                values_from = sum_emissions_y_reg
        ) %>%
        mutate(
                percentage_change_90_21 = (1 - y_2021 / y_1990) * 100,
                .by = c("pollutant")
        )

rm(so2, pm25, nox)



# Plot absolute and relative changes from 1990 to 2021 ------------------------

retrieve_desc_data <- function(pollutant) {
        execute_analysis(
                pollutant = pollutant,
                save_plots = FALSE,
                conduct_analysis = FALSE,
                write_results_table = FALSE
        )

        plots$data_descriptive %>%
                mutate(
                        emissions_s_y = sum(emissions),
                        .by = c("sector_code", "year")
                ) %>%
                distinct(
                        year,
                        sector_code,
                        emissions_s_y
                ) %>%
                mutate(
                        emissions_y = sum(emissions_s_y),
                        .by = c("year")
                ) %>%
                mutate(sector_class = case_when(
                        sector_code %in% sectors_list$non_ets_sectors ~
                                "Non-ETS",
                        sector_code %in% sectors_list$energy_sectors ~
                                "Energy",
                        sector_code %in% sectors_list$metals_sectors ~
                                "Metals",
                        sector_code %in% sectors_list$minerals_sectors ~
                                "Minerals",
                        sector_code %in% sectors_list$chemicals_sectors ~
                                "Chemicals",
                        sector_code %in% sectors_list$paper_sectors ~
                                "Paper",
                        .default = NA
                )) %>%
                mutate(
                        # in millions t
                        emissions_cat_y = sum(emissions_s_y) / 1e6,
                        .by = c("sector_class", "year")
                ) %>%
                mutate(pollutant = specification_choices$pollutant) %>%
                distinct(
                        pollutant,
                        year,
                        sector_class,
                        emissions_cat_y
                )
}

descriptive_plot <- map(
        c("so2", "pm25", "nox"),
        retrieve_desc_data
) %>%
        reduce(full_join) %>%
        mutate(pollutant = fct(pollutant)) %>%
        mutate(
                emissions_tot_y = sum(emissions_cat_y),
                .by = c("pollutant", "year")
        ) %>%
        mutate(emissions_cat_share_y = emissions_cat_y / emissions_tot_y)


common_theme <- theme(
        axis.title = element_text(size = 14),
        axis.title.y = element_text(
                margin = margin(r = 10)
        ),
        axis.text = element_text(size = 12),
        panel.spacing.x = unit(0.5, "lines"),
        legend.position = "bottom",
        plot.title = element_text(
                size = 14
        ),
        strip.text = element_text(
                size = 12
        )
)

sector_class_breaks <- c(
        "Non-ETS",
        "Energy",
        "Chemicals",
        "Metals",
        "Minerals",
        "Paper"
)

color_vector <- c(
        "#1b9e77",
        "#d95f02",
        "#7570b3",
        "#e7298a",
        "#66a61e",
        "#e6ab02"
)

pollutant_labels <- c(
        so2 = "bold(SO[2])",
        pm25 = "bold(PM[2.5])",
        nox = "bold(NO[x])"
)

strip <- strip_themed(
        background_x = elem_list_rect(
                fill = c(
                        viridis(
                                n = nlevels(descriptive_plot$pollutant),
                                alpha = 0.50
                        )[1],
                        viridis(
                                n = nlevels(descriptive_plot$pollutant),
                                alpha = 0.75
                        )[2:3]
                )
        )
)

plot_common <- ggplot(
        data = descriptive_plot
) +
        geom_vline(
                aes(xintercept = 2005),
                linewidth = 0.5
        ) +
        scale_colour_manual(
                name = "Sectors",
                breaks = sector_class_breaks,
                values = color_vector
        ) +
        scale_linetype_discrete(
                name = "Sectors",
                breaks = sector_class_breaks
        ) +
        theme_bw() +
        common_theme

plot_abs <- plot_common +
        geom_line(
                aes(
                        x = year,
                        y = emissions_cat_y,
                        colour = sector_class,
                        linetype = sector_class
                ),
                linewidth = 0.5
        ) +
        labs(
                x = "Year",
                y = "Annual emissions (millions t)"
        ) +
        facet_wrap2(
                facets = ~pollutant,
                scales = "free_y",
                labeller = as_labeller(pollutant_labels, label_parsed),
                strip = strip
        ) +
        theme_bw() +
        common_theme

plot_abs

ggsave(
        filename = here(
                "plots",
                "descriptive",
                glue("descriptive_abs.pdf")
        ),
        plot = plot_abs,
        width = 15,
        height = 7
)

plot_rel <- plot_common +
        geom_line(
                aes(
                        x = year,
                        y = emissions_cat_share_y,
                        colour = sector_class,
                        linetype = sector_class
                ),
                linewidth = 0.5
        ) +
        geom_vline(
                aes(xintercept = 2005),
                linewidth = 0.5
        ) +
        labs(
                x = "Year",
                y = "Share of total emissions"
        ) +
        scale_y_continuous(
                labels = scales::percent_format(
                ),
                breaks = scales::breaks_width(0.1)
        ) +
        facet_wrap2(
                facets = ~pollutant,
                labeller = as_labeller(pollutant_labels, label_parsed),
                strip = strip
        ) +
        theme_bw() +
        common_theme

plot_rel

ggsave(
        filename = here(
                "plots",
                "descriptive",
                glue("descriptive_rel.pdf")
        ),
        plot = plot,
        width = 15,
        height = 7
)