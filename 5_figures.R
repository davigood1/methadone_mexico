source("R/common.R")
source("R/reference_data.R")
Sys.setenv(XDG_CACHE_HOME = normalizePath("outputs/cache"))

analytic <- read_prepared()$analytic
monthly_formulation <- readr::read_csv(
  "outputs/tables/monthly_formulation.csv", show_col_types = FALSE
) %>% mutate(month = as.Date(month))
national_population <- readr::read_csv(
  "outputs/tables/population_scenarios_monthly.csv", show_col_types = FALSE
) %>% mutate(month = as.Date(month))

save_figure <- function(plot, name, width, height) {
  stem <- file.path("outputs/figures", name)
  ggplot2::ggsave(paste0(stem, ".pdf"), plot, width = width, height = height,
                  device = grDevices::cairo_pdf, bg = "white")
  ggplot2::ggsave(paste0(stem, ".png"), plot, width = width, height = height,
                  dpi = 300, bg = "white")
}

formulation_colours <- c("Oral solution" = "#0072B2", "Tablet" = "#D55E00")
figure_1 <- monthly_formulation %>%
  mutate(formulation = factor(formulation, levels = names(formulation_colours))) %>%
  ggplot(aes(month, total_mg / 1e6, fill = formulation)) +
  annotate("rect", xmin = suspension_start, xmax = reopening_start,
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = .25) +
  geom_col(width = 25) +
  geom_vline(xintercept = c(suspension_start, reopening_start),
             linetype = "dashed", linewidth = .4) +
  scale_fill_manual(values = formulation_colours) +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month",
               date_labels = "%Y", expand = expansion(mult = c(.01, .01))) +
  scale_y_continuous(expand = expansion(mult = c(0, .08))) +
  coord_cartesian(xlim = c(study_start, as.Date("2025-09-01"))) +
  labs(x = NULL, y = "Monthly methadone volume (million mg)",
       fill = "Formulation",
       caption = "2025 includes January through August.") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_line(colour = "grey75"),
    panel.grid.minor.x = element_line(colour = "grey91", linewidth = .25),
    legend.position = "bottom",
    plot.margin = margin(18, 12, 8, 18)
  )
save_figure(figure_1, "Figure_1", 9, 5.4)

annual_state <- analytic %>%
  filter(!is.na(state)) %>%
  mutate(year = year(capture_date)) %>%
  group_by(year, state) %>%
  summarise(total_mg = sum(total_mg), .groups = "drop")
top_states <- annual_state %>%
  group_by(state) %>%
  summarise(total_mg = sum(total_mg), .groups = "drop") %>%
  slice_max(total_mg, n = 5, with_ties = FALSE) %>%
  pull(state)
annual_plot <- annual_state %>%
  mutate(state_group = if_else(state %in% top_states, state, "Other states")) %>%
  group_by(year, state_group) %>%
  summarise(total_mg = sum(total_mg), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = total_mg / sum(total_mg)) %>%
  ungroup()
state_colours <- c(
  "Baja California" = "#0072B2",
  "Ciudad de México" = "#009E73",
  "Jalisco" = "#CC79A7",
  "Puebla" = "#E69F00",
  "Sonora" = "#56B4E9",
  "Other states" = "#999999"
)
state_order <- names(state_colours)
annual_plot <- annual_plot %>%
  mutate(state_group = factor(state_group, levels = state_order))
figure_2a <- ggplot(annual_plot, aes(factor(year), total_mg / 1e6,
                                    fill = state_group)) +
  geom_col() +
  scale_fill_manual(values = state_colours, drop = FALSE) +
  labs(title = "a", x = NULL, y = "Annual volume (million mg)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"))
figure_2b <- ggplot(annual_plot, aes(factor(year), share, fill = state_group)) +
  geom_col() +
  scale_fill_manual(values = state_colours, drop = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "b", x = "Year", y = "Share of annual volume", fill = "State") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"))
figure_2 <- gridExtra::arrangeGrob(
  figure_2a, figure_2b, ncol = 1, heights = c(1, 1.2),
  bottom = grid::textGrob("2025 includes January through August.",
                          gp = grid::gpar(fontsize = 10, col = "grey35"))
)
save_figure(figure_2, "Figure_2", 9, 8)

figure_3a <- ggplot(national_population,
                    aes(month, central_80mg_90pct)) +
  annotate("rect", xmin = suspension_start, xmax = reopening_start,
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = .25) +
  geom_ribbon(aes(ymin = dose_120mg_90pct, ymax = dose_40mg_90pct),
              fill = "#56B4E9", alpha = .25) +
  geom_ribbon(aes(ymin = coverage_80mg_95pct, ymax = coverage_80mg_75pct),
              fill = "#E69F00", alpha = .35) +
  geom_line(linewidth = .6, colour = "black") +
  geom_vline(xintercept = c(suspension_start, reopening_start),
             linetype = "dashed", linewidth = .4) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "a", x = NULL,
       y = "Estimated people per month",
       subtitle = "COFEPRIS volume-based scenarios") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
figure_3b <- ggplot(cij_annual, aes(year, patients, colour = site, group = site)) +
  geom_line(linewidth = .7) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = 2018:2025) +
  scale_colour_manual(values = c(
    "Ciudad Juárez" = "#D55E00", "Tijuana" = "#009E73",
    "Zapopan" = "#0072B2"
  )) +
  labs(title = "b", x = "Year", y = "Observed CIJ patients",
       colour = "CIJ site") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))
figure_3 <- gridExtra::arrangeGrob(
  figure_3a, figure_3b, ncol = 1, heights = c(1.25, 1),
  bottom = grid::textGrob(
    "Blue: 40-120 mg/day at 90% coverage. Orange: 75-95% coverage at 80 mg/day. 2025 ends in August.",
    gp = grid::gpar(fontsize = 9, col = "grey35")
  )
)
save_figure(figure_3, "Figure_3", 9, 8)

# The map uses locality-level site counts, not establishment names or licences.
stopifnot(sum(site_counts$n) == 17,
          sum(site_counts$n[site_counts$sector == "Private"]) == 14)
map_points <- site_counts %>%
  mutate(
    display_lon = case_when(
      locality == "Tijuana" & sector == "Private" ~ longitude - .20,
      locality == "Tijuana" & sector == "Public (CIJ)" ~ longitude + .24,
      locality == "Ciudad Juárez" & sector == "Private" ~ longitude - .18,
      locality == "Ciudad Juárez" & sector == "Public (CIJ)" ~ longitude + .25,
      TRUE ~ longitude
    ),
    display_lat = case_when(
      locality == "Tijuana" & sector == "Private" ~ latitude - .10,
      locality == "Tijuana" & sector == "Public (CIJ)" ~ latitude + .14,
      locality == "Ciudad Juárez" & sector == "Private" ~ latitude - .10,
      locality == "Ciudad Juárez" & sector == "Public (CIJ)" ~ latitude + .15,
      TRUE ~ latitude
    ),
    sector = factor(sector, levels = c("Private", "Public (CIJ)"))
  )
map_labels <- map_points %>%
  group_by(locality) %>%
  summarise(X = mean(display_lon), Y = mean(display_lat), .groups = "drop")
map_sf <- sf::st_as_sf(map_points, coords = c("display_lon", "display_lat"),
                      crs = 4326, remove = FALSE)
states_sf <- sf::st_read("reference/mexico_states.geojson", quiet = TRUE)
map_plot <- ggplot() +
  geom_sf(data = states_sf, fill = "#E9F0F4", colour = "#AABAC3",
          linewidth = .25) +
  geom_sf(data = map_sf, aes(shape = sector, fill = sector),
          size = 5.8, colour = "white", stroke = 1.1) +
  geom_text(data = map_points,
            aes(display_lon, display_lat, label = n),
            colour = "white", fontface = "bold", size = 3.2) +
  ggrepel::geom_text_repel(
    data = map_labels, aes(X, Y, label = locality),
    seed = 20260925, size = 3.7, fontface = "bold",
    colour = "#17242B", box.padding = .45, point.padding = .30,
    min.segment.length = 0, segment.colour = "#52636D",
    segment.size = .35, max.overlaps = Inf
  ) +
  scale_shape_manual(values = c("Private" = 21, "Public (CIJ)" = 23)) +
  scale_fill_manual(values = c("Private" = "#176B8E",
                                "Public (CIJ)" = "#D98C00")) +
  guides(shape = "none", fill = "none") +
  coord_sf(xlim = c(-118.8, -86.5), ylim = c(14, 33.4),
           expand = FALSE, datum = NA) +
  theme_void(base_size = 12) +
  theme(plot.margin = margin(8, 8, 2, 8))
map_legend <- grid::grobTree(
  grid::circleGrob(x = grid::unit(.30, "npc"), y = grid::unit(.5, "npc"),
                   r = grid::unit(.08, "in"),
                   gp = grid::gpar(fill = "#176B8E", col = "white")),
  grid::textGrob("Private", x = grid::unit(.33, "npc"),
                 y = grid::unit(.5, "npc"), just = "left",
                 gp = grid::gpar(fontsize = 11)),
  grid::polygonGrob(x = grid::unit(c(.53, .54, .53, .52), "npc"),
                    y = grid::unit(c(.70, .50, .30, .50), "npc"),
                    gp = grid::gpar(fill = "#D98C00", col = "white")),
  grid::textGrob("Public (CIJ)", x = grid::unit(.56, "npc"),
                 y = grid::unit(.5, "npc"), just = "left",
                 gp = grid::gpar(fontsize = 11))
)
supplementary_map <- gridExtra::arrangeGrob(
  map_plot, map_legend, ncol = 1,
  heights = grid::unit(c(6, .375), "in")
)
save_figure(supplementary_map, "Supplementary_Figure_S1", 9, 6.375)

message("Wrote four vector PDFs and matching 300 dpi PNGs.")
