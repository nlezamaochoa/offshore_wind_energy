
############################################################
# Title: Time series of average habitat suitability
# Author: Nerea Lezama-Ochoa
# Contact: nlezamao@ucsc.edu
############################################################

# ============================================================
# Figure 1: Habitat suitability trends across Offshore wind energy areas
# ============================================================

# clean workspace ------------------------------------------------
rm(list = ls())

# Load libraries ------------------------------------------------
library(tidyverse)
library(patchwork)

# Set paths -----------------------------------------------------
# Assumes a repo structure like: data/processed/*.csv
data_dir <- "data/processed"

# Load data -----------------------------------------------------
file_list <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

dataset <- file_list %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE))

# change acronyms by names ------------------------------------------
species_names <- c(
  "blsh_trk" = "Blue shark",
  "blwh"     = "Blue whale",
  "casl"     = "California sea lion",
  "ctsh"     = "Common thresher shark",
  "lbst"     = "Leatherback turtle",
  "smsh"     = "Shortfin mako",
  "swor"     = "Swordfish",
  "hbwh"     = "Humpback whale"
)

dataset <- dataset %>%
  mutate(
    species = species_names[species],
    Year = lubridate::year(as.Date(date, format = "%m/%d/%Y"))
  )

# Summarize across ESMs ----------------------------------------
summary_data <- dataset %>%
  group_by(Year, WE, species, gcm) %>%
  summarise(mean_suit_gcm = mean(mean_suit, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year, WE, species) %>%
  summarise(
    mean_suit = mean(mean_suit_gcm, na.rm = TRUE),
    sd_suit   = sd(mean_suit_gcm, na.rm = TRUE),
    .groups   = "drop"
  )

# Define species groups ----------------------------------------
protected_species  <- c("Blue whale", "Blue shark", "Humpback whale", "Common thresher shark")
commercial_species <- c("California sea lion", "Shortfin mako", "Leatherback turtle", "Swordfish")

summary_data <- summary_data %>%
  mutate(
    species = factor(species, levels = c(protected_species, commercial_species)),
    WE = factor(WE, levels = c("CoosBay", "Brookings", "Humboldt", "MorroBay"))
  )

# Define shaded periods ----------------------------------------
periods <- tibble(
  xmin = c(1985, 2035, 2070),
  xmax = c(2015, 2065, 2100),
  ymin = -Inf,
  ymax = Inf
)

# Color palette ------------------------------------------------
we_colors <- c("#6baed6", "#fd8d3c", "#74c476", "#9e9ac8")

# Super-column headers -----------------------------------------
column_titles <- ggplot() +
  annotate("text", x = 1, y = 1, label = "Protected", size = 5) +
  annotate("text", x = 2, y = 1, label = "Commercial", size = 5) +
  xlim(0.65, 2.5) +
  ylim(0, 2) +
  theme_void()

# Main plot ----------------------------------------------------
p_main <- ggplot(summary_data, aes(x = Year, y = mean_suit, color = WE, fill = WE)) +
  
  # Background periods
  geom_rect(
    data = periods,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "lightgray",
    alpha = 0.3
  ) +
  
  # Uncertainty ribbon
  geom_ribbon(
    aes(ymin = mean_suit - sd_suit, ymax = mean_suit + sd_suit),
    alpha = 0.2,
    color = NA
  ) +
  
  # Mean line
  geom_line(linewidth = 1.1) +
  
  # Facets
  facet_wrap(~ species, ncol = 2, scales = "free_y") +
  
  # Scales
  scale_color_manual(values = we_colors) +
  scale_fill_manual(values = we_colors) +
  
  # Labels
  labs(
    x = "Year",
    y = "Average Habitat Suitability"
  ) +
  
  coord_cartesian(xlim = c(1985, 2100)) +
  
  # Theme
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA)
  )

# Combine plot + header ----------------------------------------
fig1 <- column_titles / p_main +
  plot_layout(heights = c(0.08, 1))

# Display ------------------------------------------------------
fig1

# Optional: save -----------------------------------------------
# ggsave("figures/Fig1_habitat_suitability.png", fig1, width = 10, height = 12, dpi = 300)