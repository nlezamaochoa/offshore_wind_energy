
############################################################
# Title: Habitat Suitability Across Decades & Seasons
# Author: Nerea Lezama-Ochoa
# Contact: nlezamao@ucsc.edu
############################################################


rm(list = ls())

# ------------------------
# Load required packages
# ------------------------
required_pkgs <- c("ggplot2", "dplyr", "tidyverse", "wesanderson", "grid", "here")

invisible(lapply(required_pkgs, function(pkg){
  if(!requireNamespace(pkg, quietly = TRUE)){
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}))

# ------------------------
# Define paths (relative)
# ------------------------
data_dir  <- here::here("data", "multispecies_WE")
output_dir <- here::here("figures")

if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ------------------------
# Load and combine data
# ------------------------
file_list <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

dataset <- file_list %>%
  lapply(read.csv) %>%
  bind_rows()

# ------------------------
# Date processing
# ------------------------
dataset$date <- as.Date(dataset$date, format = "%m/%d/%Y")

dataset <- dataset %>%
  mutate(
    Year  = as.numeric(format(date, "%Y")),
    Month = as.numeric(format(date, "%m")),
    
    Season = case_when(
      Month %in% c(12, 1, 2)  ~ "Winter",
      Month %in% c(3, 4, 5)   ~ "Spring",
      Month %in% c(6, 7, 8)   ~ "Summer",
      Month %in% c(9, 10, 11) ~ "Fall"
    ),
    
    Period = floor(Year / 10) * 10
  )

# ------------------------
# Labels and factor levels
# ------------------------
species_labels <- c(
  "blwh"      = "Blue whale",
  "hbwh"      = "Humpback whale",
  "casl"      = "California sea lion",
  "lbst"      = "Leatherback turtle",
  "blsh_trk"  = "Blue shark",
  "ctsh"      = "C. thresher shark",
  "smsh"      = "S. mako shark",
  "swor"      = "Swordfish"
)

species_order <- c("blwh", "hbwh", "casl", "lbst",
                   "blsh_trk", "ctsh", "smsh", "swor")

wea_labels <- c(
  "CoosBay"   = "Coos Bay OWE",
  "Brookings" = "Brookings OWE",
  "Humboldt"  = "Humboldt OWE",
  "MorroBay"  = "Morro Bay OWE"
)

dataset <- dataset %>%
  mutate(
    species = factor(species, levels = species_order),
    WE      = factor(WE, levels = names(wea_labels)),
    Season  = factor(Season, levels = c("Winter","Spring","Summer","Fall"))
  )

# ------------------------
# Aggregate data
# ------------------------
dataset_summary <- dataset %>%
  group_by(Period, Season, species, WE) %>%
  summarise(mean_suit = mean(mean_suit, na.rm = TRUE), .groups = "drop")

# ------------------------
# Plot settings
# ------------------------
pal <- wes_palette("Zissou1", 100, type = "continuous")

y_breaks <- seq(
  floor(min(dataset_summary$Period)),
  ceiling(max(dataset_summary$Period)),
  by = 20
)

# ------------------------
# Create plot
# ------------------------
p <- ggplot(dataset_summary, aes(x = Season, y = Period)) +
  geom_tile(aes(fill = mean_suit)) +
  
  scale_fill_gradientn(colours = pal, limits = c(0,1)) +
  scale_y_continuous(breaks = y_breaks) +
  
  facet_grid(
    WE ~ species,
    labeller = labeller(species = species_labels, WE = wea_labels)
  ) +
  
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 16, face = "bold", angle = 45, hjust = 1),
    strip.text.x = element_text(size = 16, face = "bold"),
    strip.text.y = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(size = 18, face = "bold"),
    panel.spacing = unit(1.2, "lines")
  ) +
  
  labs(
    title = "Habitat Suitability Across Decadal Periods & Seasons",
    x = NULL,
    y = NULL,
    fill = "Mean suitability"
  )

# ------------------------
# Save figure
# ------------------------
ggsave(
  filename = "Fig_S2.png",
  plot = p,
  path = output_dir,
  width = 18,
  height = 13,
  dpi = 400,
  units = "in",
  bg = "white"
)

# ------------------------
# End of script
# ------------------------