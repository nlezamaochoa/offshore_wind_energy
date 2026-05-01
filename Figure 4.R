

############################################################
# Title: Bivariate habitat classification by species & WEA
# Author: Nerea Lezama-Ochoa
# Contact: nlezamao@ucsc.edu
############################################################


rm(list = ls())

# Libraries ----------------------------------------------------
library(tidyverse)

# Paths --------------------------------------------------------
data_file <- "data/processed/Table2.csv"
fig_dir   <- "figures"

# Load data ----------------------------------------------------
df <- read_csv(data_file, show_col_types = FALSE)

# Define species groups ----------------------------------------
protected_species <- c(
  "Blue whale", "Humpback whale",
  "California sea lion", "Leatherback turtle"
)

commercial_species <- c(
  "Blue shark", "Common thresher shark",
  "Mako shark", "Swordfish"
)

species_order <- c(protected_species, commercial_species)

# Format variables ---------------------------------------------
df <- df %>%
  mutate(
    Species = factor(Species, levels = rev(species_order)),  # flipped later
    WEA = factor(WEA, levels = c("CoosBay", "Brookings", "Humboldt", "MorroBay")),
    value = as.numeric(value)
  )

# Color palette ------------------------------------------------
bivariate_colors <- c(
  "Persistent high" = "#806A8A",
  "Persistent low"  = "#CABED0",
  "Emergent"        = "#BC7C8F",
  "Historical"      = "#89A1C8"
)

# Plot ---------------------------------------------------------
fig4 <- ggplot(df, aes(x = Species, y = value, fill = Bivariate)) +
  
  geom_col(
    position = "fill",
    color = "black",
    linewidth = 0.1,
    width = 0.65
  ) +
  
  facet_wrap(~ WEA) +
  
  scale_fill_manual(
    values = bivariate_colors,
    name = NULL
  ) +
  
  scale_y_continuous(labels = scales::percent) +
  
  coord_flip() +
  
  labs(
    x = NULL,
    y = "Proportion"
  ) +
  
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    legend.position = "right"
  )

# Display ------------------------------------------------------
fig4

# Save ---------------------------------------------------------
ggsave(
  filename = file.path(fig_dir, "Fig4_bivariate_WEAs.png"),
  plot = fig4,
  width = 10,
  height = 6,
  dpi = 300
)