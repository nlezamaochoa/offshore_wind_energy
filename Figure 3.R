
############################################################
# Title: Species overlap (historic vs future vs difference)
# Author: Nerea Lezama-Ochoa
# Contact: nlezamao@ucsc.edu
############################################################


rm(list = ls())

# Libraries ----------------------------------------------------
library(tidyverse)
library(raster)
library(sf)
library(patchwork)
library(rnaturalearth)
library(scales)

# Paths --------------------------------------------------------
data_dir <- "data/rasters"
shp_dir  <- "data/shapefiles"
fig_dir  <- "figures"

# Load basemap -------------------------------------------------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

west_states <- rnaturalearth::ne_states(
  country = "United States of America",
  returnclass = "sf"
) %>%
  filter(name %in% c("California", "Oregon", "Washington"))

# Load Offshore Wind Energy Areas --------------------------------------
OR_WE <- sf::st_read(file.path(shp_dir, "OR_WEA.shp"), quiet = TRUE)
CA_WE <- sf::st_read(file.path(shp_dir, "CA_WEA.shp"), quiet = TRUE)

WE <- bind_rows(OR_WE, CA_WE) %>%
  st_transform(crs = 4326)

# -------------------------------------------------------------
# Helper functions
# -------------------------------------------------------------

# Load and average ESM rasters
load_species <- function(species, period) {
  files <- c("GFDL", "HAD", "IPSL") %>%
    paste0(species, "_Habitat ", period, "_", ., "_.grd") %>%
    file.path(data_dir, .)
  
  rasters <- lapply(files, raster)
  mean(stack(rasters))
}

# Normalize raster (for whales if needed)
normalize_raster <- function(r) {
  (r - cellStats(r, "min")) / (cellStats(r, "max") - cellStats(r, "min"))
}

# Convert raster to dataframe
raster_to_df <- function(r) {
  df <- rasterToPoints(r) %>% as.data.frame()
  colnames(df) <- c("lon", "lat", "value")
  df
}

# Plot function
plot_map <- function(df, title, fill_limits, palette, legend_name) {
  ggplot() +
    geom_tile(data = df, aes(lon, lat, fill = value), alpha = 0.8) +
    scale_fill_gradientn(
      colours = palette,
      limits = fill_limits,
      oob = squish,
      name = legend_name
    ) +
    geom_sf(data = world, fill = "grey", color = "black") +
    geom_sf(data = west_states, fill = NA, color = "black", linewidth = 0.4) +
    geom_sf(data = WE, fill = NA, color = "black", linewidth = 0.5) +
    coord_sf(xlim = c(-126, -120), ylim = c(35, 47), expand = FALSE) +
    theme_classic() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      plot.title = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    ) +
    ggtitle(title)
}

# -------------------------------------------------------------
# Species groups
# -------------------------------------------------------------

protected <- c("lbst", "casl", "hbwh", "blwh")
commercial <- c("swor", "blsh_trk", "ctsh", "smsh")

periods <- c(
  hist = "1985_2015",
  near = "2035_2055",
  fut  = "2070_2100"
)

# -------------------------------------------------------------
# Build stacked rasters
# -------------------------------------------------------------

build_stack <- function(species_list, period) {
  rasters <- lapply(species_list, load_species, period = period)
  
  # normalize humpback whale
  idx <- which(species_list == "hbwh")
  if (length(idx) > 0) {
    rasters[[idx]] <- normalize_raster(rasters[[idx]])
  }
  
  sum(stack(rasters))
}

# Protected species -------------------------------------------
prot_hist <- build_stack(protected, periods["hist"])
prot_fut  <- build_stack(protected, periods["fut"])

# Commercial species ------------------------------------------
comm_hist <- build_stack(commercial, periods["hist"])
comm_fut  <- build_stack(commercial, periods["fut"])

# -------------------------------------------------------------
# Convert to dataframes
# -------------------------------------------------------------

prot_hist_df <- raster_to_df(prot_hist)
prot_fut_df  <- raster_to_df(prot_fut)
prot_diff_df <- raster_to_df(prot_fut - prot_hist)

comm_hist_df <- raster_to_df(comm_hist)
comm_fut_df  <- raster_to_df(comm_fut)
comm_diff_df <- raster_to_df(comm_fut - comm_hist)

# -------------------------------------------------------------
# Color palettes
# -------------------------------------------------------------

pal_main <- colorRampPalette(c("#9b59b6","#3498db","#1abc9c","#f1c40f","#e74c3c"))(100)
pal_diff <- colorRampPalette(c("darkred","white","#0072B2"))(50)

# -------------------------------------------------------------
# Plot: Protected
# -------------------------------------------------------------

p1 <- plot_map(prot_hist_df, "Protected - Historic", c(0, 3), pal_main, "Sum HS")
p2 <- plot_map(prot_fut_df,  "Protected - Future",   c(0, 3), pal_main, "Sum HS")
p3 <- plot_map(prot_diff_df, "Protected - Δ",        c(-1, 1), pal_diff, "Δ HS")

fig_protected <- p1 + p2 + p3 + plot_layout(ncol = 3)

# -------------------------------------------------------------
# Plot: Commercial
# -------------------------------------------------------------

p4 <- plot_map(comm_hist_df, "Commercial - Historic", c(0, 2), pal_main, "Sum HS")
p5 <- plot_map(comm_fut_df,  "Commercial - Future",   c(0, 2), pal_main, "Sum HS")
p6 <- plot_map(comm_diff_df, "Commercial - Δ",        c(-1, 1), pal_diff, "Δ HS")

fig_commercial <- p4 + p5 + p6 + plot_layout(ncol = 3)

# -------------------------------------------------------------
# Save figures
# -------------------------------------------------------------

ggsave(file.path(fig_dir, "Fig3_protected.png"), fig_protected, width = 15, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "Fig3_commercial.png"), fig_commercial, width = 15, height = 5, dpi = 300)