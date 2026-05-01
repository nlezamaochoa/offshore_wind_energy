
############################################################
# Title: Habitat suitability maps + change
# Author: Nerea Lezama-Ochoa
# Contact: nlezamao@ucsc.edu
############################################################


rm(list = ls())

# Libraries ----------------------------------------------------
library(tidyverse)
library(terra)
library(sf)
library(patchwork)
library(rnaturalearth)
library(scales)

# Paths --------------------------------------------------------
data_dir <- "data/rasters"
shp_dir  <- "data/shapefiles"
fig_dir  <- "figures"

# -------------------------------------------------------------
# Basemap
# -------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")

states <- ne_states(
  country = "United States of America",
  returnclass = "sf"
)

west_states <- states %>%
  filter(name %in% c("California", "Oregon", "Washington"))

# -------------------------------------------------------------
# Wind Energy Areas (sf only, no sp/rgdal)
# -------------------------------------------------------------
OR_WE <- st_read(file.path(shp_dir, "OR_WEA.shp"), quiet = TRUE)
CA_WE <- st_read(file.path(shp_dir, "CA_WEA.shp"), quiet = TRUE)

WE <- bind_rows(OR_WE, CA_WE) %>%
  st_transform(4326)

# -------------------------------------------------------------
# Species + metadata
# -------------------------------------------------------------
species_list <- c("hbwh","blwh","blsh_trk","casl","ctsh","lbst","smsh","swor")

species_names <- c(
  "hbwh" = "Humpback whale",
  "blwh" = "Blue whale",
  "casl" = "California sea lion",
  "lbst" = "Leatherback",
  "blsh_trk" = "Blue shark",
  "ctsh" = "Thresher shark",
  "smsh" = "Mako shark",
  "swor" = "Swordfish"
)

periods <- c(
  hist = "1985_2015",
  nf   = "2035_2065",
  fut  = "2070_2100"
)

models <- c("GFDL_", "HAD_", "IPSL_")

# -------------------------------------------------------------
# Load rasters (mean + sd)
# -------------------------------------------------------------
load_rasters <- function(species, period) {
  files <- file.path(
    data_dir,
    paste0(species, "_Habitat ", period, "_", models, ".grd")
  )
  
  r <- rast(files)
  
  list(
    mean = app(r, mean, na.rm = TRUE),
    sd   = app(r, sd, na.rm = TRUE)
  )
}

raster_data <- map(species_list, function(s) {
  map(periods, ~ load_rasters(s, .x))
}) %>%
  set_names(species_list)

# -------------------------------------------------------------
# Raster → dataframe
# -------------------------------------------------------------
raster_to_df <- function(data, type = "mean") {
  map_dfr(names(data), function(s) {
    map_dfr(names(data[[s]]), function(p) {
      r <- data[[s]][[p]][[type]]
      df <- as.data.frame(r, xy = TRUE)
      colnames(df) <- c("lon","lat","value")
      
      df %>%
        mutate(
          species = species_names[s],
          period = p
        )
    })
  }) %>%
    mutate(
      species = factor(species, levels = species_names),
      period = factor(period, levels = c("hist","nf","fut"))
    )
}

df_mean <- raster_to_df(raster_data, "mean")
df_sd   <- raster_to_df(raster_data, "sd")

# -------------------------------------------------------------
# Generic plotting function
# -------------------------------------------------------------
plot_map <- function(df, fill_var, palette, title) {
  ggplot(df) +
    geom_tile(aes(lon, lat, fill = {{fill_var}}), alpha = 0.8) +
    
    scale_fill_gradientn(
      colours = palette,
      oob = squish
    ) +
    
    geom_sf(data = world, fill = "grey", color = "black") +
    geom_sf(data = west_states, fill = NA, color = "black", linewidth = 0.3) +
    geom_sf(data = WE, fill = NA, color = "black", linewidth = 0.5) +
    
    coord_sf(xlim = c(-126, -120), ylim = c(35, 47), expand = FALSE) +
    
    facet_wrap(~ species, ncol = 4) +
    
    theme_classic() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      strip.text = element_text(size = 11, face = "bold"),
      panel.border = element_rect(color = "black", fill = NA)
    ) +
    ggtitle(title)
}

# -------------------------------------------------------------
# Color palettes
# -------------------------------------------------------------
pal_mean <- colorRampPalette(
  c("#9b59b6","#3498db","#1abc9c","#f1c40f","#e74c3c")
)(100)

pal_sd <- viridis::viridis(100)

pal_diff <- colorRampPalette(
  c("darkred","white","#0072B2")
)(100)

# -------------------------------------------------------------
# Mean + SD plots
# -------------------------------------------------------------
plot_mean <- function(period) {
  plot_map(
    df_mean %>% filter(period == !!period),
    value,
    pal_mean,
    paste("Mean SH -", period)
  )
}

plot_sd <- function(period) {
  plot_map(
    df_sd %>% filter(period == !!period),
    value,
    pal_sd,
    paste("SD -", period)
  )
}

# -------------------------------------------------------------
# Difference + % change
# -------------------------------------------------------------
compute_diff <- function(period1, period2) {
  map_dfr(species_list, function(s) {
    r1 <- raster_data[[s]][[period1]]$mean
    r2 <- raster_data[[s]][[period2]]$mean
    
    df <- as.data.frame(r2 - r1, xy = TRUE)
    colnames(df) <- c("lon","lat","value")
    
    df %>% mutate(species = species_names[s])
  })
}

compute_pct <- function(period1, period2) {
  map_dfr(species_list, function(s) {
    r1 <- raster_data[[s]][[period1]]$mean
    r2 <- raster_data[[s]][[period2]]$mean
    
    df <- as.data.frame((r2 - r1) / r1 * 100, xy = TRUE)
    colnames(df) <- c("lon","lat","value")
    
    df %>% mutate(species = species_names[s])
  })
}

df_diff <- compute_diff("hist","fut")
df_pct  <- compute_pct("hist","fut")

# -------------------------------------------------------------
# Final combined figure (clean layout)
# -------------------------------------------------------------
hist_row <- plot_map(
  df_mean %>% filter(period == "hist"),
  value,
  pal_mean,
  "Historical SH"
)

change_row <- plot_map(
  df_pct,
  value,
  pal_diff,
  "Percent change (future vs historical)"
)

fig_combined <- hist_row / change_row +
  plot_layout(heights = c(1,1))

# -------------------------------------------------------------
# Save outputs
# -------------------------------------------------------------
ggsave(file.path(fig_dir, "FigS1_mean_hist.png"), plot_mean("hist"), width = 12, height = 8, dpi = 300)
ggsave(file.path(fig_dir, "FigS1_sd_hist.png"), plot_sd("hist"), width = 12, height = 8, dpi = 300)

ggsave(file.path(fig_dir, "FigS1_diff.png"), 
       plot_map(df_diff, value, pal_diff, "Future - Historical"), 
       width = 12, height = 8, dpi = 300)

ggsave(file.path(fig_dir, "FigS1_combined.png"),
       fig_combined,
       width = 16, height = 8, dpi = 300)