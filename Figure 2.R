
############################################################
# Title: Average habitat suitability
# Author: Nerea Lezama-Ochoa
# Contact: nlezamao@ucsc.edu
############################################################

rm(list = ls())

# ------------------------
# Load packages
# ------------------------
library(terra)
library(dplyr)
library(ggplot2)
library(viridis)
library(sf)
library(rnaturalearth)
library(patchwork)
library(scales)

# ------------------------
# Define paths (EDIT ONLY THIS SECTION)
# ------------------------
data_dir   <- "data/species/"          # raster files
shape_dir  <- "data/shapefiles/"       # shapefiles
output_dir <- "outputs/figures/"       # results

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------
# Load basemaps
# ------------------------
world  <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(country = "United States of America", returnclass = "sf")

# ------------------------
# Load WEAs (using sf instead of sp)
# ------------------------
OR_WE <- st_read(file.path(shape_dir, "OR_WEA.shp"), quiet = TRUE)
CA_WE <- st_read(file.path(shape_dir, "CA_WEA.shp"), quiet = TRUE)

# Transform CRS safely
transform_area <- function(shp) {
  st_transform(shp, crs = 4326)
}

Coos_Bay <- transform_area(OR_WE %>% filter(Name == "Coos Bay"))
Brookings <- transform_area(OR_WE %>% filter(Name == "Brookings"))

Humboldt <- transform_area(
  CA_WE %>% filter(Lease_Numb %in% c("OCS-P 0561", "OCS-P 0562"))
)

MorroBay <- transform_area(
  CA_WE %>% filter(Lease_Numb %in% c("OCS-P 0563","OCS-P 0564","OCS-P 0565"))
)

areas <- rbind(Coos_Bay, Brookings, Humboldt, MorroBay)

# ------------------------
# Species and models
# ------------------------
species_list <- c("hbwh","blwh","blsh_trk","casl","ctsh","lbst","smsh","swor")

species_names_map <- c(
  "blwh"="Blue whale",
  "hbwh"="Humpback whale",
  "casl"="California sea lion",
  "lbst"="Leatherback",
  "blsh_trk"="Blue shark",
  "ctsh"="C. Thresher shark",
  "smsh"="Mako shark",
  "swor"="Swordfish"
)

time_periods <- c("hist"="1985_2015","nf"="2035_2055","fut"="2070_2100")
model_list <- c("GFDL_","HAD_","IPSL_")

# ------------------------
# Load rasters
# ------------------------
load_mean_sd_raster <- function(species, period){
  
  files <- file.path(
    data_dir,
    paste0(species, "_Habitat ", period, "_", model_list, ".grd")
  )
  
  if(any(!file.exists(files))){
    stop("Missing files: ", paste(files[!file.exists(files)], collapse=", "))
  }
  
  r <- rast(files)
  
  list(
    mean = app(r, mean),
    sd   = app(r, sd)
  )
}

raster_data <- lapply(species_list, function(s){
  lapply(time_periods, function(p){
    load_mean_sd_raster(s, p)
  })
})

names(raster_data) <- species_list
for(s in species_list) names(raster_data[[s]]) <- names(time_periods)

# ------------------------
# Convert to dataframe
# ------------------------
raster_to_df <- function(raster_list, what="mean"){
  
  bind_rows(lapply(names(raster_list), function(s){
    
    bind_rows(lapply(names(raster_list[[s]]), function(p){
      
      r <- raster_list[[s]][[p]][[what]]
      df <- as.data.frame(r, xy = TRUE)
      
      df$species <- species_names_map[s]
      df$period  <- p
      df
      
    }))
    
  })) %>%
    rename(lon = x, lat = y, layer = 3) %>%
    mutate(
      species = factor(species, levels = unname(species_names_map)),
      period  = factor(period, levels = c("hist","nf","fut"))
    )
}

combined_df_mean <- raster_to_df(raster_data, "mean")
combined_df_sd   <- raster_to_df(raster_data, "sd")

# ------------------------
# Plot functions
# ------------------------
plot_base <- function(df, fill_scale, title_text){
  
  ggplot(df) +
    geom_tile(aes(lon, lat, fill = layer), alpha = 0.8) +
    fill_scale +
    geom_sf(data = world, fill = "grey", color = "black") +
    geom_sf(data = states, fill = NA, color = "black", size = 0.3) +
    geom_sf(data = areas, fill = NA, color = "black", size = 0.5) +
    coord_sf(xlim = c(-126,-120), ylim = c(35,47), expand = FALSE) +
    facet_wrap(~species, ncol = 4) +
    theme_classic() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      strip.text = element_text(size = 13, face = "bold")
    ) +
    ggtitle(title_text)
}

# ------------------------
# Mean plots
# ------------------------
pal <- colorRampPalette(c("#9b59b6","#3498db","#1abc9c","#f1c40f","#e74c3c"))

CCE_hist <- plot_base(
  filter(combined_df_mean, period=="hist"),
  scale_fill_gradientn(colours = pal(100), name="HS"),
  "A) Historical HS (1985-2015)"
)

# ------------------------
# % Change (safe)
# ------------------------
compute_pct_change_df <- function(raster_data, species_list){
  
  bind_rows(lapply(species_list, function(s){
    
    r_hist <- raster_data[[s]][["hist"]]$mean
    r_fut  <- raster_data[[s]][["fut"]]$mean
    
    pct <- (r_fut - r_hist) / r_hist * 100
    pct[r_hist == 0] <- NA
    
    df <- as.data.frame(pct, xy=TRUE)
    df$species <- species_names_map[s]
    df
    
  })) %>%
    rename(lon=x, lat=y, layer=3) %>%
    mutate(species = factor(species, levels = unname(species_names_map)))
}

pct_change_df <- compute_pct_change_df(raster_data, species_list)

change_plot <- plot_base(
  pct_change_df,
  scale_fill_gradient2(low="darkred", mid="white", high="#0072B2",
                       midpoint=0, limits=c(-100,100),
                       oob=squish, name="% change"),
  "Far-future change (%)"
)

# ------------------------
# Combine (independent legends)
# ------------------------
combined_plot <- CCE_hist / change_plot +
  plot_layout(guides = "keep")

# ------------------------
# Save
# ------------------------
ggsave(file.path(output_dir, "CCE_summary.png"),
       combined_plot, width=12, height=8, dpi=300)