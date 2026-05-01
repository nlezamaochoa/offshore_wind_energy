

############################################################
# Title: Bivariate Habitat Maps (Future vs Present)
# Author: Nerea Lezama-Ochoa
# Contact: nlezamao@ucsc.edu
############################################################

rm(list = ls())

# ------------------------
# Load packages
# ------------------------
required_pkgs <- c(
  "raster", "data.table", "tidyverse", "classInt",
  "cowplot", "patchwork", "sf", "rnaturalearth", "here"
)

invisible(lapply(required_pkgs, function(pkg){
  if(!requireNamespace(pkg, quietly = TRUE)){
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}))

# ============================================================
# FUNCTIONS
# ============================================================

# ------------------------
# Bivariate color matrix
# ------------------------
colmat <- function(nbreaks = 2, breakstyle = "quantile",
                   upperleft = "#0096EB", upperright = "#820050", 
                   bottomleft = "#BEBEBE", bottomright = "#FFE60F",
                   xlab = "x label", ylab = "y label") {
  
  if (breakstyle == "sd") {
    warning("SD style not supported → switching to quantile")
    breakstyle <- "quantile"
  }
  
  my.data <- seq(0, 1, .01)
  my.class <- classInt::classIntervals(my.data, n = nbreaks, style = breakstyle)
  
  my.pal.1 <- classInt::findColours(my.class, c(upperleft, bottomleft))
  my.pal.2 <- classInt::findColours(my.class, c(upperright, bottomright))
  
  col.matrix <- matrix(NA, nrow = 101, ncol = 101)
  
  for (i in 1:101) {
    my.col <- c(my.pal.1[i], my.pal.2[i])
    col.matrix[102 - i, ] <- classInt::findColours(my.class, my.col)
  }
  
  seqs <- seq(0, 100, (100 / nbreaks))
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
  
  attr(col.matrix, "breakstyle") <- breakstyle
  attr(col.matrix, "nbreaks") <- nbreaks
  
  return(col.matrix)
}

# ------------------------
# Bivariate raster mapping
# ------------------------
bivariate.map <- function(rasterx, rastery, colourmatrix) {
  
  quanx <- getValues(rasterx)
  quany <- getValues(rastery)
  
  brks <- classInt::classIntervals(
    quanx,
    n = attr(colourmatrix, "nbreaks"),
    style = attr(colourmatrix, "breakstyle")
  )$brks
  
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  
  x_class <- cut(quanx, breaks = brks, labels = FALSE, include.lowest = TRUE)
  y_class <- cut(quany, breaks = brks, labels = FALSE, include.lowest = TRUE)
  
  cols <- mapply(function(x, y) colourmatrix[y, x], x_class, y_class)
  
  r <- rasterx
  r[] <- as.numeric(as.factor(cols))
  
  return(r)
}

# ============================================================
# PATHS
# ============================================================

data_dir <- here::here("data", "rasters")
shape_dir <- here::here("data", "shapefiles")
output_dir <- here::here("figures")

if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ============================================================
# LOAD SPATIAL DATA
# ============================================================

OR_WE <- rgdal::readOGR(file.path(shape_dir, "OR_WEA.shp"))
CA_WE <- rgdal::readOGR(file.path(shape_dir, "CA_WEA.shp"))

proj4string(OR_WE) <- CRS("+proj=utm +zone=10 +datum=WGS84")
proj4string(CA_WE) <- CRS("+proj=utm +zone=10 +datum=WGS84")

OR_WE <- spTransform(OR_WE, CRS("+proj=longlat +datum=WGS84"))
CA_WE <- spTransform(CA_WE, CRS("+proj=longlat +datum=WGS84"))

WE_sf <- sf::st_as_sf(rbind(OR_WE, CA_WE))
WE_outline <- sf::st_union(WE_sf)

states_sf <- rnaturalearth::ne_states(
  country = "United States of America",
  returnclass = "sf"
)

CA_OR <- subset(states_sf, postal %in% c("CA", "OR"))

# ============================================================
# SETTINGS
# ============================================================

species_list <- c("hbwh","lbst","blsh_trk","casl","smsh","ctsh","swor","blwh")
extent_to_crop <- extent(-126, -120, 35, 47)

# ============================================================
# LOOP THROUGH SPECIES
# ============================================================

for (sp in species_list) {
  
  message("Processing: ", sp)
  
  # ------------------------
  # Load rasters
  # ------------------------
  load_stack <- function(period){
    files <- c("GFDL_", "HAD_", "IPSL_") %>%
      paste0(sp, "_Habitat ", period, "_", . , ".grd") %>%
      file.path(data_dir, .)
    
    if(any(!file.exists(files))){
      stop("Missing raster files for ", sp)
    }
    
    stack(lapply(files, raster))
  }
  
  hist_stack <- load_stack("1985_2015")
  fut_stack  <- load_stack("2070_2100")
  
  present_mean <- crop(mean(hist_stack), extent_to_crop)
  future_mean  <- crop(mean(fut_stack), extent_to_crop)
  
  # ------------------------
  # Bivariate mapping
  # ------------------------
  col.matrixQ <- colmat(
    nbreaks = 2,
    breakstyle = "fisher",
    xlab = "Future",
    ylab = "Present",
    bottomright = "#BC7C8F",
    upperright = "#806A8A",
    bottomleft = "#CABED0",
    upperleft = "#89A1C8"
  )
  
  bivmap <- bivariate.map(future_mean, present_mean, col.matrixQ)
  
  df <- as.data.frame(bivmap, xy = TRUE)
  colnames(df)[3] <- "bivVal"
  
  # ------------------------
  # Plot
  # ------------------------
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_raster(aes(fill = bivVal)) +
    
    geom_sf(data = WE_outline, fill = NA, color = "black", linewidth = 1) +
    geom_sf(data = CA_OR, fill = NA, color = "black", linewidth = 0.5) +
    
    scale_fill_gradientn(colours = col.matrixQ, na.value = "transparent") +
    
    coord_sf(xlim = c(-126,-120), ylim = c(35,47), expand = FALSE) +
    
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
  
  # ------------------------
  # Save
  # ------------------------
  ggsave(
    filename = paste0("Fig_S3_", sp, ".png"),
    plot = p,
    path = output_dir,
    width = 20,
    height = 20,
    units = "cm",
    dpi = 300
  )
  
  message("Done: ", sp)
}

# ============================================================
# END
# ============================================================