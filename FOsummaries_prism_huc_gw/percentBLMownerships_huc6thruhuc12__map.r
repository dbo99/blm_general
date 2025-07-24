rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ===============================
# Load Libraries
# ===============================
library(sf)
library(dplyr)
library(units)
library(tidyr)

# ===============================
# Define Unit Conversion Constants
# ===============================

# 1 US survey mile = 1609.347219 meters
survey_mile <- set_units(1609.347219, "m")
sqmi_us_survey <- survey_mile^2  # ≈ 2,589,998.470 m² per square US survey mile

# ===============================
# Define Function to Process HUC Layer
# ===============================
process_huc_area <- function(input_path) {
  message("\nProcessing: ", input_path)
  
  st_read(input_path, quiet = TRUE) %>%
    mutate(
      area_sqmtr = st_area(geometry),
      area_sqmi_r = drop_units(area_sqmtr / sqmi_us_survey)
    )
}

# ===============================
# Process Each HUC File (No Writes)
# ===============================

# HUC 6
huc6 <- process_huc_area("ca_huc6_calalb.shp") %>% transmute(huc6, name, states, sqmi = area_sqmi_r, geometry) 

# HUC 8
huc8 <- process_huc_area("ca_huc8_calalb.shp")  %>% transmute(huc8, name, states, sqmi = area_sqmi_r, geometry) 

# HUC 10
huc10 <- process_huc_area("ca_huc10_calalb.shp")  %>% transmute(huc10, name, states, sqmi = area_sqmi_r, geometry) 

# HUC 12
huc12 <- process_huc_area("ca_huc12_calalb.shp")  %>% transmute(huc12, name, states, sqmi = area_sqmi_r, geometry) 



# =================================================================
###  Load the dissolved BLM lands shapefile (assumes EPSG:3310)
# =================================================================
blmca <- st_read("blmca_jul25_ds_calalb.shp", quiet = TRUE)

# Function to calculate % BLM ownership by intersecting with dissolved BLM layer
add_blm_ownership <- function(huc_layer, blm_layer, sqmi_col = "sqmi") {
  message("Intersecting and calculating BLM ownership...")
  
  # Ensure geometries are valid
  huc_layer <- st_make_valid(huc_layer)
  blm_layer <- st_make_valid(blm_layer)
  
  # Intersect HUCs with BLM
  huc_blm <- st_intersection(huc_layer, blm_layer)
  
  # Calculate area of BLM portion (in square meters)
  huc_blm$blm_area_m2 <- st_area(huc_blm)
  
  # Convert BLM area to sq mi (same as earlier)
  huc_blm$blm_sqmi <- drop_units(huc_blm$blm_area_m2 / sqmi_us_survey)
  
  # Summarize BLM area per HUC
  blm_summary <- huc_blm %>%
    st_drop_geometry() %>%
    group_by(across(-c(blm_area_m2, blm_sqmi))) %>%  # Group by all original HUC identifiers
    summarise(blm_sqmi = sum(blm_sqmi), .groups = "drop")
  
  # Join BLM area back to original HUC layer
  huc_layer_out <- huc_layer %>%
    left_join(blm_summary, by = colnames(blm_summary)[colnames(blm_summary) %in% colnames(huc_layer)]) %>%
    mutate(
      blm_sqmi = replace_na(blm_sqmi, 0),
      pct_blm = round(100 * blm_sqmi / .data[[sqmi_col]], 2)
    )
  
  return(huc_layer_out)
}



huc6  <- add_blm_ownership(huc6,  blmca)
huc8  <- add_blm_ownership(huc8,  blmca)
huc10 <- add_blm_ownership(huc10, blmca)
huc12 <- add_blm_ownership(huc12, blmca)

##################################################################
##############  map plotting  ####################################
##################################################################


# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridisLite)
library(sf)

# ───── Define Bins and Colors ─────
blm_bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
blm_labels <- c("0–10%", "10–20%", "20–30%", "30–40%", "40–50%", 
                "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
blm_colors <- viridisLite::viridis(10, option = "C")

# ───── Bin and Tag Function ─────
bin_and_label <- function(df, level_name) {
  df %>%
    mutate(
      blm_cat = cut(pct_blm, breaks = blm_bins, labels = blm_labels, include.lowest = TRUE, right = FALSE),
      huc_level = level_name
    )
}

# Apply to all HUC levels
huc6_tagged  <- bin_and_label(huc6,  "HUC6")
huc8_tagged  <- bin_and_label(huc8,  "HUC8")
huc10_tagged <- bin_and_label(huc10, "HUC10")
huc12_tagged <- bin_and_label(huc12, "HUC12")

# ───── Combine into one data frame and set facet order ─────
huc_all <- bind_rows(huc6_tagged, huc8_tagged, huc10_tagged, huc12_tagged) %>%
  mutate(
    huc_level = factor(huc_level, levels = c("HUC6", "HUC8", "HUC10", "HUC12"))
  )

# ───── Faceted Plot with Shared Legend and 2x2 Layout ─────
plot_all <- ggplot(huc_all) +
  geom_sf(aes(fill = blm_cat), color = NA) +
  scale_fill_manual(
    values = blm_colors,
    drop = FALSE,
    name = "% BLM",
    na.translate = FALSE
  ) +
  facet_wrap(~ huc_level, ncol = 2) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    legend.key.height = unit(2.5, "lines"),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(0.75, "lines")
  )

# ───── Save Output to Landscape PDF ─────
ggsave(
  filename = "blm_pct_by_huc_levels_facet_v2.pdf",
  plot = plot_all,
  device = "pdf",
  width = 17,
  height = 11,
  units = "in"
)


