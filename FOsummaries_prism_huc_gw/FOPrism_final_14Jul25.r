rm(list = ls())
{
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  library(terra)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(tidyterra)
  library(sf)
  library(viridis)
  library(grid)
  library(gridExtra)
  library(cowplot)
  library(scales)
  library(tidytext)
  library(forcats)
  library(png)
  
  library(tibble)
}

# ───── Field Office Names and Initials ─────
ADMU_NAME <- c(
  "El Centro Field Office", "Needles Field Office", "Barstow Field Office",
  "Ridgecrest Field Office", "Bishop Field Office", "Mother Lode Field Office",
  "Eagle Lake Field Office", "Applegate Field Office", "Redding Field Office",
  "Palm Springs/S. Coast Field Office", "Bakersfield Field Office",
  "Ukiah Field Office", "Arcata Field Office", "Central Coast Field Office"
)

fo_abbrev <- c("ecfo", "nefo", "bafo", "rifo", "bifo", "mlfo", "elfo", "alfo", "refo",
               "psfo", "bkfo", "ukfo", "arfo", "ccfo")

names <- data.frame(ADMU_NAME, fo_abbrev)

# ───── Vector and Raster Data ─────
fo_o <- st_read("fo_outer.shp") %>%
  transmute(ADMU_NAME) %>%
  left_join(names, by = "ADMU_NAME") %>%
  mutate(office = gsub(" Field Office", "", ADMU_NAME))

fo_i <- st_read("fo_inner_with_prism9120.shp") %>%
  mutate(
    acres = sq_mi * 640,
    ksqmi = sq_mi / 1000,
    prsm_in = prsm_mm / 25.4,
    prsm_ft = prsm_in / 12,
    maf = acres * prsm_ft / 1e6
  ) %>%
  left_join(st_drop_geometry(names), by = "ADMU_NAME")

fo_ppt_grid_proj_in <- rast("prsm50m9120_blmca_albers.tif") / 25.4



fo_df_w <- fo_i %>% st_drop_geometry() %>%  transmute(fo_abbrev, ksqmi, prsm_in, maf)
fo_df_l <- fo_df_w %>% pivot_longer(cols = c(ksqmi, prsm_in, maf), names_to = "type", values_to = "value")

fo_df_l_sorted <- fo_df_l %>%
  group_by(type) %>%
  mutate(fo_abbrev_reordered = fct_reorder(fo_abbrev, value, .desc = TRUE)) %>%
  ungroup()

fo_df_l_sorted_sqmi <- fo_df_l_sorted %>% filter(type == "ksqmi") %>% transmute(fo = fo_abbrev, ksqmi = round(value,2))
fo_df_l_sorted_in <- fo_df_l_sorted %>% filter(type == "prsm_in") %>% transmute(fo = fo_abbrev, `map/yr` = round(value,2))
fo_df_l_sorted_maf <- fo_df_l_sorted %>% filter(type == "maf") %>% transmute(fo = fo_abbrev, `maf/yr` = round(value,2))


###################################################################
#################   2x2 plot     #################################
##################################################################



# ───── Shared Extent ─────
bbox <- ext(fo_o)
coord_match <- coord_sf(xlim = c(bbox[1], bbox[2]), ylim = c(bbox[3], bbox[4]), expand = FALSE)

# ───── Custom Colors and Guides ───── ## limits from raster min/maxes from above
custom_colors <- c("#9400D3", "#00FFFF", "#ADFF2F", "#FFFF00", "#FFA500", "#FF4500", "#FF0000", "#CD5C5C", "#B0B0B0", "#228B22",  "#013220", "#FF1493")
p2_breaks <- round(exp(seq(log(2.2), log(182), length.out = 11)), 0)
p2_limits <- c(2.2, 182)
p2_values <- rescale(p2_breaks, to = c(0, 1))

p3_breaks <- round(exp(seq(log(4), log(78), length.out = 15)), 1)
p3_limits <- c(4, 78)
p3_values <- rescale(p3_breaks, to = c(0, 1))

# Separate guides
scaled_legend_viridis <- guide_colorbar(barheight = unit(15, "cm"), barwidth = unit(0.6, "cm"), ticks.colour = "white", ticks.linewidth = 0.5)
scaled_legend_custom <- guide_colorbar(barheight = unit(15, "cm"), barwidth = unit(0.6, "cm"), ticks.colour = "black", ticks.linewidth = 0.5)



# ───── P1: Area (ksqmi) ─────
p1_labels <- fo_i %>% mutate(label = sprintf("%.2f (%s)", round(ksqmi, 2), fo_abbrev)) %>% arrange(ksqmi)

p1 <- ggplot() +
  geom_sf(data = fo_i, aes(fill = ksqmi), color = NA) +
  geom_sf(data = fo_o, fill = NA, color = "black", linewidth = 0.2) +
  geom_sf_text(data = fo_o, aes(label = fo_abbrev), size = 3, color = "black") +
  scale_fill_viridis_c(
    name = "ksqmi",
    breaks = p1_labels$ksqmi,
    labels = p1_labels$label,
    option = "D",
    guide = scaled_legend_viridis
  ) +
  labs(title = "FO managed lands, area", subtitle = "thousand square miles (ksqmi)", hjust = 0) +
  coord_match + theme_minimal() + labs(x = NULL, y = NULL)

# ───── P2: Raster ─────
p2 <- ggplot() +
  geom_spatraster(data = fo_ppt_grid_proj_in) +
  geom_sf(data = fo_o, fill = NA, color = "gray", linewidth = 0.2) +
  geom_sf_text(data = fo_o, aes(label = fo_abbrev), size = 3, color = "black") +
  scale_fill_gradientn(
    name = "inch/yr",
    colours = custom_colors,
    values = p2_values,
    limits = p2_limits,
    breaks = p2_breaks,
    oob = squish,
    na.value = "white",
    guide = scaled_legend_custom
  ) +
  labs(title = "BLM-Ca annual average precipitation", subtitle = "mean annual inches (in/yr) (800m grid resampled to 50m)",  hjust = 0) +
  coord_match + theme_minimal() + labs(x = NULL, y = NULL)
#p2
# ───── P3: Avg Precip (in) ─────
p3_labels <- fo_i %>% mutate(label = sprintf("%.1f (%s)", round(prsm_in, 1), fo_abbrev)) %>% arrange(prsm_in)

p3 <- ggplot() +
  geom_sf(data = fo_i, aes(fill = prsm_in), color = NA) +
  geom_sf(data = fo_o, fill = NA, color = "black", linewidth = 0.2) +
  geom_sf_text(data = fo_o, aes(label = fo_abbrev), size = 3, color = "black") +
  scale_fill_gradientn(
    name = "inch/yr",
    colours = custom_colors,
    values = p3_values,
    limits = p3_limits,
    breaks = p3_labels$prsm_in,
    labels = p3_labels$label,
    oob = squish,
    guide = scaled_legend_custom
  ) +
  labs(title = "FO managed lands, mean areal precipitation (MAP)", subtitle = "mean annual inches (in/yr)",  hjust = 0) +
  coord_match + theme_minimal() + labs(x = NULL, y = NULL)

# ───── P4: Precip Volume (MAF) ─────
p4_labels <- fo_i %>% mutate(label = sprintf("%.2f (%s)", round(maf, 2), fo_abbrev)) %>% arrange(maf)

p4 <- ggplot() +
  geom_sf(data = fo_i, aes(fill = maf), color = NA) +
  geom_sf(data = fo_o, fill = NA, color = "black", linewidth = 0.2) +
  geom_sf_text(data = fo_o, aes(label = fo_abbrev), size = 3, color = "black") +
  scale_fill_viridis_c(
    name = "maf/yr",
    breaks = p4_labels$maf,
    labels = p4_labels$label,
    option = "D",
    guide = scaled_legend_viridis
  ) +
  labs(title = "FO managed lands, mean areal precip. volume (MAPV)", subtitle = "mean annual million acre feet (maf/yr)", hjust = 0) +
  coord_match + theme_minimal() + labs(x = NULL, y = NULL)






# One plot per type, sorted independently
p_area <- fo_df_l %>%
  filter(type == "ksqmi") %>%
  mutate(fo_abbrev = fct_reorder(fo_abbrev, value, .desc = TRUE)) %>%
  ggplot(aes(x = fo_abbrev, y = value)) +
  geom_col(fill = "salmon") +
  labs(title = "FO area (ksqmi)", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_maf <- fo_df_l %>%
  filter(type == "maf") %>%
  mutate(fo_abbrev = fct_reorder(fo_abbrev, value, .desc = TRUE)) %>%
  ggplot(aes(x = fo_abbrev, y = value)) +
  geom_col(fill = "forestgreen") +
  labs(title = "FO mean precip volume (maf), annual normal", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_precip <- fo_df_l %>%
  filter(type == "prsm_in") %>%
  mutate(fo_abbrev = fct_reorder(fo_abbrev, value, .desc = TRUE)) %>%
  ggplot(aes(x = fo_abbrev, y = value)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "FO mean areal precip (in), annual normal", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine them vertically
p5 <- p_area / p_precip / p_maf + plot_layout(ncol = 1)

p5
# ───── Table Grobs ─────

### field office abbreviations
fo_table_df <- fo_o %>%
  st_drop_geometry() %>%
  select(Initials = fo_abbrev, Office = office) %>%
  arrange(Office)

fo_table_grob <- tableGrob(
  fo_table_df,
  rows = NULL,
  theme = ttheme_minimal(base_size = 8, padding = unit(c(1, 4), "pt"))
)

### sq mi, alphabetical 
fo_table_grob_sqmi <-    fo_df_l_sorted_sqmi %>% 
  select(Office = fo, ksqmi) %>%
  tableGrob(
    rows = NULL,
    theme = ttheme_minimal(base_size = 8, padding = unit(c(2, 4), "pt")))

### inches per year, alphabetical 
fo_table_grob_in <-    fo_df_l_sorted_in %>% 
  select(Office = fo, `in/yr` = `map/yr`) %>%
  tableGrob(
    rows = NULL,
    theme = ttheme_minimal(base_size = 8, padding = unit(c(1, 4), "pt")))

### maf per year, alphabetical 
fo_table_grob_maf <-    fo_df_l_sorted_maf %>% 
  select(Office = fo, `maf/yr`) %>%
  tableGrob(
    rows = NULL,
    theme = ttheme_minimal(base_size = 8, padding = unit(c(1, 4), "pt")))





# ───── Compose Layout ─────
# Define layout design (5 rows, 3 columns)
main_plot <- ((p1 | p2) / (p3 | p4)) #/ p5 


final_plot <- ggdraw(main_plot) +
  draw_grob(fo_table_grob, x = 0.7, y = 0.635, width = 0.24, height = 0.35) +
  draw_grob(fo_table_grob_sqmi, x = 0.21, y = 0.635, width = 0.24, height = 0.35) +
  draw_grob(fo_table_grob_in, x = 0.21, y = 0.133, width = 0.24, height = 0.35) +
  draw_grob(fo_table_grob_maf, x = 0.7, y = 0.133, width = 0.24, height = 0.35) 
  
final_plot <- final_plot + plot_annotation(
  title = "  \nPRISM-based historical precipitation (1991-2020) summary per BLM-California field office (FO)",
  subtitle = "BLM-Ca managed lands = colored areas",
  caption =  "_________________________________________________________________________________________________________________________\n  \nBLM-Ca manages ~15% of State-of-Ca area. The thirty year mean from 1991 to 2020 of annual precipitation volume over the State-of-California,\nper PRISM vM5, is ~209 million acre feet (maf) (not shown above). Over the same period, only ~13.5 maf fell on annual average over BLM-Ca managed\nlands (which cover part of NW Nv), representing <6.5% of State-of-Ca's average total. Much of BLM-California managed land is desert, but some\nareas in Northern California qualify as temperate rainforest and locally receive more than fifteen feet of average annual precipitation.\n   \nMaximum FO mean annual MAP: 77.98-in (6.5 ft)--Arcata FO (arfo); Minimum FO mean annual MAP: 4.05-in--El Centro FO (ecfo). With\nboth its relatively moderate size in area and amount of precipitation, Applegate FO receives the greatest mean annual volume of precipitation\n(2.12 maf). Arcata FO also has the largest modeled climatological precipitation gradient, from ~38 in to ~181 in -- a difference of nearly 12 ft. The\nrange for BLM-Ca managed lands spans ~2.25 in ecfo to ~181 in arfo.\n\nPrecipitation data: PRISM 1991–2020 normals vM5, Copyright ©2025, PRISM Group, Oregon State University https://prism.oregonstate.edu.\nMap created 7 July 2025 by BLM-Ca Hydrologist, David O'Connor (doconnor@blm.gov)\nProjection: NAD83 / California Albers (EPSG:3310)\n ",
  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 13, hjust = 0.5),
                plot.caption = element_text(size = 11, hjust = 0.0),
                plot.margin = margin(t = 10, r = 20, b = 10, l = 25) )) # values in points)


# Load BLM logo image
blm_logo <- readPNG("blmlogo.png")
blm_logo_grob <- rasterGrob(blm_logo, interpolate = TRUE)

# Add logo to bottom right of the canvas (adjust x, y for fine tuning)
# Add logo to bottom right "white space" (annotation zone)
final_plot <- ggdraw(final_plot) +  # wrap existing full plot
  draw_grob(blm_logo_grob,
            x = 0.98, y = 0.002,     # near bottom-right margin
            width = 0.08, height = 0.08,
            hjust = 1, vjust = 0)  
  
  # anchor from bottom-right
ggsave(
  "FOPrism_office_2x2_v1.pdf",
  plot = final_plot,
  width = 11,
  height = 17,
  dpi = 400
)

###################################################################################
###########################################################
## zoom in on prism grid  as second plot
#########################################################
##############################################################################


### first adding points representing field office locato

# Define lat/lon for each BLM-CA Field Office
# 1. BLM Field Office Points
fo_office_pts <- tribble(
  ~fo_abbrev, ~lat,     ~lon,
  "arfo",     40.8675, -124.0883,
  "alfo",     41.5926, -120.5596,
  "bafo",     34.8800, -117.0254,
  "bifo",     37.3639, -118.3952,
  "bkfo",     35.3474, -119.0236,
  "ccfo",     36.6727, -121.6540,
  "elfo",     40.4195, -120.6460,
  "ecfo",     32.7920, -115.5485,
  "mlfo",     38.4203, -120.8650,
  "nefo",     34.8458, -114.6102,
  "psfo",     33.8303, -116.5410,
  "refo",     40.5768, -122.3903,
  "rifo",     35.6225, -117.6709,
  "ukfo",     39.1502, -123.2078
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = crs(fo_ppt_grid_proj_in))  # Match PRISM CRS

# 2. Major City Points + Custom Labels
city_pts <- tribble(
  ~city,          ~lat,     ~lon,        ~label,
  "Sacramento",   38.60135, -121.39793,    "CASO",
  "San Francisco",37.7749, -122.4194,    "SF",
  "Los Angeles",  34.0522, -118.2437,    "LA"
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = crs(fo_ppt_grid_proj_in)) %>%
  
  mutate(
    nudge_x = 10000,  # Northeast offset in CRS units (meters)
    nudge_y = 10000
  )
  

# 3. Legend guide for the color ramp
scaled_legend_custom_zoom <- guide_colorbar(
  barheight = unit(28, "cm"),
  barwidth = unit(2.6, "cm"),
  ticks.colour = "black",
  ticks.linewidth = 0.5
)

# 4. Main plot
p2_zoom <- ggplot() +
  # Raster layer
  geom_spatraster(data = fo_ppt_grid_proj_in) +
  
  # BLM Field Office boundaries and labels
  geom_sf(data = fo_o, fill = NA, color = "gray19", linewidth = 0.2) +
  geom_sf_text(data = fo_o, aes(label = fo_abbrev), size = 7, color = "gray19") +
  
  # BLM Field Office points with legend
  geom_sf(
    data = fo_office_pts,
    aes(shape = "BLM-Ca Field Office"),
    fill = "black", color = "black", size = 2.5,
    show.legend = TRUE
  ) +
  
  # Major cities as gray stars (no legend)
  # Star markers
  geom_sf(
    data = city_pts,
    shape = 8,
    color = "gray14",
    size = 3,
    show.legend = FALSE
  ) +
  
  # Nudged labels (text only, not the point)
  geom_text(
    data = cbind(city_pts, st_coordinates(city_pts)),
    aes(x = X + nudge_x, y = Y + nudge_y, label = label),
    size = 3.5,
    color = "gray14"
  ) +

  
  # Precipitation color ramp
  scale_fill_gradientn(
    name = "inch/yr",
    colours = custom_colors,
    values = p2_values,
    limits = p2_limits,
    breaks = p2_breaks,
    oob = squish,
    na.value = "white",
    guide = scaled_legend_custom_zoom
  ) +
  
  # Manual shape legend for field office points
  scale_shape_manual(
    name = "",
    values = c("BLM-Ca Field Office" = 21)
  ) +
  
  # Guide order
  guides(
    shape = guide_legend(order = 1),
    fill = scaled_legend_custom_zoom
  ) +
  
  # Labels and theme
#  labs(
#    title = "BLM-Ca annual average (\"normal\" aka 30-yr mean) precipitation",
#    subtitle = "mean annual inches (in/yr) (800m grid resampled to 50m)"
#  ) +
  coord_match +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )

# 5. Annotated layout
final_plot <- p2_zoom + plot_annotation(
  title = "  \nPRISM-based historical precipitation (1991–2020), BLM-California ",
  subtitle = "BLM-Ca managed lands = colored areas\nBLM-Ca annual average (\"normal\" aka 30-yr mean) precipitation\nmean annual inches (in/yr) (800m grid resampled to 50m)",
  caption = "_________________________________________________________________________________________________________________________\n  \n add something here\n ",
  theme = theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.0),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 25)
  )
)

# 6. Export the figure
ggsave(
  "FOPrism_office_p2_with_cities.pdf",
  plot = final_plot,
  width = 11,
  height = 17,
  dpi = 400
)

  
######## extra summary stats #######

# Calculate mean, min, and max for each polygon in fo_o
#summary_stats <- terra::extract(
#  fo_ppt_grid_proj_in,
#  fo_o,
#  fun = function(x) c(mean = mean(x, na.rm = TRUE),
#                      min = min(x, na.rm = TRUE),
#                      max = max(x, na.rm = TRUE)),
#  touches = TRUE
#)
#
#
#sumstats <- data.frame(summary_stats) 
#colnames(sumstats)
#sumstats <- sumstats %>% transmute(mean = prsm50m9120_blmca, 
#                                   min = prsm50m9120_blmca.1, 
#                                   max = prsm50m9120_blmca.2,
#                                   range = max - min)



##################################################
################   Huc percent ownership
####################################################


blmca_dsslvd <-  fo_i %>%
                 summarise(blmca_july2024 = st_union(geometry))
#plot(BLMCa_dsslvd)
huc6 <- st_read("WBDHU6.shp") 
plot(huc6)



##################################################
############### monthly tifs   ###################
##################################################

ppt9120_jan <- rast("ppt_9120_jan_50m.tif") / 25.4
ppt9120_feb <- rast("ppt_9120_feb_50m.tif") / 25.4
ppt9120_mar <- rast("ppt_9120_mar_50m.tif") / 25.4
ppt9120_apr <- rast("ppt_9120_apr_50m.tif") / 25.4
ppt9120_may <- rast("ppt_9120_may_50m.tif") / 25.4
ppt9120_jun <- rast("ppt_9120_jun_50m.tif") / 25.4
ppt9120_jul <- rast("ppt_9120_jul_50m.tif") / 25.4
ppt9120_aug <- rast("ppt_9120_aug_50m.tif") / 25.4
ppt9120_sep <- rast("ppt_9120_sep_50m.tif") / 25.4
ppt9120_oct <- rast("ppt_9120_oct_50m.tif") / 25.4
ppt9120_nov <- rast("ppt_9120_nov_50m.tif") / 25.4
ppt9120_dec <- rast("ppt_9120_dec_50m.tif") / 25.4
