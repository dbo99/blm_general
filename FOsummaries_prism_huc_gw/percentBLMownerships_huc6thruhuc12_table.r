## ── Setup ------------------------------------------------------------
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(sf)
library(dplyr)
library(units)
library(tidyr)
library(stringr)
library(scales)

# Table/vis packages (pick what you need)
#library(kableExtra)   # Option A
library(reactable)    # Option B
#library(DT)           # Option C
library(htmlwidgets)

## ── Constants --------------------------------------------------------
survey_mile   <- set_units(1609.347219, "m")
sqmi_us_survey <- survey_mile^2  # ≈ 2,589,998.470 m²

## ── Helpers ----------------------------------------------------------
process_huc_area <- function(input_path) {
  message("\nProcessing: ", input_path)
  st_read(input_path, quiet = TRUE) %>%
    mutate(
      area_sqmtr = st_area(geometry),
      area_sqmi_r = drop_units(area_sqmtr / sqmi_us_survey)
    )
}

# Add %BLM ownership to any HUC layer
add_blm_ownership <- function(huc_layer, blm_layer, sqmi_col = "sqmi") {
  message("Intersecting and calculating BLM ownership...")
  huc_layer <- st_make_valid(huc_layer)
  blm_layer <- st_make_valid(blm_layer)
  
  huc_blm <- st_intersection(huc_layer, blm_layer)
  
  huc_blm$blm_area_m2 <- st_area(huc_blm)
  huc_blm$blm_sqmi    <- drop_units(huc_blm$blm_area_m2 / sqmi_us_survey)
  
  blm_summary <- huc_blm %>%
    st_drop_geometry() %>%
    group_by(across(-c(blm_area_m2, blm_sqmi))) %>%
    summarise(blm_sqmi = sum(blm_sqmi), .groups = "drop")
  
  huc_layer %>%
    left_join(
      blm_summary,
      by = intersect(names(blm_summary), names(huc_layer))
    ) %>%
    mutate(
      blm_sqmi = replace_na(blm_sqmi, 0),
      pct_blm  = round(100 * blm_sqmi / .data[[sqmi_col]], 2)
    )
}

# Helper to rename columns per level
prep_level <- function(df, level){
  st_drop_geometry(df) %>%
    rename(
      !!paste0("huc", level, "_code") := !!sym(paste0("huc", level)),
      !!paste0("huc", level, "_name") := name,
      !!paste0("sqmi", level)         := sqmi,
      !!paste0("blm_sqmi", level)     := blm_sqmi,
      !!paste0("pct_blm", level)      := pct_blm
    )
}

h6   <- prep_level(huc6,  6)
h8   <- prep_level(huc8,  8)
h10  <- prep_level(huc10, 10)
h12  <- prep_level(huc12, 12)

# Derive parent codes from HUC12 code
h12 <- h12 %>%
  mutate(
    huc10_code = substr(huc12_code, 1, 10),
    huc8_code  = substr(huc12_code, 1, 8),
    huc6_code  = substr(huc12_code, 1, 6)
  )

# Join parent names onto HUC12 rows
nested_df <- h12 %>%
  left_join(select(h10, huc10_code, huc10_name), by = "huc10_code") %>%
  left_join(select(h8,  huc8_code,  huc8_name),  by = "huc8_code")  %>%
  left_join(select(h6,  huc6_code,  huc6_name),  by = "huc6_code")  %>%
  arrange(huc6_name, huc8_name, huc10_name, huc12_name)

# (Optional) also attach %BLM for upper levels
nested_df <- nested_df %>%
  left_join(select(h10, huc10_code, pct_blm10 = pct_blm10), by = "huc10_code") %>%
  left_join(select(h8,  huc8_code,  pct_blm8  = pct_blm8),  by = "huc8_code")  %>%
  left_join(select(h6,  huc6_code,  pct_blm6  = pct_blm6),  by = "huc6_code")

# Keep just what you want to show
tbl <- nested_df %>%
  select(
    HUC6  = huc6_name,
    HUC8  = huc8_name,
    HUC10 = huc10_name,
    HUC12 = huc12_name,
    `% BLM (HUC12)` = pct_blm12
  ) %>%
  mutate(`% BLM (HUC12)` = `% BLM (HUC12)` / 100)  # reactable's percent formatter expects 0–1

# ── Build interactive collapsible table ──────────────────────────────

rt <- reactable(
  tbl,
  groupBy        = c("HUC6", "HUC8", "HUC10"),
  defaultExpanded = FALSE,        # collapse all groups at load
  highlight      = TRUE,
  striped        = TRUE,
  bordered       = TRUE,
  sortable       = TRUE,
  searchable     = TRUE,
  defaultPageSize = 25,
  onClick        = "expand",
  columns = list(
    `HUC6`  = colDef(name = "HUC6"),
    `HUC8`  = colDef(name = "HUC8"),
    `HUC10` = colDef(name = "HUC10"),
    `HUC12` = colDef(name = "HUC12"),
    `% BLM (HUC12)` = colDef(
      name   = "% BLM (HUC12)",
      format = colFormat(percent = TRUE, digits = 1),
      align  = "right"
    )
  )
)

# Save to standalone HTML if you want to share
saveWidget(rt, "HUC_BLM_nested_table.html", selfcontained = TRUE)
