# Load necessary libraries
library(sf)
library(dplyr)
library(units)

# Step 1: Read the shapefile (assumes EPSG:3310 = California Albers)
huc6 <- st_read("ca_huc6_calalb.shp")

# Step 2: Calculate area in square meters
huc6 <- huc6 %>%
  mutate(area_m2 = st_area(geometry))

# Step 3: Convert to square US survey miles
# Define the unit conversion manually for precision
survey_mile <- set_units(1609.347219, "m")  # 1 US survey mile = 1609.347219 meters
sqmi_us_survey <- survey_mile^2              # = 2.589998470 km²

huc6 <- huc6 %>%
  mutate(area_sqmi_r = drop_units(area_m2 / sqmi_us_survey))

# Step 4: Compare to ArcGIS Pro's "areasqmi" column
huc6 <- huc6 %>%
  mutate(
    diff_sqmi = area_sqmi_r - areasqmi,
    pct_diff = 100 * diff_sqmi / areasqmi
  )

# Step 5: Print summary
summary(huc6$pct_diff)
print(huc6 %>% select(areasqmi, area_sqmi_r, diff_sqmi, pct_diff))

# Optional: View in mapview
# library(mapview)
# mapview(huc6, zcol = "pct_diff")