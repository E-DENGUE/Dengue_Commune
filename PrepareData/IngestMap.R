# Complete workflow with comprehensive ID mapping
library(sf)
library(spdep)
library(INLA)

# Read and prepare data
shp <- st_read("./Data/Staging_shapefiles/svn_admin_boundary_level2_2025.geojson")
shp <- st_make_valid(shp)

# Store original data for reference
original_data <- data.frame(
  original_row = 1:nrow(shp),
  l2_code = shp$l2_code,
  stringsAsFactors = FALSE
)

# Identify and remove islands
nb <- poly2nb(shp, queen = TRUE)
islands <- which(card(nb) == 0)

cat("Original polygons:", nrow(shp), "\n")
cat("Islands found:", length(islands), "\n")

if(length(islands) > 0) {
  cat("Island regions:", shp$l2_code[islands], "\n")
  
  # Store information about removed islands
  removed_islands <- data.frame(
    original_row = islands,
    l2_code = shp$l2_code[islands],
    reason_removed = "island_no_neighbors",
    stringsAsFactors = FALSE
  )
  
  # Remove islands
  shp_clean <- shp[-islands, ]
} else {
  shp_clean <- shp
  removed_islands <- data.frame(
    original_row = integer(0),
    l2_code = character(0),
    reason_removed = character(0),
    stringsAsFactors = FALSE
  )
}

# Create final INLA graph
nb_clean <- poly2nb(shp_clean, queen = TRUE)
nb2INLA("../Data/MDR.graph", nb_clean)
g <- inla.read.graph("../Data/MDR.graph")

# Create comprehensive ID mapping key
id_mapping_key <- data.frame(
  inla_idx = 1:nrow(shp_clean),                    # INLA uses 1-based indexing
  l2_code = shp_clean$l2_code,                     # Original region codes
  original_row = which(!1:nrow(shp) %in% islands), # Original row in shapefile
  neighbor_count = card(nb_clean),                  # Number of neighbors
  stringsAsFactors = FALSE
)

vroom::vroom_write(id_mapping_key,'../Data/inla_id_key.csv')

# Add the INLA index to the clean shapefile
shp_clean$inla_idx <- id_mapping_key$inla_idx

saveRDS(shp_clean,'../Data/inla_shp_file.rds')


# Verify everything matches
stopifnot(length(id_mapping_key$l2_code) == g$n)
stopifnot(all(card(nb_clean) > 0))  # Ensure no islands remain

# Print summary
cat("\n=== ID MAPPING SUMMARY ===\n")
cat("Final dataset: polygons =", nrow(shp_clean), ", graph nodes =", g$n, "\n")
cat("Islands removed:", nrow(removed_islands), "\n")

# Display the mapping key
cat("\n=== ID MAPPING KEY (first 10 rows) ===\n")
print(head(id_mapping_key, 10))

cat("\n=== REMOVED ISLANDS ===\n")
if(nrow(removed_islands) > 0) {
  print(removed_islands)
} else {
  cat("No islands were removed\n")
}
