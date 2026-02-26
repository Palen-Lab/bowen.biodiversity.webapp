# Pipeline functions: Phase 2 — Landuse Data
# Zoning, protected areas, parcels, private land

load_zoning <- function(project_crs) {
  sf::st_read(
    here::here("data-1-raw/datasets/zoning/BM_ZONING.shp"),
    quiet = TRUE
  ) %>%
    sf::st_transform(crs = project_crs)
}

load_protected_areas <- function(project_crs) {
  pa_dir <- here::here("data-1-raw/datasets/protectedareas")

  covenants_path <- file.path(pa_dir, "BI-Covenant-Catalogue-Layer 1.3.2.gpkg")
  covenants <- sf::st_read(covenants_path, quiet = TRUE) %>%
    dplyr::select(name = Document.No.)
  covenants <- covenants[!sf::st_is_empty(covenants$geom), ]
  covenants$type <- "Covenants"
  covenants$filepath <- covenants_path

  alr_path <- file.path(pa_dir, "Bowen-ALR-Lands-JD.gpkg")
  alr <- sf::st_read(alr_path, quiet = TRUE) %>%
    dplyr::select() %>%
    dplyr::mutate(name = paste0("ALR_", dplyr::row_number()))
  alr <- alr[!sf::st_is_empty(alr$geom), ]
  alr$type <- "Agricultural Land Reserve"
  alr$filepath <- alr_path

  conservancies_path <- file.path(pa_dir, "Bowen-Conservancies-JD.gpkg")
  conservancies <- sf::st_read(conservancies_path, quiet = TRUE) %>%
    dplyr::select(name = NAME)
  conservancies <- conservancies[!sf::st_is_empty(conservancies$geom), ]
  conservancies$type <- "Conservancies"
  conservancies$filepath <- conservancies_path

  mvparks_path <- file.path(pa_dir, "Bowen-Metro-Parks-JD.gpkg")
  mvparks <- sf::st_read(mvparks_path, quiet = TRUE) %>%
    dplyr::select(name = NAME)
  mvparks <- mvparks[!sf::st_is_empty(mvparks$geom), ]
  mvparks$type <- "MetroVancouver Park"
  mvparks$filepath <- mvparks_path

  bowenmuniparks_path <- file.path(pa_dir, "Bowen-Muni-Parks-JD.gpkg")
  bowenmuniparks <- sf::st_read(bowenmuniparks_path, quiet = TRUE) %>%
    dplyr::select(name = NAME)
  bowenmuniparks <- bowenmuniparks[!sf::st_is_empty(bowenmuniparks$geom), ]
  bowenmuniparks$type <- "Bowen Island Municipality Park"
  bowenmuniparks$filepath <- bowenmuniparks_path

  provparks_path <- file.path(pa_dir, "Bowen-Prov-Parks-JD.gpkg")
  provparks <- sf::st_read(provparks_path, quiet = TRUE) %>%
    dplyr::select(name = parkname)
  provparks <- provparks[!sf::st_is_empty(provparks$geom), ]
  provparks$type <- "Provincial Park"
  provparks$filepath <- provparks_path

  ecoreserve_path <- file.path(pa_dir, "Eco-Reserve-JD.gpkg")
  ecoreserve <- sf::st_read(ecoreserve_path, quiet = TRUE) %>%
    dplyr::select(name = parkname)
  ecoreserve <- ecoreserve[!sf::st_is_empty(ecoreserve$geom), ]
  ecoreserve$type <- "Ecological Reserve"
  ecoreserve$filepath <- ecoreserve_path

  # 2026/02/24: Removed ALR
  rbind(bowenmuniparks, conservancies, covenants, ecoreserve, mvparks, provparks) %>%
    sf::st_transform(project_crs) %>%
    sf::st_make_valid()
}

dissolve_protected_areas <- function(bowen_protectedareas) {
  bowen_protectedareas %>%
    dplyr::summarise() %>%
    sf::st_cast() %>%
    sf::st_buffer(1)
}

load_ogma <- function(project_crs) {
  ogma_path <- here::here("data-1-raw/datasets/protectedareas/Bowen-OGMAs-JD.gpkg")
  ogma <- sf::st_read(ogma_path, quiet = TRUE) %>%
    sf::st_transform(project_crs) %>%
    dplyr::select(name = NAME) %>%
    sf::st_make_valid()
  ogma <- ogma[!sf::st_is_empty(ogma$geom), ]
  ogma$type <- "Old Growth Management Area"
  ogma$filepath <- ogma_path
  ogma
}

load_crown <- function(project_crs) {
  crown_path <- here::here("data-1-raw/datasets/protectedareas/Crown-Land-JD.gpkg")
  crown <- sf::st_read(crown_path, quiet = TRUE) %>%
    sf::st_transform(project_crs) %>%
    dplyr::select(name = parkname)
  crown <- crown[!sf::st_is_empty(crown$geom), ] %>%
    sf::st_snap(x = ., y = ., tolerance = 0.1) %>%
    sf::st_make_valid()
  crown$type <- "Crown Land"
  crown$filepath <- crown_path
  crown
}

# Removes two known slivers (rows 1 and 74) from unprotected crown land.
create_unprotected_crown <- function(crown, dissolved_protectedareas) {
  unprotected_crown <- sf::st_difference(crown, dissolved_protectedareas)
  # Row 1: Fairy Fen Natural Reserve (sliver)
  # Row 74: Seymour Bay Park (wrong polygon)
  unprotected_crown[!rownames(unprotected_crown) %in% c("1", "74"), ]
}

load_parcelmap <- function(project_crs) {
  sf::st_read(
    here::here("data-1-raw/datasets/parcelmap_bowen/parcelmap_bowen.gpkg"),
    layer = "pmbc_parcel_fabric_poly_svw",
    quiet = TRUE
  ) %>%
    sf::st_transform(project_crs) %>%
    sf::st_cast("MULTIPOLYGON") %>%
    sf::st_make_valid() %>%
    filter(OBJECTID != 2096841) # Remove ferry route
}

parcel_subdiv <- function(parcelmap) {
  #### Preparing layers for plotting ####
  # RR1 and RR2 (Rural Residential 1 and 2 Zoning)
  rr1_rr2 <- here(
    "data-1-raw/datasets/development_potential_danielmartin/zoning subdivision potential.xlsx"
  ) %>%
  readxl::read_xlsx(sheet = "RR1,RR2")
  # RR3 (Rural Residential 3 Zoning)
  rr3 <- here(
    "data-1-raw/datasets/development_potential_danielmartin/zoning subdivision potential.xlsx"
  ) %>%
  readxl::read_xlsx(sheet = "RR3 properties")
  # SR (Settlement Residential zoning)
  sr <- here(
    "data-1-raw/datasets/development_potential_danielmartin/zoning subdivision potential.xlsx"
  ) %>%
  readxl::read_xlsx(sheet = "SR properties")
  # CD (Comprehensive Development Zones)
  ## Deal with these separately, need to do some wrangling
  ocp_subdivision <- rbind(rr1_rr2, rr3, sr) %>%
    filter(!is.na(pid))
  ocp_subdivision_parcelmap <- merge(
    ocp_subdivision,
    parcelmap,
    by.x = "pid",
    by.y = "PARCEL_NAME"
  ) %>%
    mutate(
      subdividable = case_when(
        `Can Subdivide?` == 0 ~ FALSE,
        `Can Subdivide?` > 0 ~ TRUE
      )
    ) %>%
    st_as_sf() 

  ocp_subdivision_parcelmap_f <- ocp_subdivision_parcelmap %>%
    mutate(
      pid = as.numeric(pid)
    )
  # Removing duplicated columns
  unique(
    ocp_subdivision_parcelmap_f$pid == ocp_subdivision_parcelmap_f$PID_NUMBER
  ) # TRUE
  unique(
    ocp_subdivision_parcelmap_f$parcelclass ==
    toupper(ocp_subdivision_parcelmap_f$PARCEL_CLASS)
  ) # TRUE
  unique(
    ocp_subdivision_parcelmap_f$ownertype ==
    toupper(ocp_subdivision_parcelmap_f$OWNER_TYPE)
  ) # FALSE
  which(
    ocp_subdivision_parcelmap_f$ownertype !=
    toupper(ocp_subdivision_parcelmap_f$OWNER_TYPE)
  ) # 1740
  # turns out its UNCLASS and Unclassified, so the same
  ocp_subdivision_parcelmap_fd <- ocp_subdivision_parcelmap_f %>%
    select(
      !c(
        parcelclass,
        ownertype,
        pid,
        Shape__Area,
        area_sqm,
        SHAPE_Area,
        SHAPE_Length,
        objectid
      )
    )
  # Remove outlier and duplicate PID
  unique(ocp_subdivision_parcelmap_fd$PID_NUMBER) %>% length() # 1755, so one fewer than than nrows
  ocp_subdivision_parcelmap_fdr <- ocp_subdivision_parcelmap_fd %>%
    filter(`Can Subdivide?` != 46)

  ocp_subdivision_parcelmap_fdr
}

parcel_biod_val <- function(parcelmap, rankmap) {
  # Match parcelmap projection to the Zonation rast
  parcelmap_vect <- parcelmap %>%
    vect() %>%
    project(rankmap)
  # Extract values that overlap cells
  parcelmap_extract <- terra::extract(
    rankmap,
    parcelmap_vect,
    sum, # sums all cells that overlap with each parcel
    bind = T, # bind to SpatVector
    weights = T # weight by fraction of cell that is within the polygon
  )
  parcelmap_extract %>%
    st_as_sf()
  # Save extracted values
  # parcelmap_extract_sf %>%
  #   st_drop_geometry() %>%
  #   write.csv(here(output_dir, "parcelmap_biod_vals.csv"))
}

create_privateland <- function(parcelmap_bowen, dissolved_protectedareas) {
  parcelmap_bowen %>%
    dplyr::filter(OWNER_TYPE %in% c("Private", "Unclassified")) %>%
    sf::st_make_valid() %>%
    sf::st_union() %>%
    sf::st_difference(dissolved_protectedareas) %>%
    sf::st_snap(x = ., y = ., tolerance = 0.1) %>%
    sf::st_buffer(5) %>%
    rmapshaper::ms_simplify(keep = 0.03, keep_shapes = FALSE)
}

load_pa_candidates <- function(project_crs) {
  here(
    "data-3-outputs/7_protected_areas/new_protected_areas.gpkg"
  ) %>%
    st_read() %>%
    st_transform(st_crs(project_crs))
}

# Creates a categorical raster of land ownership/authority:
#   1 = Private, 2 = Protected Areas, 3 = Crown Land, 4 = Mixed
# Uses rankmap as the rasterization template (CRS/resolution/extent).
create_land_ownership_rast <- function(pa, unprotected_crown, privateland, rankmap) {
  sf_rasterize <- function(sf_obj) {
    sf_obj %>%
      terra::vect() %>%
      terra::project(rankmap) %>%
      terra::rasterize(rankmap, touches = TRUE, background = NA)
  }
  # Rasterize each category
  private_rast <- sf_rasterize(privateland)

  protectedareas_rast <- sf_rasterize(pa) %>%
    terra::subst(1, 2) %>%
    terra::as.factor()

  unprotected_crown_rast <- sf_rasterize(unprotected_crown) %>%
    terra::subst(1, 3) %>%
    terra::as.factor()

  # Combine: mixed where private overlaps public, then layer protected > crown > private
  pub_prot_rast <- terra::cover(protectedareas_rast, unprotected_crown_rast)
  mixed_rast <- terra::mask(pub_prot_rast, private_rast) %>%
    terra::not.na(falseNA = TRUE) %>%
    terra::subst(1, 4) %>%
    terra::as.factor()

  terra::cover(mixed_rast, pub_prot_rast) %>%
    terra::cover(private_rast) %>%
    terra::as.factor()
}

# Returns a named list of cell counts and percentages for each land ownership
# category: private (1), protected (2), crown (3), mixed (4).
compute_land_ownership_stats <- function(land_ownership_rast) {
  freq_tbl <- terra::freq(land_ownership_rast)
  counts   <- setNames(freq_tbl$count, as.character(freq_tbl$value))
  total    <- sum(counts)
  data.frame(
    total_cells   = total,
    private_n     = counts[["1"]],
    protected_n   = counts[["2"]],
    crown_n       = counts[["3"]],
    mixed_n       = counts[["4"]],
    private_pct   = counts[["1"]] / total,
    protected_pct = counts[["2"]] / total,
    crown_pct     = counts[["3"]] / total,
    mixed_pct     = counts[["4"]] / total
  )
}

# Returns land ownership cell counts and percentages restricted to the top 30%
# biodiversity cells (rankmap >= 70th percentile).
compute_land_ownership_top30_stats <- function(land_ownership_rast, rankmap) {
  top30_mask <- rankmap %>%
    remove_by_quantile(0.7) %>%
    terra::not.na() %>%
    terra::classify(cbind(FALSE, NA)) %>%
    terra::resample(land_ownership_rast, method = "near")

  masked_rast <- terra::mask(land_ownership_rast, top30_mask)

  freq_tbl <- terra::freq(masked_rast)
  counts   <- setNames(freq_tbl$count, as.character(freq_tbl$value))
  total    <- sum(counts)
  data.frame(
    total_cells   = total,
    private_n     = counts[["1"]],
    protected_n   = counts[["2"]],
    crown_n       = counts[["3"]],
    mixed_n       = counts[["4"]],
    private_pct   = counts[["1"]] / total,
    protected_pct = counts[["2"]] / total,
    crown_pct     = counts[["3"]] / total,
    mixed_pct     = counts[["4"]] / total
  )
}