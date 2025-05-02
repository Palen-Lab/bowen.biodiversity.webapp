#' Create species richness map from SDM rasters
#'
#' @param sdm_stack raster stack, to be combined into a richness map
#' @param threshold numeric, value from 0 to 1 corresponding to probability
#' threshold in the cells of the SDM to include into species richness map
#'
#' @return SpatRaster, with species present in each cell
#' @export
#'
sdm_to_species_richness <- function(SDM_stack,
                                    presence_threshold = 0.7
) {
  presence_stack <- sapp(SDM_stack, fun = function(x) {
    x[x > presence_threshold] <- 1
    x[x <= presence_threshold] <- 0
    return(x)
  })

  total_richness <- presence_stack %>% sum(na.rm = T)
}

# Defining CRS to make sure all rasters have the same
#the_crs <- "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# Bowen Shoreline polygon for masking the rasters
# bowen_shoreline <- terra::vect("RawData/shoreline_dem_smoothed2/shoreline_dem_smoothed2.shp") %>%
#   terra::project(the_crs)
# bowen_boundary <- terra::vect("RawData/bowen_boundary/Bowen_boundary.shp")

#' Prepare rasters to match Bowen Island
#'
#' @param input_rast, SpatRaster
#'
#' @return SpatRaster
#' @export
#'
raster_prep_bowen <- function(input_rast,
                              bowen_boundary,
                              bowen_shoreline,
                              crs) {
  # Weights for terra::focal() function that provides moving window average
  ## - Smoothing algorithm essentially
  ## - Use this to fill some empty cells on Bowen Island with weighted mean of the
  ##   neighbouring cells.
  weights <- matrix(c(0, 0, 1, 1, 1, 0, 0,
                      0, 1, 1, 2, 1, 1, 0,
                      1, 1, 3, 3, 3, 1, 1,
                      1, 2, 3, 5, 3, 2, 1,
                      1, 1, 3, 3, 3, 1, 1,
                      0, 1, 1, 2, 1, 1, 0,
                      0, 0, 1, 1, 1, 0, 0
  ), nrow=7)

  output_rast <- input_rast %>%
    # Figured out why the SDMs were cropped incorrectly
    # The default for cropping a raster by a polygon is to snap = "near"
    # This means that the polygon extent is snapped to the closest raster
    # We need snap = "out" to make sure the full extent of the polygon is covered by the output raster
    terra::crop(bowen_boundary, snap = "out") %>%
    terra::project(y = crs) %>%
    # Fills the NA values missing in south part of Bowen Island
    terra::focal(w = weights,
                 fun = "mean",
                 na.policy = "only") %>%
    # Downsamples from 400 m to 100 m resolution
    terra::disagg(fact = 4) %>%
    # Smooths the raster
    terra::focal(w = weights,
                 fun = "mean",
                 na.rm = T,
                 na.policy = "omit") %>%
    terra::mask(bowen_shoreline)
}

#' Convert sf polygons to terra rast objects
#'
#' @param input_sf sf object to rasterize to Bowen Island extent
#' @param varname varname to describe rast band
#'
#' @returns terra rast object
#' @export
bowen_sf_to_rast <- function(input_sf,
                             varname = "Present") {
  bowen_boundary <- sf::st_read(here::here("data-raw", "bowen_boundary", "Bowen_boundary.shp")) %>%
    sf::st_transform(sf::st_crs(input_sf))
  ex_sdm <- terra::rast(here::here("inst/app/rasters/2024_12_11_SDM_100m_Bowen_Island/Amphibian_mask/Ambystoma.gracile_BowenIsland_100m.tif"))

  output_rast <- input_sf %>%
    sf::st_intersection(bowen_boundary) %>% # Clip polygons to Bowen Island boundary, change to intersect?
    terra::vect() %>% # Change to SpatVector for rasterization
    terra::project(ex_sdm) %>% # Reproject to the SDM CRS
    terra::rasterize(ex_sdm,
                     touches = T,
                     background = NA) # Rasterize to match SDM, set background values to NA
  terra::varnames(output_rast) <- varname

  output_rast
}

#' Normalize on 0 to 1 scale
#'
#' @param input_rast input raster
#' @returns terra rast where the values are normalized
normalize <- function(input_rast) {
  mm <- terra::minmax(input_rast)
  min <- mm[1]
  max <- mm[2]
  normalized <- (input_rast - min) / (max - min)
}

#' Invert raster, so max values are min and vice versa.
#'
#' @param input_rast input raster
#' @returns terra rast that has been inverted
invert <- function(input_rast) {
  mm <- terra::minmax(input_rast)
  max <- mm[2]
  inverted <- max - input_rast
}


#' Rasterize to Bowen Island Mask
#' @param sf input sf object to rasterize
#' @export
bowen_rasterize <- function(sf) {
  bowen_island_mask <- here("inst/extdata/bowen_mask.tif") %>%
    rast() %>%
    project(bowen.biodiversity.webapp::project_crs)

  output <- sf %>%
    terra::vect() %>% # Change to SpatVector for rasterization
    terra::project(bowen.biodiversity.webapp::project_crs) %>% # Reproject to the mask CRS
    terra::rasterize(bowen_island_mask,
                     touches = T,
                     background = NA) # Rasterize to match mask, set background values to NA
}
