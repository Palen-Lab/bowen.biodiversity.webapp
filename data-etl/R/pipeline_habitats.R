# Pipeline functions: Phase 4 — Habitat Data
# Alan Whitehead consultant datasets: ponds, wetlands, fish-bearing streams
# TEM/SEI habitat rasterization lives in analyses/03_input_habitats.qmd

# Each function writes its output to data-2-processed/04_habitats/ and
# returns the file path (format = "file" targets).

process_whitehead_ponds <- function() {
  out_dir <- here::here("data-2-processed/04_habitats")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  ponds_wc <- sf::st_read(
    here::here("data-1-raw/datasets/wetlands_whitehead_consultants/ponds-aw.gpkg"),
    quiet = TRUE
  )
  out_path <- file.path(out_dir, "ponds_wc.gpkg")
  sf::st_write(ponds_wc, out_path, append = FALSE, quiet = TRUE)
  out_path
}

process_whitehead_wetlands <- function() {
  out_dir <- here::here("data-2-processed/04_habitats")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  wetlands_wc <- sf::st_read(
    here::here("data-1-raw/datasets/wetlands_whitehead_consultants/wetlands2015.gpkg"),
    quiet = TRUE
  )
  out_path <- file.path(out_dir, "wetlands_wc.gpkg")
  sf::st_write(wetlands_wc, out_path, append = FALSE, quiet = TRUE)
  out_path
}

process_whitehead_fish <- function() {
  out_dir <- here::here("data-2-processed/04_habitats")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  fish_streams_wc <- sf::st_read(
    here::here("data-1-raw/datasets/fish_whitehead_consultants/streams_fish.shp"),
    quiet = TRUE
  )
  fish_streams_wc[319, "OCP_CLASS"] <- "drainage channel"

  out_path <- file.path(out_dir, "fish_streams_wc.gpkg")
  sf::st_write(fish_streams_wc, out_path, append = FALSE, quiet = TRUE)
  out_path
}
