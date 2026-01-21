# =============================================================================
# Master Pipeline Orchestration Script
# Bowen Island Biodiversity Conservation Planning
# =============================================================================
#
# This script runs the entire data-etl pipeline from raw data to final outputs.
#
# EXECUTION TIME: 3-5 hours for full pipeline (species processing is slow)
#
# PREREQUISITES:
#   - R >= 4.4
#   - Zonation5 installed
#   - Raw data downloaded (see data-raw/README.md)
#   - renv environment restored: renv::restore()
#
# USAGE:
#   source("00_run_all.R")
#
#   Or customize execution by editing the config section below
#
# =============================================================================

# Load required packages
library(here)
library(usethis)

# =============================================================================
# CONFIGURATION
# =============================================================================

config <- list(
  # Phase 1: Setup (2-5 minutes)
  run_setup = TRUE,           # CRS, boundary, analysis mask

  # Phase 2: Foundation Data (5-10 minutes)
  run_foundation = TRUE,      # Roads, trails, parcels, protected areas, etc.

  # Phase 3: Species Data (3-5 HOURS!)
  run_species = FALSE,        # SDM processing + iNaturalist/eBird queries
                              # WARNING: Queries 341+ APIs with rate limiting
                              # Set to FALSE if SDMs already processed

  # Phase 4: Habitat Data (10-20 minutes)
  run_habitats = TRUE,        # TEM, SEI, consultant data → ~50 layers

  # Phase 5: Zonation (20-40 minutes)
  run_zonation = TRUE,        # Conservation prioritization analysis

  # Phase 6: Threats (10-15 minutes)
  run_threats = TRUE,         # Development, fire, climate overlays

  # Phase 7: Documentation (5-10 minutes)
  render_analyses = FALSE     # Render Quarto website
                              # Note: Can be slow, run separately if preferred
)

# =============================================================================
# PIPELINE EXECUTION
# =============================================================================

start_time <- Sys.time()
message("========================================")
message("Bowen Island Biodiversity Data Pipeline")
message("Started: ", start_time)
message("========================================\n")

# Phase 1: Setup
# -----------------------------------------------------------------------------
if(config$run_setup) {
  message("\n=== PHASE 1: Setup ===")
  message("Creating base layers: CRS, boundary, mask, roads, trails, shoreline...")

  tryCatch({
    scripts <- list.files(here("data-raw/scripts/01_setup"),
                          pattern = "\\.R$", full.names = TRUE)
    scripts <- sort(scripts)  # Ensure numbered order

    for(script in scripts) {
      script_name <- basename(script)
      message("Running: ", script_name)
      source(script)
      message("✓ ", script_name, " complete")
    }

  }, error = function(e) {
    message("✗ ERROR in Setup phase:")
    message(e$message)
    stop("Pipeline halted due to error in Phase 1")
  })

  message("Phase 1 complete.")
}

# Phase 2: Foundation Data
# -----------------------------------------------------------------------------
if(config$run_foundation) {
  message("\n=== PHASE 2: Foundation Data ===")
  message("Processing context layers: parcels, protected areas, zoning, wetlands, fish streams...")

  tryCatch({
    scripts <- list.files(here("data-raw/scripts/02_foundation"),
                          pattern = "\\.R$", full.names = TRUE)
    scripts <- sort(scripts)  # Ensure numbered order

    for(script in scripts) {
      script_name <- basename(script)
      message("Running: ", script_name)
      source(script)
      message("✓ ", script_name, " complete")
    }

  }, error = function(e) {
    message("✗ ERROR in Foundation Data phase:")
    message(e$message)
    stop("Pipeline halted due to error in Phase 2")
  })

  message("Phase 2 complete.")
}

# Phase 3: Species Data (SLOW - 3-5 hours)
# -----------------------------------------------------------------------------
if(config$run_species) {
  message("\n=== PHASE 3: Species Data ===")
  message("WARNING: This phase takes 3-5 hours due to API rate limiting!")
  message("Processing 341 SDMs → filtering to 193 species...")
  message("Querying iNaturalist and eBird for validation...")

  tryCatch({
    scripts <- list.files(here("data-raw/scripts/03_species"),
                          pattern = "\\.R$", full.names = TRUE)
    scripts <- sort(scripts)

    for(script in scripts) {
      script_name <- basename(script)
      message("Running: ", script_name)
      source(script)
      message("✓ ", script_name, " complete")
    }

  }, error = function(e) {
    message("✗ ERROR in Species Data phase:")
    message(e$message)
    message("This phase is slow and may have API timeout issues.")
    message("Consider running scripts individually with error handling.")
    stop("Pipeline halted due to error in Phase 3")
  })

  message("Phase 3 complete.")
}

# Phase 4: Habitat Data
# -----------------------------------------------------------------------------
if(config$run_habitats) {
  message("\n=== PHASE 4: Habitat Data ===")
  message("Processing TEM, SEI, consultant data → ~50 habitat layers...")

  tryCatch({
    scripts <- list.files(here("data-raw/scripts/04_habitats"),
                          pattern = "\\.R$", full.names = TRUE)
    scripts <- sort(scripts)

    for(script in scripts) {
      script_name <- basename(script)
      message("Running: ", script_name)
      source(script)
      message("✓ ", script_name, " complete")
    }

  }, error = function(e) {
    message("✗ ERROR in Habitat Data phase:")
    message(e$message)
    stop("Pipeline halted due to error in Phase 4")
  })

  message("Phase 4 complete.")
}

# Phase 5: Zonation
# -----------------------------------------------------------------------------
if(config$run_zonation) {
  message("\n=== PHASE 5: Conservation Prioritization (Zonation) ===")
  message("Running Zonation analysis to identify priority areas...")
  message("This may take 20-40 minutes depending on system...")

  tryCatch({
    source(here("data-raw/scripts/05_zonation/01_run_zonation.R"))
    message("✓ Zonation analysis complete")

  }, error = function(e) {
    message("✗ ERROR in Zonation phase:")
    message(e$message)
    message("Check that Zonation5 is installed and accessible.")
    stop("Pipeline halted due to error in Phase 5")
  })

  message("Phase 5 complete.")
}

# Phase 6: Threats
# -----------------------------------------------------------------------------
if(config$run_threats) {
  message("\n=== PHASE 6: Threat Analysis ===")
  message("Processing threat layers: development, fire, climate...")

  tryCatch({
    scripts <- list.files(here("data-raw/scripts/06_threats"),
                          pattern = "\\.R$", full.names = TRUE)
    scripts <- sort(scripts)

    for(script in scripts) {
      script_name <- basename(script)
      message("Running: ", script_name)
      source(script)
      message("✓ ", script_name, " complete")
    }

  }, error = function(e) {
    message("✗ ERROR in Threat Analysis phase:")
    message(e$message)
    stop("Pipeline halted due to error in Phase 6")
  })

  message("Phase 6 complete.")
}

# Phase 7: Render Documentation
# -----------------------------------------------------------------------------
if(config$render_analyses) {
  message("\n=== PHASE 7: Rendering Documentation Website ===")
  message("Building Quarto website (this may take 5-10 minutes)...")

  tryCatch({
    quarto::quarto_render()
    message("✓ Documentation website rendered")
    message("View at: _site/index.html")

  }, error = function(e) {
    message("✗ ERROR rendering documentation:")
    message(e$message)
    message("Website rendering failed, but data processing is complete.")
    message("You can render manually with: quarto::quarto_render()")
  })

  message("Phase 7 complete.")
}

# =============================================================================
# SUMMARY
# =============================================================================

end_time <- Sys.time()
elapsed <- end_time - start_time

message("\n========================================")
message("Pipeline Complete!")
message("========================================")
message("Started:  ", start_time)
message("Finished: ", end_time)
message("Elapsed:  ", round(elapsed, 2), " ", attr(elapsed, "units"))
message("\nOutputs saved to:")
message("  • Data:      output-data/")
message("  • Figures:   output-figures/")
message("  • Documents: output-documents/")

if(config$render_analyses) {
  message("  • Website:   _site/")
}

message("\nNext steps:")
message("  • Review outputs in output-data/ and output-figures/")
message("  • Check _site/index.html for rendered documentation")
message("  • Run validation: source('data-raw/scripts/validate_pipeline.R')")
message("========================================\n")
