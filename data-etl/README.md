# Bowen Island Biodiversity Data Pipeline

Data processing pipeline for biodiversity conservation planning on Bowen Island, BC. Integrates species distribution models, habitat data, and threat assessments to identify priority conservation areas.

## Quick Start

```r
renv::restore()          # Install dependencies

targets::tar_make()      # Run pipeline (first run: 3–5 hours)
targets::tar_visnetwork() # Visualize dependency graph
```

## Requirements

- R ≥ 4.4, Quarto, Zonation5
- All R packages managed via `renv`
- Some datasets require manual acquisition (see [data-1-raw/README.md](data-1-raw/README.md))

## Project Structure

```
data-etl/
├── _targets.R            # Pipeline definition (run with tar_make())
├── R/                    # Pipeline functions and plot utilities
│   ├── pipeline-foundation.R
│   ├── pipeline-landuse.R
│   ├── pipeline-species.R
│   ├── pipeline-habitats.R
│   ├── pipeline-create-plots.R
│   └── utils-plots.R
├── analyses/             # Quarto analysis documents
├── data-atlas/           # Data atlas Quarto document
├── data-1-raw/           # Raw input data and acquisition notes
├── data-2-processed/     # Intermediate processed data (not committed)
├── data-3-outputs/       # Final outputs (rasters, vectors, figures)
└── output-figures/       # Generated maps
```

## Pipeline Phases

| Phase | Targets | Notes |
|-------|---------|-------|
| 0 – Constants | `project_crs` | |
| 1 – Foundation | `boundary`, `shoreline`, `mask`, `ocean_sf`, `roads`, `trails` | |
| 2 – Landuse | `zoning`, `pa`, `parcelmap`, `crown` | |
| 3 – Species | `inat_raw`, `inat_gpkg`, `rankmap` | SDMs processed in `analyses/` |
| 4 – Habitats | `ponds_wc`, `wetlands_wc`, `fish_streams_wc` | |
| 5 – Values | `land_ownership_rast`, `land_ownership_stats` | |
| 6 – Figures | All `*_annotated` / `*_unannotated` targets | Saved to `output-figures/` and Google Drive |

## Data Sources

- **Popescu et al. (2020)**: 341 vertebrate SDMs — contact Wendy Palen (SFU)
- **Metro Vancouver**: SEI, land cover
- **BC Government**: TEM, protected areas, WUI, ParcelMap BC
- **Alan Whitehead Consulting**: Wetlands, fish-bearing streams
- **iNaturalist / eBird**: Observation validation
- **Daniel Martin**: Subdivision potential analysis

## Contributors

Jay Matsushiba, Michael Tylo, Wendy Palen, Tom Sisk
See [index.qmd](index.qmd) for full acknowledgments.

## Citation

```
Matsushiba, J., Tylo, M., Palen, W., & Sisk, T. (2025).
Bowen Island Biodiversity Conservation Planning.
Bowen Island Municipality.
```
