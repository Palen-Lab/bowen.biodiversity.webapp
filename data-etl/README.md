# Bowen Island Biodiversity Data Pipeline

A comprehensive data processing pipeline for biodiversity conservation planning on Bowen Island, BC. This project integrates species distribution models, habitat data, and threat assessments to identify priority areas for conservation.

## Overview

This pipeline processes raw spatial data through multiple stages:
1. **Foundation data** - CRS, boundaries, base layers
2. **Species data** - 193 species distribution models from 341 available SDMs
3. **Habitat data** - ~50 habitat layers from TEM, SEI, and consultant reports
4. **Conservation prioritization** - Zonation5 analysis identifying key biodiversity areas
5. **Threat analysis** - Development pressure, wildfire risk, climate change
6. **Outputs** - Maps, data atlas, web application data

## Quick Start

```r
# Install R 4.4+, then restore package environment
renv::restore()

# Run entire pipeline (WARNING: 3-5 hours for full execution)
source("00_run_all.R")

# Or run specific phases by editing config in 00_run_all.R
```

## Prerequisites

### Software Requirements
- **R** ≥ 4.4
- **Zonation5** (for conservation prioritization)
- **GDAL** (for spatial data processing)
- **Quarto** (for rendering documentation)

### R Packages
All dependencies are managed via `renv`. Key packages:
- `sf`, `terra` - spatial data
- `dplyr`, `tidyr` - data manipulation
- `ggplot2` - visualization
- `here` - path management

### External Data
Some datasets require manual download or API access:
- **Species Distribution Models**: Popescu et al. (2020) - contact Wendy Palen (SFU)
- **iNaturalist API**: Automatic queries (rate-limited)
- **eBird API**: Automatic queries (requires key)
- See [data-raw/README.md](data-raw/README.md) for complete data acquisition guide

## Project Structure

This project deviates from standard R package structure for clarity:

```
data-etl/
├── data-raw/           # Raw data and processing scripts
│   ├── scripts/        # ETL scripts organized by phase
│   └── [data folders]  # Raw spatial data
├── analyses/           # Analysis documentation (Quarto docs)
├── output-data/        # Processed datasets for web app
├── output-figures/     # Generated maps and visualizations
├── output-documents/   # Final reports and deliverables
├── R/                  # Reusable R functions (plotting utilities)
└── data/               # Data objects for use within analyses
```

## Pipeline Execution Order

The pipeline follows a sequential workflow:

### Phase 1: Setup (2-5 minutes)
```r
# Base layers: CRS, boundary, mask, roads, trails, shoreline
scripts <- list.files("data-raw/scripts/01_setup", pattern = "\\.R$", full.names = TRUE)
for(script in scripts) source(script)
```

### Phase 2: Foundation Data (5-10 minutes)
```r
# Context layers: parcels, protected areas, zoning, wetlands, fish streams
scripts <- list.files("data-raw/scripts/02_foundation", pattern = "\\.R$", full.names = TRUE)
for(script in scripts) source(script)
```

### Phase 3: Species Data (3-5 HOURS)
```r
# Process 341 SDMs → filter to 193 species based on observations
source("data-raw/scripts/03_species/01_process_sdm.R")     # SDM processing
source("data-raw/scripts/03_species/02_inat_ebird.R")      # iNaturalist/eBird validation
```
⚠️ **Warning**: This step queries APIs 341+ times with rate-limiting delays

### Phase 4: Habitat Data (10-20 minutes)
```r
# Process TEM, SEI, consultant data → ~50 habitat layers
source("data-raw/scripts/04_habitats/01_process_tem.R")
source("data-raw/scripts/04_habitats/02_process_sei.R")
source("data-raw/scripts/04_habitats/03_human_disturbance.R")
```

### Phase 5: Zonation (20-40 minutes)
```r
# Conservation prioritization analysis
source("data-raw/scripts/05_zonation/01_run_zonation.R")
```

### Phase 6: Threats (10-15 minutes)
```r
# Process threat layers
source("data-raw/scripts/06_threats/01_development.R")
source("data-raw/scripts/06_threats/02_fire.R")
source("data-raw/scripts/06_threats/03_climate.R")
```

### Phase 7: Documentation
```r
# Render Quarto website
quarto::quarto_render()
```

## Documentation

Full analysis documentation is available as a Quarto website:

```bash
quarto render           # Build website locally
quarto preview          # Preview in browser
```

Key analysis documents:
- [analyses/02_input_species.qmd](analyses/02_input_species.qmd) - Species selection criteria
- [analyses/03_input_habitats.qmd](analyses/03_input_habitats.qmd) - Habitat layer preparation
- [analyses/04_conservation_values.qmd](analyses/04_conservation_values.qmd) - Zonation results
- [analyses/08_data_atlas.qmd](analyses/08_data_atlas.qmd) - Final data atlas

See [analyses/README.md](analyses/README.md) for complete documentation guide.

## Data Sources

This project integrates data from multiple sources:

- **Popescu et al. (2020)**: 341 species distribution models for BC vertebrates
- **Metro Vancouver**: Sensitive Ecosystem Inventory (SEI), land cover
- **BC Government**: Terrestrial Ecosystem Mapping (TEM), protected areas
- **Alan Whitehead Consulting**: Wetlands, fish-bearing streams
- **Bowen Heron Watch**: Heron and raptor nest locations
- **iNaturalist & eBird**: Observation validation data
- **Pottinger (2005)**: Historical environmental inventory

Complete citations and acquisition instructions: [data-raw/README.md](data-raw/README.md)

## Outputs

### Processed Data (`output-data/`)
- 193 species SDM rasters (100m resolution)
- ~50 habitat extent rasters
- Zonation conservation priority rankmap
- Threat layers (development, fire, disturbance)
- Web app GeoJSON files

### Figures (`output-figures/`)
- Species distribution maps
- Habitat richness maps
- Conservation value maps (top 10%, 20%, 30% areas)
- Threat overlay maps
- Protected area gap analysis

### Documents (`output-documents/`)
- Data atlas PDF (2025)
- Workshop materials
- Technical reports

## Contributors

### Analysts
- Jay Matsushiba
- Michael Tylo
- Wendy Palen
- Tom Sisk

### Workshop Facilitators
- Maya Persam
- Kyra Rolfe
- Valerie Kwok

### Data Contributors
See [index.qmd](index.qmd) for complete acknowledgments

## Citation

If using this pipeline or data:

```
Matsushiba, J., Tylo, M., Palen, W., & Sisk, T. (2025).
Bowen Island Biodiversity Conservation Planning.
Bowen Island Municipality. https://github.com/Palen-Lab/bowen.biodiversity.webapp
```

## License

MIT License - See LICENSE file

## Contact

For questions or issues:
- Open an issue: https://github.com/Palen-Lab/bowen.biodiversity.webapp/issues
- Contact: Jay Matsushiba (hello@jmatsushiba.com)
