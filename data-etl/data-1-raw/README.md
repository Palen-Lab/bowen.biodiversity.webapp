# Raw Data Sources and Acquisition Guide

This directory contains raw spatial data and ETL processing scripts for the Bowen Island biodiversity analysis.

## Directory Structure

```
data-raw/
├── scripts/                # ETL processing scripts (numbered by execution order)
│   ├── 01_setup/           # Foundation: CRS, boundary, mask
│   ├── 02_foundation/      # Base layers: roads, trails, parcels, etc.
│   ├── 03_species/         # Species distribution model processing
│   ├── 04_habitats/        # Habitat layer processing
│   ├── 05_zonation/        # Conservation prioritization
│   └── 06_threats/         # Threat analysis
│
├── datasets/               # Raw data organized by source (listed below)
│   ├── boundary/
│   ├── roads/
│   ├── trails/
│   ├── parcelmap/
│   ├── protectedareas/
│   ├── TEM/
│   ├── inat/
│   ├── ebird/
│   └── [and more...]
│
└── README.md               # This file
```

## Data Sources

### Administrative Boundaries

#### Bowen Island Boundary
- **Folder**: `datasets/boundary/`
- **Source**: Metro Vancouver Open Data
- **URL**: https://www.metrovancouver.org/data
- **Format**: Shapefile
- **CRS**: BC Albers (EPSG:3005) → reprojected to custom CRS
- **Script**: `scripts/01_setup/02_bowen_boundary.R`

### Transportation Networks

#### Roads
- **Folder**: `datasets/roads/`
- **Source**: BC Road Inventory
- **Format**: Shapefile
- **Notes**: Includes road classification, surface type
- **Script**: `scripts/02_foundation/01_bowen_roads.R`

#### Trails
- **Folder**: `datasets/trails/`
- **Source**: Bowen Island Municipality
- **Format**: Shapefile
- **Notes**: Hiking trails, maintained and informal
- **Script**: `scripts/02_foundation/02_bowen_trails.R`

### Land Ownership and Zoning

#### Property Parcels
- **Folder**: `datasets/parcelmap/`, `datasets/bc_parcelmap/`
- **Source**: BC Assessment / ParcelMap BC
- **URL**: https://www.bcassessment.ca/
- **Format**: Shapefile
- **Notes**: Private land boundaries, property attributes
- **Script**: `scripts/02_foundation/04_bowen_parcelmap.R`

#### Zoning
- **Folder**: `datasets/zoning/`
- **Source**: Bowen Island Municipality
- **Format**: Shapefile
- **Notes**: Land use zones (residential, commercial, conservation, etc.)
- **Script**: `scripts/02_foundation/06_bowen_zoning.R`

### Protected Areas

#### Multiple Protected Area Types
- **Folder**: `datasets/protectedareas/`
- **Source**: Multiple (BC Parks, Agricultural Land Reserve, Conservation Covenants, etc.)
- **Includes**:
  - Parks and protected areas (BC Parks)
  - Agricultural Land Reserve (ALR)
  - Conservation covenants
  - Old Growth Management Areas (OGMA)
  - Crown land
  - Ecological reserves
  - Community forests
- **Script**: `scripts/02_foundation/05_bowen_protectedareas.R`

### Species Data

#### Species Distribution Models (SDMs)
- **Folder**: `species_distribution_models/` (NOT IN GIT - too large)
- **Source**: Popescu et al. (2020)
- **Paper**: https://www.nature.com/articles/s41598-020-64501-7
- **Access**: Contact Wendy Palen (SFU) - Cumulative Effects Teams Sharepoint
- **Path on Sharepoint**: `BCH_project_backup/SPECIES/`
- **Count**: 341 SDMs available → 193 used after filtering
- **Species**: Vertebrates (mammals, birds, reptiles, amphibians)
- **Resolution**: 400m native → downsampled to 100m
- **Format**: GeoTIFF rasters
- **Notes**: Use files with `_NAs_` suffix (rock/ice/lakes masked)
- **Script**: `scripts/03_species/01_process_sdm.R`

#### iNaturalist Observations
- **Folder**: `datasets/inat/`
- **Source**: iNaturalist API
- **URL**: https://www.inaturalist.org/
- **Access**: Public API (rate-limited, 3 sec/request)
- **Format**: CSV from API queries
- **Notes**: Used for validation of SDM species presence
- **Script**: `scripts/03_species/02_inat_ebird.R`

#### eBird Observations
- **Folder**: `datasets/ebird/`
- **Source**: eBird / Nature Counts
- **URL**: https://ebird.org/
- **Access**: Public API (requires key)
- **Format**: CSV from API queries
- **Notes**: Bird observations for validation
- **Script**: `scripts/03_species/02_inat_ebird.R`

#### Christmas Bird Count
- **Folder**: `datasets/christmas_bird_count/`
- **Source**: Audubon Society / Bowen Island community
- **Format**: CSV/Shapefile
- **Notes**: Annual bird survey data

#### Heron and Raptor Nests
- **Folder**: `datasets/heron_watch/`
- **Source**: Bowen Heron Watch (community monitoring)
- **Format**: Shapefile (point locations)
- **Notes**: Great Blue Heron colonies, raptor nests
- **Script**: `scripts/04_habitats/` (integrated into habitat layers)

### Habitat Data

#### Terrestrial Ecosystem Mapping (TEM)
- **Folder**: `datasets/TEM/`
- **Source**: BC Government
- **URL**: https://catalogue.data.gov.bc.ca/
- **Format**: Shapefile (polygons)
- **Notes**: Detailed ecosystem classification (~20+ habitat types)
- **Script**: `scripts/04_habitats/01_process_tem.R`

#### Sensitive Ecosystem Inventory (SEI)
- **Folder**: `datasets/metrovancouver_sensitive_ecosystem_inventory/`
- **Source**: Metro Vancouver
- **Format**: Shapefile (polygons)
- **Notes**: Forest age classes, riparian zones, coastal habitats
- **Script**: `scripts/04_habitats/02_process_sei.R`

#### Wetlands (Alan Whitehead Consultants)
- **Folder**: `datasets/wetlands_whitehead_consultants/`
- **Source**: Alan Whitehead Environmental Consulting
- **Format**: Shapefile (polygons)
- **Notes**: Replaces MVSEI wetlands (more accurate/detailed)
- **Script**: `scripts/02_foundation/07_bowen_wetlands.R`

#### Fish-bearing Streams (Alan Whitehead Consultants)
- **Folder**: `datasets/fish_whitehead_consultants/`
- **Source**: Alan Whitehead Environmental Consulting
- **Format**: Shapefile (lines)
- **Notes**: Stream classification, fish presence
- **Script**: `scripts/02_foundation/08_bowen_fish.R`

### LiDAR and Elevation

#### Digital Surface Model (DSM)
- **Folder**: `datasets/dsm/`
- **Source**: BC LiDAR Program
- **URL**: https://www2.gov.bc.ca/gov/content/data/geographic-data-services/lidar
- **Format**: GeoTIFF
- **Resolution**: 1m native
- **Notes**: Used to create analysis mask, tree density
- **Scripts**:
  - `scripts/01_setup/03_bowen_mask.R`
  - `analyses/supplementary/tree_density.qmd`

### Land Cover

#### Metro Vancouver Land Cover
- **Folder**: `datasets/metrovancouver_landcover_raster/`
- **Source**: Metro Vancouver
- **Format**: GeoTIFF raster
- **Classes**: Forest, developed, agriculture, water, etc.
- **Script**: `scripts/04_habitats/` (supplementary)

### Human Disturbance

#### Human Footprint Layer
- **Folder**: `human_disturbance/`
- **Source**: Derived from multiple inputs (roads, buildings, development)
- **Format**: GeoTIFF raster
- **Notes**: Normalized 0-1 scale, inverted for Zonation (1 = undisturbed)
- **Script**: `scripts/04_habitats/03_human_disturbance.R`

### Threat Data

#### Wildland-Urban Interface (WUI)
- **Folder**: `datasets/bc_wui/`
- **Source**: BC Wildfire Service
- **URL**: https://catalogue.data.gov.bc.ca/
- **Format**: Shapefile
- **Notes**: Fire risk zones
- **Script**: `scripts/06_threats/02_fire.R`

#### Development Potential
- **Folder**: `datasets/development_potential_danielmartin/`
- **Source**: Daniel Martin (consultant analysis)
- **Format**: Shapefile
- **Notes**: Developable land analysis
- **Script**: `scripts/06_threats/01_development.R`

### Historical Data

#### Pottinger Report (2005)
- **Folder**: `datasets/pottinger_report/`
- **Source**: Environmental Inventory for Bowen Island (2005)
- **Format**: PDF report, some spatial data
- **Notes**: Historical baseline, species lists (especially mammals)
- **Citation**: Pottinger, R. (2005). Environmental Inventory for Bowen Island.

#### Shoreline
- **Folder**: Derived from multiple sources
- **Notes**: Ocean polygon for mapping context
- **Script**: `scripts/02_foundation/03_bowen_shoreline.R`

## Data Not in Git

The following folders are excluded from version control due to size:

- `species_distribution_models/` - 341 raster files (~2GB)
- `datasets/dsm/` - High-resolution LiDAR (~500MB)
- `archive/` - Old/deprecated data versions

To reproduce the analysis, you must manually acquire these datasets.

## Required API Keys

### iNaturalist
- No API key required (public API)
- Rate-limited: Include 3-second delays between requests
- Script handles rate limiting automatically

### eBird
- API key required (free registration)
- Set in `.Renviron`: `EBIRD_KEY=your_key_here`
- Register at: https://ebird.org/api/keygen

### Google Sheets (for metadata)
- OAuth authentication required for reading species/habitat metadata
- Script will prompt for authentication on first run
- Credentials cached in `.secrets/`

## Data Update Procedures

### Updating SDMs
1. Check for new Popescu et al. releases
2. Download updated `.tif` files to `species_distribution_models/`
3. Re-run `scripts/03_species/01_process_sdm.R`
4. Update species count in documentation

### Updating Observations
1. iNaturalist/eBird data is queried fresh each run
2. To use cached observations, modify scripts to skip API calls
3. Archive old observation CSVs to `datasets/inat/archive/` or `datasets/ebird/archive/`

### Updating Protected Areas
1. Check BC Parks, ALR for boundary updates
2. Download new shapefiles
3. Re-run `scripts/02_foundation/05_bowen_protectedareas.R`

## File Format Specifications

### Shapefiles
- **CRS**: Original data in various CRS (EPSG:4326, EPSG:3005, etc.)
- **Reprojection**: All data reprojected to custom BC Albers (see `project_crs`)
- **Fields**: Vary by source - see individual script headers for expected attributes

### Rasters
- **Format**: GeoTIFF (.tif)
- **Resolution**: 100m x 100m (standardized for analysis)
- **CRS**: Custom BC Albers (matches `project_crs`)
- **Data Type**: Float32 for probabilities, Int16 for classifications
- **NoData**: -9999 or NA

### CSV Files
- **Encoding**: UTF-8
- **Delimiter**: Comma
- **Headers**: Required
- **Date Format**: YYYY-MM-DD

## Data Licensing and Usage

- **BC Government Data**: Open Government License - BC (https://www2.gov.bc.ca/gov/content/data/open-data/open-government-licence-bc)
- **Metro Vancouver Data**: Open Data License
- **iNaturalist**: CC-BY or CC0 (varies by observer)
- **eBird**: Terms of Use (https://www.birds.cornell.edu/home/ebird-data-access-terms-of-use/)
- **Consultant Data**: Used with permission (Alan Whitehead, Daniel Martin)
- **Popescu SDMs**: Academic use with citation

When publishing results, cite all data sources appropriately.

## Contact for Data Acquisition

- **Popescu SDMs**: Wendy Palen (wpalen@sfu.ca)
- **Bowen Island Municipal Data**: Bowen Island Municipality GIS
- **Consultant Data**: Respective consultants (see above)
- **BC/Metro Vancouver Open Data**: Available via catalogs (URLs above)
