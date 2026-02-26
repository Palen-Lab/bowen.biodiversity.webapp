# Raw Data (`data-1-raw/`)

Raw input datasets. Most spatial files are not committed to git due to size.

## Directory Structure

```
data-1-raw/
└── datasets/
    ├── boundary/
    ├── roads/
    ├── trails/
    ├── zoning/                             # BM_ZONING.shp
    ├── parcelmap_bowen/
    ├── protectedareas/                     # ALR, covenants, parks, OGMA, crown
    ├── inat/                               # iNaturalist export zip
    ├── bc_wui/                             # Wildland-Urban Interface
    ├── wetlands_whitehead_consultants/
    ├── fish_whitehead_consultants/
    └── development_potential_danielmartin/ # Subdivision potential xlsx
```

## Data Acquisition

| Dataset | Source | Access |
|---------|--------|--------|
| Species Distribution Models | Popescu et al. (2020) | Contact Wendy Palen (SFU) — Sharepoint: `BCH_project_backup/SPECIES/` |
| iNaturalist observations | iNaturalist export | Download from inaturalist.org (project: Bowen Island) |
| BC WUI | BC Wildfire Service | BC Data Catalogue |
| ParcelMap BC | BC Assessment | BC Data Catalogue |
| Wetlands / Fish streams | Alan Whitehead Consulting | Provided via email Apr 9, 2025 |
| Subdivision potential | Daniel Martin | Provided as xlsx |

## Not in Git

- `species_distribution_models/` — 341 rasters (~2 GB)
- High-resolution LiDAR DSM (~500 MB)

## Licensing

- BC Government data: Open Government License – BC
- Metro Vancouver data: Open Data License
- iNaturalist: CC-BY / CC0 (varies by observer)
- Consultant data: Used with permission
