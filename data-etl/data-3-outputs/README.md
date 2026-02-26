# Final Outputs (`data-3-outputs/`)

Processed spatial datasets and analysis results. 

## Directory Structure

```
data-3-outputs/
├── 5_values/
│   └── rankmap.tif           # Zonation conservation priority (0–1)
├── 6_threats/
│   └── fire_index_40m.tif    # Wildfire Vulnerability Index
└── 7_protected_areas/
    └── new_protected_areas.gpkg  # Candidate protected areas
```

## Key Files

| File | Description | Loaded by |
|------|-------------|-----------|
| `5_values/rankmap.tif` | Zonation rankmap — relative biodiversity value per cell | `tar_terra_rast(rankmap, ...)` |
| `6_threats/fire_index_40m.tif` | Wildfire Vulnerability Index raster | `tar_terra_rast(fire_index, ...)` |
| `7_protected_areas/new_protected_areas.gpkg` | Candidate new protected areas | `tar_target(pa_candidates, ...)` |

## Notes

- Zonation is run externally (Zonation5 CLI) from inputs in `analyses/04_conservation_values.qmd`
- Intermediate processed data lives in `data-2-processed/` (also not committed)
- All rasters are 100m resolution, projected to BC Albers custom CRS
