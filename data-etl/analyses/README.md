# Analyses

Quarto documents documenting methods, results, and visualizations for the Bowen Island biodiversity analysis. Rendered as a Quarto website.

## Rendering

```r
quarto::quarto_preview()  # Preview locally
quarto::quarto_render()   # Build _site/
```

## Documents

| File | Description |
|------|-------------|
| `02_input_species.qmd` | SDM species selection (341 → 193 species) |
| `03_input_habitats.qmd` | Habitat layer preparation (TEM, SEI, consultants) |
| `04_conservation_values.qmd` | Zonation prioritization results |
| `05_threats_development.qmd` | Development pressure analysis |
| `06_threats_fire.qmd` | Wildfire vulnerability and WUI |
| `07_protected_areas.qmd` | Gap analysis and candidate protected areas |

Supplementary analyses (species richness, LiDAR, tree density) are in `supplementary/`.
Archived prior iterations are in `archive/`.

## Data Access

Documents load pipeline outputs via `tar_read()` / `tar_load()`. Run `targets::tar_make()` before rendering if pipeline outputs are stale.

## Configuration

- `_quarto.yml` — sidebar structure and render list
- `freeze: auto` — code only re-executes when inputs change
