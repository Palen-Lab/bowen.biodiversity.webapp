# Analysis Documentation

This folder (renamed from `vignettes/` to `analyses/`) contains Quarto/RMarkdown documents that explain the biodiversity analysis workflow, methods, and results.

## Purpose

These documents serve as:
1. **Documentation** - Explain methods, data sources, and decisions
2. **Reproducibility** - Show how outputs were generated
3. **Communication** - Present results to stakeholders and community
4. **Web Content** - Rendered as website via Quarto

## Organization

### Current Analyses (Main Pipeline)

These documents follow the sequential workflow:

1. **01_input_foundation.qmd** (NEW)
   - Foundation data layers (boundary, roads, trails, etc.)
   - Documents output from `data-raw/scripts/01_setup/` and `02_foundation/`

2. **02_input_species.qmd** (renamed from `input_sdm.qmd`)
   - Species selection criteria
   - 341 → 193 species filtering process
   - iNaturalist/eBird validation
   - Related script: `data-raw/scripts/03_species/`

3. **03_input_habitats.qmd** (renamed from `input_habitats.qmd`)
   - TEM, SEI, consultant habitat data
   - ~50 habitat layer preparation
   - Human disturbance layer
   - Related script: `data-raw/scripts/04_habitats/`

4. **04_conservation_values.qmd** (renamed from `2025_05_02_output_zonation.qmd`)
   - Zonation conservation prioritization
   - Top 10%, 20%, 30% priority areas
   - Overlap with protected areas and private land
   - Related script: `data-raw/scripts/05_zonation/`

5. **05_threats_development.qmd** (renamed from `threats_development.qmd`)
   - Development pressure analysis
   - BC Assessment data
   - Developable land overlay with priorities
   - Related script: `data-raw/scripts/06_threats/01_development.R`

6. **06_threats_fire.qmd** (renamed from `threats_fire.qmd`)
   - Wildfire risk (WUI zones)
   - Fire vulnerability assessment
   - Related script: `data-raw/scripts/06_threats/02_fire.R`

7. **07_protected_areas.qmd** (renamed from `protected_areas.qmd`)
   - Current protected area extent
   - Gap analysis
   - Recommendations for new protected areas
   - Related to multiple foundation scripts

8. **08_data_atlas.qmd** (renamed from `2025_05_07_data_atlas.qmd`)
   - Final comprehensive data atlas
   - All maps and figures for publication
   - Integration of all previous analyses

### Supplementary Analyses

Located in `supplementary/` subfolder:

- **species_richness.qmd** - Species richness by taxon and threat status
- **tree_density.qmd** (renamed from `tree_density_from_lidar.qmd`) - LiDAR-derived tree density
- **lidar_processing.qmd** (renamed from `lidar_dem.qmd`) - LiDAR data processing methods
- **inaturalist.qmd** - iNaturalist data exploration

### Archived Analyses

Located in `archive/` subfolder with explanatory README:

- **2025_02_01_output_zonation.qmd** - Feb 1 workshop Zonation results
- **2025_04_17_output_zonation.qmd** - April 17 Zonation iteration
- **2025_04_24_output_zonation.qmd** - April 24 Zonation iteration
- **feb1_maps.Rmd** - Feb 1 workshop maps (superseded by data_atlas)
- **species_richness_plots.Rmd** - Old species richness plots
- **threats_development_abacusapi.Rmd** - Experimental BC Assessment API approach
- **climate_data.Rmd** - Climate change analysis (may be integrated elsewhere)

These are kept for historical reference and comparison but are no longer actively maintained.

### Other Files

- **downloads.qmd** - Instructions for downloading required data
- **zonation/** - Zonation configuration files (`.dat`, `.spp`, `.txt`)
- **outputs/** - Temporary analysis outputs (not committed to git)

## Relationship Between Scripts and Analyses

**ETL Scripts** (`data-raw/scripts/`) contain executable code:
- Load raw data
- Process and transform
- Generate outputs (`output-data/`, `output-figures/`)
- Minimal documentation

**Analyses** (`analyses/`) contain documentation:
- Explain methods and rationale
- Show example outputs and visualizations
- Interpret results
- Link to corresponding scripts
- NO executable ETL code (moved to scripts)

## Rendering the Website

### Local Preview
```r
quarto::quarto_preview()
```

### Build Full Site
```r
quarto::quarto_render()
```

Output website is in `_site/` directory.

### Configuration
- **_quarto.yml** - Defines which files to render and sidebar structure
- **index.qmd** - Landing page
- Only files listed in `_quarto.yml` are rendered (archived files excluded)

## Output Organization

Analyses generate outputs in three top-level directories:

1. **output-data/** - Processed spatial datasets
   - Species SDM rasters
   - Habitat layers
   - Zonation outputs
   - Threat layers

2. **output-figures/** - All generated maps and visualizations
   - Organized by category (species/, habitats/, conservation/, etc.)
   - Referenced in analyses via relative paths: `../output-figures/`

3. **output-documents/** - Final reports and deliverables
   - Data atlas PDF
   - Workshop materials
   - Technical reports

## Editing Analyses

### When Editing Existing Documents:

1. Update text, methods, interpretation as needed
2. If adding new visualizations, save figures to `output-figures/` (not `figures/`)
3. Use relative paths for figures: `![](../output-figures/category/figure.png)`
4. If data processing changes, update corresponding script in `data-raw/scripts/`
5. Update `_quarto.yml` if changing filenames

### Creating New Documents:

1. Add to appropriate location (main workflow vs. supplementary)
2. Follow numbering convention if part of main pipeline
3. Include standard sections:
   - Overview
   - Data Sources
   - Processing Scripts (link to `data-raw/scripts/`)
   - Methods
   - Results
   - Outputs (what files are generated)
4. Add to `_quarto.yml` render list and sidebar

## Stub Files

- **threats_invasive_species.qmd** - Placeholder (TODO: complete analysis)
- **threats_logging.qmd** - Placeholder (TODO: complete analysis)

These may be removed or completed in future iterations.

## Figure Paths

**OLD** (before refactor):
```markdown
![Figure](figures/species/bear.png)
```

**NEW** (after refactor):
```markdown
![Figure](../output-figures/species/bear.png)
```

All figure paths must be updated to reference `output-figures/` at the top level.

## Code Execution

Documents use `freeze: true` in `_quarto.yml` to cache code execution:
- Code is not re-executed on every render (faster)
- To force re-execution, delete `_freeze/` directory or set `freeze: false`
- Heavy computations should be in scripts, not analysis documents

## Dependencies

Documents depend on:
- Processed data in `output-data/`
- Figures in `output-figures/`
- R package functions in `R/plots.R`
- Spatial data objects in `data/` (`.rda` files)

Run ETL scripts first to generate required inputs.

## Accessibility

When writing analyses:
- Use descriptive alt text for figures
- Ensure color palettes are colorblind-friendly
- Structure headings hierarchically (## → ### → ####)
- Provide text descriptions of visual results

## Questions?

For questions about analysis documentation:
- See main README: [../README.md](../README.md)
- Check data sources: [../data-raw/README.md](../data-raw/README.md)
- Review scripts: `data-raw/scripts/`
