# Archived Analyses

This folder contains analysis documents that are no longer actively maintained but are kept for historical reference and comparison.

## Why Archive?

These analyses represent previous iterations of the Bowen Island biodiversity conservation planning workflow. They have been superseded by more recent, comprehensive analyses but provide valuable context for:

- Understanding the evolution of methods
- Comparing different Zonation parameter choices
- Tracking changes in data sources and weights
- Documenting workshop materials from specific dates

## Archived Files

### Zonation Iterations

#### `2025_02_01_output_zonation.qmd`
- **Date**: February 1, 2025
- **Purpose**: Zonation results for February 1 workshop with Caring4Nature Group
- **Key Features**:
  - Initial Zonation run with Metro Vancouver SEI habitats
  - 50/50 weighting between species (193) and habitats
  - Basic overlap analysis with protected areas
- **Superseded by**: `04_conservation_values.qmd` (May 2, 2025 version)
- **Why archived**: Used older habitat data sources, less refined weighting

#### `2025_04_17_output_zonation.qmd`
- **Date**: April 17, 2025
- **Purpose**: Updated Zonation run incorporating TEM data
- **Key Changes from Feb 1**:
  - Added TEM ecosystem classifications
  - Refined habitat layer preparation
  - Updated human disturbance layer
- **Superseded by**: `04_conservation_values.qmd`
- **Why archived**: Intermediate iteration, further refinements made

#### `2025_04_24_output_zonation.qmd`
- **Date**: April 24, 2025
- **Purpose**: Zonation run with adjusted weights
- **Key Changes**:
  - Experimented with different species:habitat weight ratios
  - Updated protected areas layer
- **Superseded by**: `04_conservation_values.qmd`
- **Why archived**: Experimental weighting scheme not adopted

### Workshop Materials

#### `feb1_maps.Rmd`
- **Date**: February 1, 2025
- **Purpose**: Generate maps for February 1 workshop
- **Contents**:
  - Species richness maps
  - Initial conservation priority maps
  - Protected area overlays
  - Large-format PDFs for printing
- **Superseded by**: `08_data_atlas.qmd`
- **Why archived**: Workshop-specific materials, now integrated into data atlas

#### `species_richness_plots.Rmd`
- **Date**: Early 2025
- **Purpose**: Generate species richness visualizations by taxon
- **Contents**:
  - Mammal, bird, reptile, amphibian richness
  - Threatened species overlays
- **Superseded by**: `supplementary/species_richness.qmd`
- **Why archived**: Rewritten as .qmd with updated data

### Experimental Approaches

#### `threats_development_abacusapi.Rmd`
- **Date**: 2025
- **Purpose**: Experimental approach using BC Assessment Abacus API
- **Contents**:
  - Direct API queries for property values
  - Development pressure index calculations
- **Superseded by**: `05_threats_development.qmd`
- **Why archived**: API approach replaced with static data download (more reliable)
- **Note**: May be useful if real-time property data needed in future

#### `climate_data.Rmd`
- **Date**: 2025
- **Purpose**: Climate change projections for Bowen Island
- **Contents**:
  - Temperature and precipitation scenarios
  - Climate envelope analysis
- **Status**: May be integrated into future threat analysis
- **Why archived**: Not yet integrated into main workflow

### Input Data Preparation

#### `input_mask.qmd`
- **Date**: Early 2025
- **Purpose**: Document creation of Bowen Island analysis mask from DSM
- **Contents**:
  - LiDAR DSM processing
  - Mask creation from elevation (exclude ocean)
  - Code for generating `bowen_mask.tif`
- **Superseded by**: `data-raw/scripts/01_setup/03_bowen_mask.R`
- **Why archived**: Executable code moved to script, documentation merged into `01_input_foundation.qmd`

## Using Archived Files

### To View Archived Analyses

Archived files are **not rendered** as part of the main documentation website. To view them:

1. **Open directly in RStudio/VS Code**
2. **Render individually**:
   ```r
   quarto::quarto_render("analyses/archive/2025_02_01_output_zonation.qmd")
   ```
3. **Compare outputs**: Check `analyses/outputs/` for any preserved figures

### To Compare Zonation Runs

Key questions when comparing archived Zonation iterations:

1. **Data Differences**: Which species/habitat layers were included?
2. **Weight Differences**: What species:habitat ratio was used?
3. **Performance**: How did priority areas differ?
4. **Protected Area Overlap**: Did coverage percentages change?

See `analyses/archive/2025_04_24_output_zonation.qmd` for sensitivity analysis comparing runs.

### To Resurrect Archived Code

If you need to use code from an archived file:

1. **Check for equivalent in current scripts**: Most code has been moved to `data-raw/scripts/`
2. **Update paths**: Change `vignettes/` → `analyses/`, `inst/extdata/` → `output-data/`, `figures/` → `output-figures/`
3. **Test thoroughly**: Data sources may have changed

## Archive Policy

Files are archived when:
- ✅ They represent completed workshop milestones (preserved as historical record)
- ✅ They've been superseded by more comprehensive analyses
- ✅ They contain experimental approaches not adopted in main workflow
- ✅ They're interim iterations of analyses with final versions available

Files are **not** archived when:
- ❌ They're still actively used in the current workflow
- ❌ They contain unique analysis not available elsewhere
- ❌ They're referenced in external publications or reports

## Future Archiving

As the project evolves, additional files may be archived here. When archiving:

1. Update this README with new entry
2. Remove from `_quarto.yml` render list
3. Consider extracting reusable code to scripts
4. Preserve any workshop-specific outputs to `output-documents/workshop_reports/`

## Questions?

For questions about archived analyses:
- **Historical context**: Contact Jay Matsushiba
- **Technical details**: Check git history for commit messages
- **Zonation methods**: See current `04_conservation_values.qmd` for latest approach
