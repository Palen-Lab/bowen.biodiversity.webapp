# Data Atlas

Quarto document producing the Bowen Island Biodiversity Data Atlas — a comprehensive map atlas for conservation planning.

## Files

| File | Description |
|------|-------------|
| `data_atlas.qmd` | Main atlas document |
| `_data_atlas_setup.qmd` | Shared setup chunk (libraries, theme) |
| `header.tex` | LaTeX header for PDF output |
| `reference.docx` | Word output style template |

## Rendering

```r
quarto::quarto_render("data-atlas/data_atlas.qmd")
```

Output figures (annotated and unannotated) are generated via the `targets` pipeline (`tar_make()`) and saved to `output-figures/data-atlas/`. The atlas document embeds these pre-rendered PNGs.

## Figure Outputs

Maps are saved to:
- `output-figures/data-atlas/annotated/` — with north arrow, scale bar, title
- `output-figures/data-atlas/unannotated/` — base map only (for layout flexibility)

All map targets follow the naming convention `{section}_{map_name}_{annotated|unannotated}` in `_targets.R`.
