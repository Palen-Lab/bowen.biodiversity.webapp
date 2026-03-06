# UI Redesign — Feature Tracking

Redesign the web application from a multi-view panel system to a single map view
with a `bslib::accordion()` sidebar, where each thematic area has one panel and
layers are toggled on/off by checkboxes.

---

## 1. Layout — `app_ui.R`

- [ ] Remove the mini icon sidebar (`bslib::sidebar` with `mini_sidebar_btn` buttons)
- [ ] Replace the inner `tabsetPanel` (hidden type) with a single `bslib::accordion()`
      inside the main sidebar
- [ ] Add an **"About"** `bslib::nav_item()` / `actionLink` to the `page_navbar` top
      bar that reopens the welcome modal
- [ ] Delete the `"Start"` tab panel from the `tabsetPanel` / accordion

---

## 2. Welcome Modal — `mod_start.R` → `app_server.R`

- [ ] Convert `mod_start_ui` content into a `bslib::modal()` / `shiny::modalDialog()`
- [ ] Auto-show the modal on app load using `shiny::showModal()` in `app_server`
      (inside `session$onFlushed` or at the top of the server)
- [ ] Wire the navbar "About" button to `shiny::showModal()` to reopen it
- [ ] Remove `mod_start_server` (map-clearing logic no longer needed on a tab switch)
- [ ] Remove `mod_start_ui` (UI now lives inline as a `modalDialog`)

---

## 3. Sidebar — accordion panels

Each panel replaces one existing module. The checkbox inside each panel toggles the
layer on/off; the panel body also holds contextual info (currently in the module sidebar).

### 3a. Species — replaces `mod_species.R`

- [ ] Add accordion panel: **"Species"** (`icon("dove")`)
- [ ] `selectInput` for species group (all / threatened / birds / other) — same options
      as current `mod_species_ui`
- [ ] `checkboxInput("species_show", "Show layer", value = FALSE)` — checking it loads
      and displays the raster; unchecking clears it
- [ ] Server: observe checkbox + select to add/clear raster image on the map
- [ ] Preserve existing colour palette and legend logic from `mod_species_server`

### 3b. Habitats — replaces `mod_habitats.R`

- [ ] Add accordion panel: **"Habitats"** (`icon("tree")`)
- [ ] Retain the two-level `selectInput` (habitat group → specific layer) from
      `mod_habitats_ui`
- [ ] `checkboxInput("habitats_show", "Show layer", value = FALSE)`
- [ ] Server: observe checkbox + selects to add/clear raster image
- [ ] Preserve subselectGroup dynamic UI and colour ramp logic

### 3c. Human Footprint — replaces `mod_people.R`

- [ ] Add accordion panel: **"Human Footprint"** (`icon("person")`)
- [ ] `selectInput` for layer (Human Footprint Index / iNaturalist Observations)
- [ ] `checkboxInput("people_show", "Show layer", value = FALSE)`
- [ ] Server: observe checkbox + select to add/clear raster image

### 3d. Conservation Values — replaces `mod_values.R`

- [ ] Add accordion panel: **"Conservation Values"** (`icon("chart-simple")`)
- [ ] `checkboxInput("values_show", "Show layer", value = FALSE)`
- [ ] `sliderInput` for top % threshold — same as current `mod_values_ui`
- [ ] `selectInput` for overlay layer (same options as current module)
- [ ] Server: observe checkbox + slider + overlay select to update raster/polygons

### 3e. Threats — replaces `mod_threats.R`

- [ ] Add accordion panel: **"Threats"** (`icon("triangle-exclamation")`)
- [ ] `selectInput` for threat layer (wildfire index / wildland-urban interface /
      development pressure) — same options as current `mod_threats_ui`
- [ ] `checkboxInput("threats_show", "Show layer", value = FALSE)`
- [ ] Server: observe checkbox + select to add/clear raster or polygon layer

### 3f. Protected Areas — replaces `mod_protected_areas.R`

- [ ] Add accordion panel: **"Protected Areas"** (`icon("seedling")`)
- [ ] `checkboxInput("pa_show", "Show existing protected areas", value = FALSE)`
- [ ] `checkboxInput("pa_candidates_show", "Show candidate protected areas", value = FALSE)`
- [ ] Retain the protected-area table and expand-PA checkbox from `mod_protected_areas_ui`
- [ ] Server: observe each checkbox independently to add/clear polygon layers

### 3g. Overlay — replaces `mod_overlay.R`

- [ ] Add accordion panel: **"Overlay"** (`icon("layer-group")`)
- [ ] Retain layer select inputs and top-% slider from `mod_overlay_ui`
- [ ] `checkboxInput("overlay_show", "Show overlay", value = FALSE)`
- [ ] Server: observe checkbox + selects to compute and display overlay polygons/rasters
- [ ] Retain existing overlap polygon logic

---

## 4. Server — `app_server.R`

- [ ] Remove all `observeEvent` blocks that switch the active panel (`r$active_panel`)
- [ ] Remove the `updateTabsetPanel` call and mini-sidebar highlight logic
- [ ] Remove calls to `mod_start_server`, `mod_species_server`, `mod_habitats_server`,
      etc. that were triggered on panel switch — server logic moves into each accordion
      module or inline `observe`/`observeEvent` blocks
- [ ] Add `showModal(welcome_modal())` on app start
- [ ] Add `observeEvent` for the "About" navbar button to `showModal`

---

## 5. Module cleanup

- [ ] Delete `mod_start.R` (UI/server replaced by modal inline in app_server/app_ui)
- [ ] Decide whether to keep existing `mod_*.R` files as the server logic home or
      inline everything into `app_server.R` — **recommendation:** keep as modules,
      update signatures to remove `map_id` / `parent_session` tab-switching args
- [ ] Remove `mod_layered_select_template.R` if it is fully absorbed into `mod_habitats`
      or replaced by accordion pattern (audit first)

---

## 6. CSS / styling

- [ ] Remove `.mini_sidebar_btn` and `.mini_sidebar_heading` CSS rules (no longer needed)
- [ ] Adjust sidebar width if needed for accordion layout
- [ ] Ensure map height `calc(100vh - 90px)` still correct with new navbar

---

## Notes

- All layers start **unchecked** (nothing shown on load except base tiles + boundary /
  roads / trails / ocean).
- Multiple layers from different panels can be visible simultaneously.
- Accordion panels can be open or closed independently of whether their layer is shown.
- The `bslib::accordion()` `multiple = TRUE` argument should be set so several panels
  can be open at once.
