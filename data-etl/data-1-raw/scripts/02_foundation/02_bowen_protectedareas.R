## code to prepare `bowen_protectedareas` dataset goes here
library(dplyr)
library(sf)
library(foreach)
library(here)
library(magrittr)
# Bowen Protected Areas originally prepared and provided by John Dowler:
# https://drive.google.com/drive/folders/1uO5FjvHfCnixZyF6UHcI4YUp1V2tv2kg
dir <- here("data-raw/datasets/protectedareas")
files <- list.files(dir, full.names = T)

# Covenants
covenants_path <- "data-raw/datasets/protectedareas/BI-Covenant-Catalogue-Layer 1.3.2.gpkg"
covenants <- here(covenants_path) %>%
  sf::st_read() %>%
  dplyr::select(name = Document.No.)
covenants <- covenants[!st_is_empty(covenants$geom), ] # Remove invalid, empty rows
covenants$type <- "Covenants"
covenants$filepath <- covenants_path

# Agricultural Land Reserve
alr_path <- "data-raw/datasets/protectedareas/Bowen-ALR-Lands-JD.gpkg"
alr <- here(alr_path) %>%
  sf::st_read() %>%
  dplyr::select() %>%
  dplyr::mutate(name = paste0("ALR_", row_number()))
alr <- alr[!st_is_empty(alr$geom), ] # Remove invalid, empty rows
alr$type <- "Agricultural Land Reserve"
alr$filepath <- alr_path

# Conservancies
conservancies_path <- "data-raw/datasets/protectedareas/Bowen-Conservancies-JD.gpkg"
conservancies <- here(conservancies_path) %>%
  sf::st_read() %>%
  select(name = NAME)
conservancies <- conservancies[!st_is_empty(conservancies$geom),]
conservancies$type <- "Conservancies"
conservancies$filepath <- conservancies_path

# Metro Vancouver Parks
mvparks_path <- "data-raw/datasets/protectedareas/Bowen-Metro-Parks-JD.gpkg"
mvparks <- here(mvparks_path) %>%
  st_read() %>%
  select(name = NAME)
mvparks <- mvparks[!st_is_empty(mvparks$geom),]
mvparks$type <-"MetroVancouver Park"
mvparks$filepath <- mvparks_path

# Municipal Parks
bowenmuniparks_path <- "data-raw/datasets/protectedareas/Bowen-Muni-Parks-JD.gpkg"
bowenmuniparks <- here(bowenmuniparks_path) %>%
  st_read() %>%
  select(name = NAME)
bowenmuniparks <- bowenmuniparks[!st_is_empty(bowenmuniparks$geom),]
bowenmuniparks$type <-"Bowen Island Municipality Park"
bowenmuniparks$filepath <- bowenmuniparks_path

# Provincial Parks
provparks_path <- "data-raw/datasets/protectedareas/Bowen-Prov-Parks-JD.gpkg"
provparks <- here(provparks_path) %>%
  st_read() %>%
  dplyr::select(name = parkname)
provparks <- provparks[!st_is_empty(provparks$geom),]
provparks$type <-"Provincial Park"
provparks$filepath <- provparks_path

# Ecological Reserves
ecoreserve_path <- "data-raw/datasets/protectedareas/Eco-Reserve-JD.gpkg"
ecoreserve <- here(ecoreserve_path) %>%
  st_read() %>%
  select(name = parkname)
ecoreserve <- ecoreserve[!st_is_empty(ecoreserve$geom),]
ecoreserve$type <-"Ecological Reserve"
ecoreserve$filepath <- ecoreserve_path

# Combine into one Protected Areas layer
bowen_protectedareas <- rbind(alr, bowenmuniparks, conservancies, covenants, ecoreserve, mvparks, provparks) %>%
  st_transform(project_crs) %>%
  st_make_valid()
usethis::use_data(bowen_protectedareas, overwrite = TRUE)

# Dissolve protected areas to difference with the crown land
# Turn into a single multipolygon, because don't want to have multiple difference with each row
dissolved_protectedareas <- bowen_protectedareas %>%
  summarise() %>%
  st_cast() %>%
  st_buffer(1)
usethis::use_data(dissolved_protectedareas, overwrite = TRUE)

# Old Growth Management Areas
ogma_path <- "data-raw/datasets/protectedareas/Bowen-OGMAs-JD.gpkg"
ogma <- here(ogma_path) %>%
  st_read() %>%
  st_transform(project_crs) %>%
  select(name = NAME) %>%
  st_make_valid()
ogma <- ogma[!st_is_empty(ogma$geom),]
ogma$type <-"Old Growth Management Area"
ogma$filepath <- ogma_path
usethis::use_data(ogma, overwrite = TRUE)

# Crown Land
crown_path <- "data-raw/datasets/protectedareas/Crown-Land-JD.gpkg"
crown <- here(crown_path) %>%
  st_read() %>%
  st_transform(project_crs) %>%
  select(name = parkname)
crown <- crown[!st_is_empty(crown$geom),] %>%
  st_snap(x = ., y = ., tolerance = 0.1) %>%
  st_make_valid()
crown$type <-"Crown Land"
crown$filepath <- crown_path
usethis::use_data(crown, overwrite = TRUE)

# Unprotected Crown Land

# Difference with crown land and protected areas, get unprotected crown land
unprotected_crown <- st_difference(crown, dissolved_protectedareas)

# Manually check each crown land polygon
for(i in 1:nrow(unprotected_crown)) {
  plot(unprotected_crown[i,])
}
# Remove polygons that should not be included in notprotected areas
# rowname 1 (Fairy Fen Natural Reserve), sliver
# rowname 74 (Seymour Bay Park), wrong polygon in the first place
unprotected_crown <- unprotected_crown[!rownames(unprotected_crown) %in% c("1","74"),]
usethis::use_data(unprotected_crown, overwrite = TRUE)

