## code to prepare `bowen_protectedareas` dataset goes here
library(dplyr)
library(sf)
library(foreach)
library(here)
# Bowen Protected Areas originally prepared and provided by John Dowler:
# https://drive.google.com/drive/folders/1uO5FjvHfCnixZyF6UHcI4YUp1V2tv2kg
dir <- here("data-raw/bowen_protectedareas")
files <- list.files(dir, full.names = T)

# Covenants
covenants_path <- "data-raw/bowen_protectedareas/BI-Covenant-Catalogue-Layer 1.3.2.gpkg"
covenants <- here(covenants_path) %>%
  sf::st_read() %>%
  dplyr::select(name = Document.No.)
covenants <- covenants[!st_is_empty(covenants$geom), ] # Remove invalid, empty rows
covenants$type <- "Covenants"
covenants$filepath <- covenants_path

# Agricultural Land Reserve
alr_path <- "data-raw/bowen_protectedareas/Bowen-ALR-Lands-JD.gpkg"
alr <- here(alr_path) %>%
  sf::st_read() %>%
  dplyr::select() %>%
  dplyr::mutate(name = paste0("ALR_", row_number()))
alr <- alr[!st_is_empty(alr$geom), ] # Remove invalid, empty rows
alr$type <- "Agricultural Land Reserve"
alr$filepath <- alr_path

# Conservancies
conservancies_path <- "data-raw/bowen_protectedareas/Bowen-Conservancies-JD.gpkg"
conservancies <- here(conservancies_path) %>%
  sf::st_read() %>%
  select(name = NAME)
conservancies <- conservancies[!st_is_empty(conservancies$geom),]
conservancies$type <- "Conservancies"
conservancies$filepath <- conservancies_path

# Metro Vancouver Parks
mvparks_path <- "data-raw/bowen_protectedareas/Bowen-Metro-Parks-JD.gpkg"
mvparks <- here(mvparks_path) %>%
  st_read() %>%
  select(name = NAME)
mvparks <- mvparks[!st_is_empty(mvparks$geom),]
mvparks$type <-"MetroVancouver Park"
mvparks$filepath <- mvparks_path

# Municipal Parks
bowenmuniparks_path <- "data-raw/bowen_protectedareas/Bowen-Muni-Parks-JD.gpkg"
bowenmuniparks <- here(bowenmuniparks_path) %>%
  st_read() %>%
  select(name = NAME)
bowenmuniparks <- bowenmuniparks[!st_is_empty(bowenmuniparks$geom),]
bowenmuniparks$type <-"Bowen Island Municipality Park"
bowenmuniparks$filepath <- bowenmuniparks_path

# Old Growth Management Areas
# - leave out for now

# Provincial Parks
provparks_path <- "data-raw/bowen_protectedareas/Bowen-Prov-Parks-JD.gpkg"
provparks <- here(provparks_path) %>%
  st_read() %>%
  dplyr::select(name = parkname)
provparks <- provparks[!st_is_empty(provparks$geom),]
provparks$type <-"Provincial Park"
provparks$filepath <- provparks_path

# # Crown Land
# crown_path <- "data-raw/bowen_protectedareas/Crown-Land-JD.gpkg"
# crown <- here(crown_path) %>%
#   st_read() %>%
#   select(name = parkname)
# crown <- crown[!st_is_empty(crown$geom),]
# crown$type <-"Crown Land"
# crown$filepath <- crown_path

# Ecological Reserves
ecoreserve_path <- "data-raw/bowen_protectedareas/Eco-Reserve-JD.gpkg"
ecoreserve <- here(ecoreserve_path) %>%
  st_read() %>%
  select(name = parkname)
ecoreserve <- ecoreserve[!st_is_empty(ecoreserve$geom),]
ecoreserve$type <-"Ecological Reserve"
ecoreserve$filepath <- ecoreserve_path

# Combine into one Protected Areas layer
bowen_protectedareas <- rbind(alr, bowenmuniparks, conservancies, covenants, crown, ecoreserve, mvparks, provparks)

usethis::use_data(bowen_protectedareas, overwrite = TRUE)
