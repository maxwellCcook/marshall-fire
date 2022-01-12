#################
# LIBRARIES
library(tidyverse)
library(sf)
library(stringr)
library(brms)
library(cmdstanr)
library(performance)
library(bayesrules)
library(bayesplot)
library(tidybayes)
library(vctrs)
library(data.table)
library(magrittr)
#################
# ENVIRONMENTS
setwd("C:/Users/mccoo/OneDrive/mcook/marshall-fire/")
#################
# DATA
# ~~~~~~~
# Damage assessment linked to ZTRAX
# Residential only
dmg.res <- st_read("data/spatial/damage_records_geocoded_matched.gpkg") %>%
  filter(TYPE == "Residential") %>%
  mutate(
    STATUS = as.factor(STATUS),
    # Match street names
    PropertyStreetName = str_to_upper(PropertyStreetName),
    STR_MATCH = str_detect(STREET_NAME, PropertyStreetName),
    # Column data types
    PropertyStreetName = str_to_upper(PropertyStreetName),
    PropertyZoningDescription = as.double(PropertyZoningDescription),
    PropertyAddressConfidenceScore = as.double(PropertyAddressConfidenceScore),
    PropertyAddressCBSACode = as.double(PropertyAddressCBSACode),
    PropertyAddressCBSADivisionCode = as.double(PropertyAddressCBSADivisionCode),
    YearRemodeled = as.double(YearRemodeled),
    BuildingConditionStndCode = as.double(BuildingConditionStndCode),
    BuildingClassStndCode = as.double(BuildingClassStndCode),
    RoofStructureTypeStndCode = as.double(RoofStructureTypeStndCode),
    TotalKitchens = as.double(TotalKitchens),
    FoundationTypeStndCode = as.double(FoundationTypeStndCode)) %>%
  # Filter for matching street names
  filter(STR_MATCH == TRUE) %>%
  dplyr::select(-STR_MATCH)

# Boulder County ZTRAX (2021)
ztrax.res <- st_read("data/spatial/ztrax_2021_extraction_county_properties_08013.gpkg") %>%
  # Residential only
  filter(grepl("RR",PropertyLandUseStndCode)|grepl("RI",PropertyLandUseStndCode),
         !RowID %in% dmg.res$RowID) %>%
  # Merge with damage assessments
  bind_rows(., dmg.res) %>%
  mutate(DAMAGE = as.factor(if_else(is.na(STATUS), "Intact", as.character(STATUS))))
summary(ztrax.res$DAMAGE)

# Fire perimeter and convex hull
evt <- st_read("data/spatial/marshall_fire_boundary_nifc.gpkg") %>%
  st_transform(st_crs(ztrax.res)) %>%
  dplyr::select(poly_Incid)
# Convex hull of boundary
hull <- st_convex_hull(evt) %>% st_buffer(100)
# Spatial filter on ztrax
ztrax.res.evt <- st_intersection(ztrax.res, evt)
ztrax.res.hull <- st_intersection(ztrax.res, hull)

