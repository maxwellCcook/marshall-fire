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
library(lubridate)
library(scales)
library(ggpubr)
library(grid)
library(forcats)
#################
# ENVIRONMENTS
setwd("C:/Users/mccoo/OneDrive/mcook/marshall-fire/")
#################
# DATA
# ~~~~~~~
# Load the ICS-209-PLUS spatial points
ics <- st_read("../ics209/data/spatial/ics209plus_conus_incidents_spatial_1999to2020.gpkg") %>%
  mutate(START_MONTH = as.factor(month(DISCOVERY_DATE, label=T)),
         START_WEEK = as.factor(week(DISCOVERY_DATE)),
         MTBS_ID = gsub("\\s*\\([^\\)]+\\)","", LRGST_MTBS_FIRE_INFO),
         MTBS_FIRE_NAME = stringr::str_extract(string = LRGST_MTBS_FIRE_INFO,
                                               pattern = "(?<=\\().*(?=\\))"))
# Read in QC'd ICS-WEST
ics.west <- read.csv('../ics209/data/ics209plus_wf_incidents_west_1999to2020_edit_qc.csv')

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
hull <- st_convex_hull(evt) %>% st_buffer(250)

# Spatial filter on ztrax
ztrax.res.evt <- st_intersection(ztrax.res, evt) %>% st_transform(st_crs(ztrax.res))
ztrax.res.hull <- st_intersection(ztrax.res, hull) %>% st_transform(st_crs(ztrax.res))

# Boudler County
boco <- st_read("data/spatial/boulder_county.gpkg")
cities <- st_read("data/spatial/boco_major_cities.gpkg") %>%
  st_transform(st_crs(boco))
boco.cities <- st_intersection(cities, boco)
counties <- st_read("data/spatial/tl_2019_us_county_west.gpkg") 
co.co <- counties %>%
  filter(STATEFP == "08")

# MTBS perimeters
mtbs <- st_read("C:/Users/mccoo/OneDrive/mcook/data/mtbs/mtbs_perims_DD/mtbs_perims_DD.gpkg") %>% 
  mutate(
    MTBS_DATE = as.Date(Ig_Date, "%Y-%m-%d"),
    MTBS_YEAR = format(Ig_Date, format = "%Y"),
    MTBS_ID = Event_ID,
    MTBS_FIRE_NAME = Incid_Name,
    MTBS_ACRES = BurnBndAc) %>% 
  dplyr::select(MTBS_DATE, MTBS_YEAR, MTBS_FIRE_NAME, MTBS_ID, MTBS_ACRES) %>%
  st_transform(st_crs(ics))
