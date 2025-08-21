library(tidyverse)
library(sf)

# Fields from Data Exchange Standard for Peat Surveys v1.0
# recordDate,surveyor,institution,location,country,latitude,longitude,coordinateUncertainty,geodeticDatum,locationRemarks,surfacePeatDepth,peatDepthResolution,probeType,probeReachedBottom

# Read in depths ####

# 2025-08-20: A handful of NA `depth_cm` values are added from `Note`-field. 
# 2025-08-20: Coordinates associated with human disturbance (e.g. roadside) have NA `depth_cm`

depth <- st_read("output/fielddata.gpkg", layer="depth")
depth |> 
  filter(is.na(depth_cm)) |> 
  nrow()
depth <- depth |> 
  filter(!is.na(depth_cm)) |> 
  st_transform(crs = "epsg:4326")

## probeReachedBottom ####

depth |> 
  arrange(desc(depth_cm)) |>
  filter(grepl("[A-Za-z]", note))
depth <- depth |> 
  mutate(probeReachedBottom = case_when(
    note == "400 kommer ikke lenger ned pga darlige spy punkt 25" ~ FALSE,
    TRUE ~ TRUE
  ))

## Coordinate uncertainty ####

qc <- read_csv("data/fielddata/QC.csv")

depth <- depth |> 
  left_join(qc, by = join_by(ptname == Name)) |> 
  select(-c(`Lat(North)`, `Lon(East)`, `Ht(G)`, VRMS, Time, `Solution Type`))

## Create standardized output ####

depth_standardized <- depth |> 
  mutate(
    recordDate = case_when(
      !is.na(Date) ~ as.character(Date),
      TRUE ~ "2024-06-11/2024-07-12"
    ),
    surveyor = "Henning Hov | Sander Nybak Urdal",
    institution = "Western Norway University of Applied Sciences",
    location = "Vestland County",
    country = "Norway",
    latitude = st_coordinates(geom)[,2],
    longitude = st_coordinates(geom)[,1],
    coordinateUncertainty = case_when(
      !is.na(HRMS) ~ HRMS,
      TRUE ~ 5
    ),
    geodeticDatum = "http://www.opengis.net/def/crs/EPSG/0/4326",
    surfacePeatDepth = depth_cm,
    peatDepthResolution = "3 cm",
    probeType = "peat probe",
    probeReachedBottom = probeReachedBottom
  ) |> 
  select(recordDate, surveyor, institution, location, country, 
         latitude, longitude, coordinateUncertainty, geodeticDatum, 
         surfacePeatDepth, peatDepthResolution, 
         probeType, probeReachedBottom) |> 
  st_drop_geometry() |> 
  tibble()

# Check standardized data ####

summary(depth_standardized)

standardized_pts <- depth_standardized |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = "epsg:4326")
plot(standardized_pts["recordDate"])
plot(standardized_pts["coordinateUncertainty"])
plot(standardized_pts["surfacePeatDepth"])
plot(standardized_pts["probeReachedBottom"])

## Export standardized data ####

write_csv(depth_standardized, "output/standardized_depths.csv")
