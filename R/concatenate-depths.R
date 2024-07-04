library(tidyverse)
library(sf)

list.files("data/fielddata/")

# From DGPS ####

readfiles <- list.files("data/fielddata/", pattern = '[0-9].txt$', full.names = TRUE)
names(readfiles) <- basename(readfiles)
dat <- map(readfiles, \(x) read_csv(x, col_names = FALSE, col_types = 'ddddcd'))
dat <- bind_rows(dat, .id="file")
dat <- select(dat, file=file, ptname =X1, utmN=X2, utmE=X3, z=X4, note=X5) |> 
  arrange(note)

extract_starting_digits <- function(text_vector) {
  # Use a regular expression to match up to three digits at the beginning of the text
  matches <- stringr::str_extract(text_vector, "^\\d{1,3}")
  
  # Convert the matches to numeric, will return NA for non-matches (non-numeric strings or strings not starting with digits)
  as.numeric(matches)
}
# Example usage
extract_starting_digits(c("123abc", "45def", "no digits", "9991 number", "007James"))

dat <- dat |> 
  mutate(depth_cm = extract_starting_digits(note))
tail(dat)
filter(dat, is.na(depth_cm))

boxplot(dat$depth_cm)

dat <- dat |>
  st_as_sf(coords = c("utmE","utmN"), crs="epsg:25833") |> 
  select(file, ptname, depth_cm, note)

# Without DGPS ####

dat2 <- read_csv("data/fielddata/ManglendePunkter.csv")
clhspts <- st_read('output/ExportToMagnetField/clhs_draped.kml') |> 
  mutate(Name = as.numeric(Name)) |> 
  select(-Description)
#filter(dat2, punkt == 59)
#st_coordinates(filter(clhspts, Name == 59))

dat2 <- left_join(dat2, clhspts, by = join_by(punkt == Name)) |> 
  st_as_sf() |> 
  st_transform(crs = "epsg:25833")

dat2 <- dat2 |> 
  st_zm(drop = TRUE) |> 
  pivot_longer(cols = 2:5, names_to = "retning", values_to = "depth_cm")
offsetscalar <- 3.5/sqrt(2) 
dat2 <- dat2 |> 
  mutate(
    newx = case_when(
      retning == 'NordØst' | retning == 'SørØst' ~ st_coordinates(geometry)[,"X"] + offsetscalar,
      retning == 'NordVest' | retning == 'SørVest' ~ st_coordinates(geometry)[,"X"] - offsetscalar),
    newy = case_when(
      retning == 'NordØst' | retning == 'NordVest' ~ st_coordinates(geometry)[,"Y"] + offsetscalar,
      retning == 'SørØst' | retning == 'SørVest' ~ st_coordinates(geometry)[,"Y"] - offsetscalar)
  )
dat2 <- dat2 |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("newx","newy"), crs="epsg:25833") |> 
  mutate(file = "ManglendePunkter.csv") |> 
  select(file, depth_cm, note = kommentar)

# All ####

bind_rows(dat, dat2) |> 
  filter(!(is.na(depth_cm) & is.na(note))) |> 
  arrange(file, ptname) |> 
  st_write("output/fielddata.gpkg", layer="depth", append=FALSE)
