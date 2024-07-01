library(purrr)
library(readr)
library(dplyr)
library(sf)

list.files("data/fielddata/")
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

dat |> 
  arrange(ptname) |> 
  write_csv("data/fielddata/concatenated.csv")
