library(tidyverse)
library(sf)
library(terra)
library(clhs)

depthpts <- st_read("output/fielddata.gpkg", layer="depth")
sa <- rast("output/samplingMatrix.tif")

# Clean raw data set ####
# remove non peat depth and add 0 depth where only noted 
depthpts <- depthpts |> 
  mutate(depth_cm = case_when(
    note == "7 meter unna punkt som ligger i elv" ~ NA,
    note == "18, veikant grus" ~ 0,
    note == "Alt 0, på stein" ~ 0,
    note == "fjellvegg, ikke myr" ~ 0,
    note == "gdpsen slet i skogen, men vi tror dette er punktet. ikke myr, men stor myr ved siden av" ~ 0,
    note == "grus langs vei. ikke myr" ~ 0,
    note == "grus ved veikant, ikke myr" ~ 0,
    note == "grus ved veikant. ikke myr" ~ 0,
    note == "grus ved veikanten, ikke myr" ~ 0,
    note == "grus. veikant" ~ 0,
    note == "ikke myr, ved veikant" ~ 0,
    note == "ikke torv, sannsynligvis omplassert masse.vei" ~ 0,
    note == "stein, ikke myr" ~ 0,
    note == "steinroys, ikke myr" ~ 0,
    TRUE ~ depth_cm
  ))

depthpts <- filter(depthpts, !is.na(depth_cm))
hist(depthpts$depth_cm)

# Remove pseudo-replication by multiple measurements per cell ####
depthcells <- mutate(depthpts, sacell = cellFromXY(sa, st_coordinates(depthpts))) |> 
  st_drop_geometry() |> 
  group_by(sacell) |> 
  summarize(depth_cm = mean(depth_cm))
hist(depthcells$depth_cm)
                     
# Create model frame ####
df <- bind_cols(depthcells, extract(sa, depthcells$sacell)) |> 
  select(depth_cm, DTM, slope, TPI, MRVBF, continentality) |> 
  drop_na()
pairs(df)

# Train random forest ####
# with default hyperparameters
library(ranger)
set.seed(123)
ranger_model <- ranger(depth_cm ~ ., data = df, num.trees = 1e3, keep.inbag = TRUE)

sadf <- read_csv(file="output/samplingMatrix.csv") |> 
  filter(road == 0, rwalkcost < 5000, canopy < 50)
predictions_with_se <- predict(ranger_model, data = sadf, type = 'se', se.method='jack')

predictionrast <- sa$allmyr
names(predictionrast) <- "pred"
values(predictionrast) <- NA
values(predictionrast)[sadf$cell] <- predictions_with_se$predictions
plot(predictionrast)

serast <- sa$allmyr
names(serast) <- "pred.se"
values(serast) <- NA
values(serast)[sadf$cell] <- predictions_with_se$se
plot(serast)

writeRaster(c(predictionrast, serast), 
            "output/adaptiveSamplingRF.tif", overwrite=TRUE)

# Use predicted depth/prediction SE to subset population ####

sa <- c(predictionrast, serast, sa)
minpred <- 150
minse <- 30
df <- as.data.frame(sa, xy = TRUE, cells=TRUE, na.rm=TRUE)

# Select 10 highest priority localities ####

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

localities <- df |>
  mutate(wgt = range01(pred)*range01(pred.se), .after = pred.se) |> 
  arrange(desc(wgt)) |> 
  slice_head(n = 10) |> 
  st_as_sf(coords = c('x','y'), crs="EPSG:25833") |> 
  select(pred, pred.se, wgt) |> 
  st_write('data/GIS/samplingDesign.gpkg', layer = 'adaptivesample1-localites', append=FALSE)

# Subset population further by rwalkcost ####

reach <- rast("output/rwalk-cost-adaptivesample1.tif")
reach <- resample(reach, sa, method='bilinear')
names(reach) <- "rwalkcost.adapt"

reachcoords <- as.data.frame(reach <= 1000, xy=TRUE) |> 
  filter(rwalkcost.adapt == TRUE) |> 
  select(x, y)
reachcells <- cellFromXY(sa, reachcoords)

# Sample in proportion to standardized pred*pred.se ####
# To focusing on the deep/uncertain cells at localities

samplingpop <- df |> 
  filter(cell %in% reachcells, road == 0, canopy == 0, pred > 100)
hist(samplingpop$pred)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

set.seed(123)
sample <- samplingpop |>
  mutate(wgt = range01(pred)*range01(pred.se), .after = pred.se) |> 
  slice_sample(n = 25, weight_by = wgt) |> 
  st_as_sf(coords = c('x','y'), crs="EPSG:25833") |> 
  select(pred, DTM, slope, TPI, MRVBF, continentality) |> 
  st_write('data/GIS/samplingDesign.gpkg', layer = 'adaptivesample1', append=FALSE)

