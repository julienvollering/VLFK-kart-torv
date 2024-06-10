library(terra)
library(sf)
library(dplyr)

# Population
allmyr <- rast("output/allmire.tif")

# Can.include
reach <- rast("output/rwalk-cost.tif")
reach <- resample(reach, allmyr, method='bilinear')
names(reach) <- "rwalkcost"

servicearea <- st_read("data/GIS/samplingDesign.gpkg" , layer="servicearea")
serviceareabuffer <- st_cast(servicearea, to="LINESTRING") |> 
  st_buffer(dist = 4000) |> 
  st_union()
plot(st_geometry(serviceareabuffer))
st_write(serviceareabuffer, "data/GIS/samplingDesign.gpkg" , 
         layer="serviceareabuffer", append=FALSE)
# serviceareabuffer <- st_read("data/GIS/samplingDesign.gpkg", layer="serviceareabuffer")
drive <- rasterize(vect(serviceareabuffer), allmyr, field=1)
names(drive) <- "driveable"

# Stratified on
dtm <- rast("data/GIS/LusterSogndalSunnfjordtiles10m.tif")
names(dtm) <- "DTM"
plot(dtm)
mrvbf <- rast("data/GIS/LusterSogndalSunnfjordMRVBF.tif")
names(mrvbf) <- "MRVBF"
plot(mrvbf)
terrainind <- terrain(dtm, v = c('slope','TPI','TRI','roughness'))
terr <- c(dtm, terrainind, mrvbf)
terr <- resample(terr, allmyr, method='bilinear')

section <- rast("data/GIS/Bakkestuen2008_JBiogeogr/seksjoner.tif")
names(section) <- "continentality"
section <- project(section, allmyr, method='bilinear')
plot(section)

covariates <- c(terr, section)

# Matrix
stack <- c(reach, drive, covariates)
sa <- mask(stack, allmyr, maskvalue=1, inverse=TRUE)
sa <- trim(sa)
plot(sa)

df <- as.data.frame(sa, xy = TRUE, cells=TRUE, na.rm=TRUE)

writeRaster(sa, "output/samplingMatrix.tif", overwrite=TRUE)
readr::write_csv(df, "output/samplingMatrix.csv", append=FALSE)
