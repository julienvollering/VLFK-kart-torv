library(tidyverse)
library(sf)
library(terra)
library(clhs)

N <- 25 # 225 hours/9 hours/day
sa <- rast("output/samplingMatrix.tif")
plot(sa)

# Read data with coordinates and attributes
df <- read_csv(file="output/samplingMatrix.csv") |> 
  filter(road == 0)
summary(df)
df |> 
  select(DTM, slope, TPI, TRI, roughness, MRVBF, continentality) |> 
  cor()

# Discard TRI and roughness for high correlation with slope 
tictoc::tic()
clhsseeds <- 1:10 %>% 
  map(\(x) {
    set.seed(x)
    clhs(x = select(df, rwalkcost, DTM, slope, TPI, MRVBF, continentality), 
         size = N, 
         can.include = which(df$rwalkcost < 3000 & df$driveable == 1 & df$canopy < 50), 
         cost = "rwalkcost",
         iter = 1e6, simple = FALSE)
  })
tictoc::toc() #1939 sec elapsed
write_rds(clhsseeds, file="output/makeCLHS.rds")
# clhsseeds <- read_rds("output/makeCLHS.rds")

plot(map_dbl(clhsseeds, \(x) x$obj[1e6]))
clhsseedsdf <- map(clhsseeds, \(x) df[x$index_samples, ])
clhsseedsdf <- map(clhsseedsdf, \(x) arrange(x, desc(rwalkcost)))
bind_rows(clhsseedsdf, .id = "clhsseed") |> 
  st_as_sf(coords=c('x','y'), crs='EPSG:25833') |> 
  st_write('data/GIS/samplingDesign.gpkg', layer = 'primaryclhsseeds', append=FALSE)

# Examine 10 25-point sets in GIS to choose the set that is most practical/feasible
#clhsseeds <- read_rds("output/makeCLHS.rds")
chosenindex <- 5
# df |> 
#   select(DTM, slope, TPI, MRVBF, continentality) |> 
#   summary()
# df[clhsseeds[[chosenindex]]$index_samples, ] |> 
#   select(DTM, slope, TPI, MRVBF, continentality) |> 
#   summary()
# df |> 
#   select(DTM, slope, TPI, MRVBF, continentality) |> 
#   cor()
# df[clhsseeds[[chosenindex]]$index_samples, ] |> 
#   select(DTM, slope, TPI, MRVBF, continentality) |> 
#   cor()

# Secondary cLHS with rwalkcost from primary cLHS as starting points
# rwalkcost2 also has a new friction layer to allow movement along all roads

# Can.include
reach <- rast("output/rwalk-cost2.tif")
reach <- resample(reach, sa, method='bilinear')
names(reach) <- "rwalkcost2"
plot(reach <= 1000)

reachcoords <- as.data.frame(reach <= 1000, xy=TRUE) |> 
  filter(rwalkcost2 == TRUE) |> 
  select(x, y)
reachcells <- cellFromXY(sa, reachcoords)

set.seed(42)
tictoc::tic()
clhs2 <- clhs(x = select(df, DTM, slope, TPI, MRVBF, continentality), 
              size = 20*10, 
              #must.include = which(df$cell %in% clhsprimary$cell), 
              can.include = which(df$cell %in% reachcells & df$canopy < 50), 
              use.cpp=TRUE, iter = 1e7, simple = FALSE)
tictoc::toc() #1008 sec elapsed
plot(clhs2)

write_rds(clhs2, file="output/makeCLHS2.rds")

clhssecondary <- df[clhs2$index_samples, ] |> 
  select(cell, x, y, DTM, slope, TPI, MRVBF, continentality)

# Write secondary cLHS
clhssecondary |> 
  st_as_sf(coords = c('x','y'), crs="EPSG:25833") |> 
  select(DTM, slope, TPI, MRVBF, continentality) |> 
  st_write('data/GIS/samplingDesign.gpkg', layer = 'clhs', append=FALSE)

# Troubleshooting clhs::clhs 'can.include'

df <- data.frame(
  a = runif(1000), 
  b = rnorm(1000), 
  c = sample(LETTERS[1:5], size = 1000, replace = TRUE))

res <- clhs(df, size = 50, progress = FALSE, simple = TRUE)
str(res)

res <- clhs(df, size = 50, use.cpp = TRUE, iter = 5000, progress = FALSE, simple = FALSE)
str(res)
plot(res)

res <- clhs(df, size = 50, use.cpp = TRUE, iter = 5000, progress = FALSE, simple = FALSE,
            can.include = 1:500)
res$index_samples

res <- clhs(df, size = 50, use.cpp = TRUE, iter = 5000, progress = FALSE, simple = FALSE,
            must.include = 1:25, can.include = 1:500)
res$index_samples
