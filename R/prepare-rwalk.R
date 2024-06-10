library(terra)
library(sf)
library(dplyr)
library(purrr)

N50 <- c("data/GIS/Luster_25833_N50/Basisdata_4644_Luster_25833_N50Kartdata_FGDB.gdb",
         "data/GIS/Sogndal_25833_N50/Basisdata_4640_Sogndal_25833_N50Kartdata_FGDB.gdb",
         "data/GIS/Sunnfj_25833_N50/Basisdata_4647_Sunnfjord_25833_N50Kartdata_FGDB.gdb") |> 
  map(\(x) st_read(x, layer = "N50_Arealdekke_omrade")) |> 
  bind_rows()
N50myr <- filter(N50, objtype == "Myr") |> 
  st_union()
N50land <- filter(N50, objtype != "Havflate") |> 
  st_union()
plot(N50land)

dtm <- rast("data/GIS/LusterSogndalSunnfjordtiles10m.tif")

veglenke <- st_read("data/GIS/samplingDesign.gpkg", layer="veglenke")
plot(st_geometry(veglenke))

veglenke |> 
  filter(tilgang) |> 
  st_geometry() |> 
  plot()

start <- rasterize(filter(veglenke, tilgang), dtm, field=1)
writeRaster(start, "output/rwalk-start.tif", overwrite=TRUE)

#The friction cost parameter represents a time penalty in seconds of additional walking time to cross 1 meter distance. 
friction <- dtm
values(friction) <- 30
unique(pull(N50, objtype))
friction <- rasterize(filter(N50, objtype == "Alpinbakke" | objtype == "ÅpentOmråde" | 
                               objtype == "Industriområde" | objtype == "Myr" | 
                               objtype == "Skog" | objtype == "SportIdrettPlass" | 
                               objtype == "Steinbrudd" | objtype == "Steintipp" | 
                               objtype == "Golfbane"), 
                      friction, field=5, update=TRUE)
friction <- rasterize(filter(veglenke, vegkategori == "P" | vegkategori == "S"), 
                      friction, field=0, update=TRUE)
plot(friction)
writeRaster(friction, "output/rwalk-friction.tif",overwrite=TRUE)

friction <- rasterize(filter(veglenke, vegkategori == "K" | vegkategori == "F" | 
                               vegkategori == "R"| vegkategori == "E"), 
                      friction, field=0, update=TRUE)
writeRaster(friction, "output/rwalk-friction2.tif",overwrite=TRUE)
