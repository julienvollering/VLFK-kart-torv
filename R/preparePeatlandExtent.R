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
plot(N50myr, add=TRUE)

st_area(N50myr) |> 
  magrittr::divide_by(st_area(N50land)) #0.01962041 [1]

AR5 <- c("data/GIS/Luster_25832_AR5/Basisdata_4644_Luster_25832_FKB-AR5_FGDB.gdb",
         "data/GIS/Sogndal_25832_AR5/Basisdata_4640_Sogndal_25832_FKB-AR5_FGDB.gdb",
         "data/GIS/Sunnfj_25832_AR5/Basisdata_4647_Sunnfjord_25832_FKB-AR5_FGDB.gdb") |> 
  map(\(x) st_read(x, layer = "fkb_ar5_omrade")) |> 
  bind_rows()
AR5myr <- filter(AR5, arealtype == 60) |> 
  st_union() |> 
  st_transform(crs = st_crs(N50myr))

plot(N50land)
plot(AR5myr, add=TRUE)
st_area(AR5myr) |> 
  magrittr::divide_by(st_area(N50land)) #0.01837273 [1]

N50AR5myr <- st_union(N50myr, AR5myr) 
st_area(N50AR5myr) |> 
  magrittr::divide_by(st_area(N50land)) #0.03051263 [1]
N50AR5myr <- st_cast(N50AR5myr, to="POLYGON")

plot(N50land)
plot(N50AR5myr, add=TRUE)

bakkestuen <- rast("data/GIS/Bakkestuen2023_RemoteSensing/MyrMod2Rv.tif")
plot(bakkestuen, ext = st_bbox(N50land))

st_bbox(N50land)
studyrast <- rast(crs=crs(bakkestuen), resolution=10, 
                  xmin=-17040, ymin=6800990, xmax=144450, ymax=6881630)

myr <- rasterize(vect(N50land), studyrast, field=0)
myr <- rasterize(vect(N50AR5myr), myr, field=1, update=TRUE)
plot(myr)
mean(values(myr, na.rm=TRUE)) #0.0305072

bakkestuen_resampled <- resample(bakkestuen, studyrast, method='bilinear') |> 
  mask(vect(N50land))

# Hofsten et al. 2018 find that 5.2% of land area (including freshwater) in S&F is mire
# Hofsten, J., Rekdal, Y., & Strand, G.-H. (2018). 
# Arealregnskap for utmark. Arealstatistikk for Sogn og Fjordane. 
# NIBIO. https://nibio.brage.unit.no/nibio-xmlui/handle/11250/2566034

# Adjust threshold to obtain 5.2% mire
bakkestuenThreshold <- 640
allmyr <- mask(myr, bakkestuen_resampled >= bakkestuenThreshold, 
               maskvalues=TRUE, updatevalue=1)
plot(allmyr)
mean(values(allmyr, na.rm=TRUE))

writeRaster(allmyr, "output/allmire.tif")
