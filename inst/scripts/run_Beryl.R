
# setup -------------------------------------------------------------------

rm(list = ls())
library(fs)
library(ncdf4)
help(package="ncdf4")

library(raster)
help(package="raster")
rasterOptions()

# https://disc.gsfc.nasa.gov/information/data-in-action?title=Precipitation%20impacts%20from%20Hurricane%20Beryl

DISC_DATADIR <- "E:/Datasets/GOV/NASA/GSFC/DISC"

?readLines
(file_hdf5.list <- readLines(file.path(DISC_DATADIR, "subset_GPM_3IMERGHHL_07_20241024_154346_.txt"), warn = FALSE) %>% basename()%>%
    grep(pattern="\\.HDF5$",value = TRUE) %>%
    fs::path_abs(start = DISC_DATADIR))
warnings()

stopifnot(all(file.exists(file_hdf5.list)))

# file_hdf5.list[!file.exists(file_hdf5.list)]


# ncdf4 -------------------------------------------------------------------


?nc_open
(nc <- ncdf4::nc_open(file_hdf5.list[[1]]))
methods(class="ncdf4")

print(paste("The file has",nc$nvars,"variables"))

str(nc$var, max.level = 1L)

library(purrr)
map_chr(nc$var, "name")

precipitation.mat <- ncvar_get(nc, "Grid/precipitation"  )
class(precipitation.mat)
dim(precipitation.mat)
range(precipitation.mat, na.rm = TRUE)

lon_bnds.mat <- ncvar_get(nc, "Grid/lon_bnds"  )
dim(lon_bnds.mat)


?terra::rast


# terra::rast -------------------------------------------------------------


(precipitation.SpatRaster <- terra::rast(file_hdf5.list[[1]], subds="//Grid/precipitation" ))
t(precipitation.SpatRaster)
t(precipitation.SpatRaster) %>% plot(main="precipitation.SpatRaster")


# raster::raster ----------------------------------------------------------

?raster::raster
(precipitation.RasterLayer <- raster::raster(file_hdf5.list[[1]],sub="//Grid/precipitation", RAT=TRUE
                                             # ,crs= "+proj=longlat +datum=NAD83 +no_defs"
                                             ))
slotNames(precipitation.RasterLayer)
if(precipitation.RasterLayer@rotated) print(precipitation.RasterLayer@rotation)


?`RasterLayer-class`
showClass("RasterLayer")
methods(class = "RasterLayer")
names(precipitation.RasterLayer)

getValues(precipitation.RasterLayer)
as(precipitation.RasterLayer, "SpatRaster") %>% terra::plot()

?raster::plot
raster::plot(precipitation.RasterLayer
             # , "precipitation"
             , useRaster=TRUE)

?raster::raster
debugonce(stack)
precip.stack <- raster::stack(file_hdf5.list[1:2], varname="Grid/precipitation" )
warnings()

precip.stack
