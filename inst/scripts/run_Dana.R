
# setup -------------------------------------------------------------------

rm(list = ls())
source("~/.RProfile", echo=TRUE)

# library(fs)

# ?configr::read.config
# cfg <- configr::read.config(eval.expr = TRUE)
# str(cfg)

# library(ncdf4)
# help(package="ncdf4")

# setGeneric("not.na", function(x, ...) {
#   x[!is.na(x)]
#   # standardGeneric("not.na")
# } )
# isGeneric("not.na")
#
# setGeneric("is.bool", function(x, ...) {
#   x[is.logical(x)]
#   # standardGeneric("not.na")
# } )
# setGeneric("is.int", function(x, ...) {
#   x[is.integer(x)]
#   # standardGeneric("not.na")
# } )

if(!isNamespaceLoaded("terra")) devtools::load_all("~/Spatial/terra-master/")
print(getLoadedDLLs())

ns <- getNamespace("terra")
ls(ns)
# https://disc.gsfc.nasa.gov/information/data-in-action?title=Precipitation%20impacts%20from%20Hurricane%20Beryl


# before ------------------------------------------------------------------
browseURL("https://disc.gsfc.nasa.gov/")

(land0_tif.list <- list.files(file.path(DISC_DATADIR,"OPERA_L3_DSWX-HLS_V1_1.0-20241101_074233")
                              , pattern="^OPERA_L3_DSWx-HLS_T30SYJ_20241008T103829Z_20241010T103453Z_S2B_30_v1.0"
                              #                              , pattern = "LAND.tif"
                              , full.names = TRUE))
land0_tif <- terra::rast(land0_tif.list)
names(land0_tif)
land0_tif %>%plot(main="Oct 8, 2024")

# after -------------------------------------------------------------------------
?list.files
(land_tif.list <- list.files(file.path(DISC_DATADIR,"OPERA_L3_DSWX-HLS_V1_1.0-20241101_060318")
                             , pattern="^OPERA_L3_DSWx-HLS_T30SYJ_20241030T103658Z_20241101T030810Z_L8_30_v1.0"
                             #                             , pattern = "LAND.tif"
                             , full.names = TRUE))


land_tif <- terra::rast(land_tif.list)
land_tif
names(land_tif)==names(land0_tif)
land_tif %>% terra::plot("Land cover classification (LAND)", main="Oct 30, 2024")


# plot compare ------------------------------------------------------------
plot_compare <- function(layer) {
  opar <- par(mfrow=c(1,2))
  land0_tif %>%plot(layer, main="Oct 8, 2024")
  land_tif %>% terra::plot(layer, main="Oct 30, 2024")
  par(opar)
  title(layer)
}

# -------------------------------------------------------------------------


library(purrr)
?terra::plot
# terra::panel(land_tif)

lapply(names(land_tif), plot_compare)

?map
map(land_tif.list, ~terra::rast(.x) %>% terra::plot())

# file_hdf5.list ----------------------------------------------------------

# ?readLines
(file_hdf5.list <- readLines(file.path(DISC_DATADIR, "subset_GPM_3IMERGHHE_07_20241031_070927_.txt"), warn = FALSE) %>% basename()%>%
   grep(pattern="\\.HDF5$",value = TRUE) %>%
   fs::path_abs(start = DISC_DATADIR))
warnings()

stopifnot(all(file.exists(file_hdf5.list)))


# file_hdf5.list[!file.exists(file_hdf5.list)]


# -------------------------------------------------------------------------

source("~/Spatial/terra-master/R/Agenerics.R", echo=TRUE)

COPERNICUS_DATADIR <-
  files <- file_hdf5.list
import_gpm <- function(files, output_class= "SpatRaster", proj_epsg="4326", roi_mask=NULL) {

  print(files)
  stopifnot(all(file.exists(files)))
  nfiles <- length(files)

  if (output_class == "SpatRaster") {

    help("rast", package="terra")

    # browser()
    # options(verbose=FALSE)
    devtools::load_all("~/Spatial/terra-master/")
    debugonce(terra::rast)
    r <- terra::rast(files
                     # , drivers="NETCDF"
    ) %>% terra::trans() %>% terra::flip("vertical") %>% terra::flip("horizontal")


    rlist <- list()
    i <- 1L
    for (i in seq_along(files)) {
      message(paste("Processing", files[i], "\n"))

      # debugonce(terra::rast)
      library(ncdf4)
      nc <- ncdf4::nc_open(files[i])

      purrr::map_chr(nc$var, getElement, "name")
      match("Grid/precipitation", purrr::map_chr(nc$var, getElement, "name"))
      str(v4 <- nc$var[[4]])
      # ?ncvar_get

      precipitation <- ncdf4::ncvar_get(nc, v4, verbose = FALSE)
      dim(precipitation)

      r <- terra::rast(files[i], drivers="HDF5") %>% terra::trans() %>% terra::flip("vertical") %>% terra::flip("horizontal")
      # r <- terra::rast(precipitation)
      print(r)
      # r <- terra::trans(r)# %>% terra::flip("vertical") %>% terra::flip("horizontal")
      terra::ext(r) <- c(-180,180,-90,90)
      terra::crs(r) <- "epsg:4326"; print(r)
      # terra::plot(r[["precipitation"]]);
      # terra::plot(r
      #           #  , "precipitation"
      #             ,  col= rev(heat.colors(255)))
      rlist[[i]] <- r
    }

    rasts <- terra::rast(rlist)
    # rasts <- terra::rast(files[max(nfiles-1L,1L):nfiles])
    print(rasts)
    help("crs", package="terra")
    terra::crs(rasts) <- "epsg:4326"; print(rasts)
    print(terra::crs(rasts, describe=TRUE))

    help("ext", package="terra")
    print(ext(rasts))
    # browser()
    #  help("t", package="terra")
    # rasts <- rasts %>%
    #   terra::t()
    #
    # rasts <- rasts %>%
    #   terra::flip("vertical")
    #
    # rasts <- rasts %>%
    #   terra::flip("horizontal")
    print(rasts)
    if (!is.null(proj_epsg)) {
      rasts <- terra::project(rasts, paste0("epsg:", proj_epsg))
    } else {
      rasts <- terra::project(rasts, "epsg:4326")

    }
    print(rasts)
    if (!is.null(roi_mask)) {
      roi_mask <- terra::vect(roi_mask)
      if (!is.null(proj_epsg)) {
        roi_mask <- terra::project(roi_mask, paste0("epsg:", proj_epsg))
      } else {
        roi_mask <- terra::project(roi_mask, "epsg:4326")
      }

      rasts <- terra::mask(rasts, roi_mask)
    }
  } else if (output_class == "stars") {
    stop("stars output is not implemented for this collection")
    # rasts <- stars::read_stars(files) %>%
    #   st_transform(4326) %>%
    #   stars::t() %>%
    #   stars::flip("y")

    # if(!is.null(proj_epsg)){
    #   rasts <- stars::st_transform(rasts,paste0("epsg:",proj_epsg))
    # }
  }

  return(rasts)
}



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


# # raster::raster
#
# ?raster::raster
# (precipitation.RasterLayer <- raster::raster(file_hdf5.list[[1]],sub="//Grid/precipitation", RAT=TRUE
#                                              # ,crs= "+proj=longlat +datum=NAD83 +no_defs"
# ))
# slotNames(precipitation.RasterLayer)
#
#
# ?`RasterLayer-class`
# showClass("RasterLayer")
# methods(class = "RasterLayer")
# names(precipitation.RasterLayer)
#
# getValues(precipitation.RasterLayer)
# as(precipitation.RasterLayer, "SpatRaster") %>% terra::plot()
#
# ?raster::plot
# raster::plot(precipitation.RasterLayer
#              # , "precipitation"
#              , useRaster=TRUE)
#
# ?raster::raster
# debugonce(stack)
# precip.stack <- raster::stack(file_hdf5.list[1:2], varname="Grid/precipitation" )
# warnings()
#
# precip.stack
#

# Copernicus --------------------------------------------------------------
# COPERNICUS_DATADIR <- "E:/Datasets/EU/Copernicus/"
setwd(COPERNICUS_DATADIR)

(zipfiles.list <- list.files(COPERNICUS_DATADIR,pattern = "\\.zip$", full.names = TRUE))

basename(zipfiles.list) %>% fs::path_ext_remove()

library(purrr)
?as_mapper

my_unzip <- as_mapper(function(.x) { exdir <- fs::path_ext_remove(.x); unzip(.x, exdir = exdir); return(exdir)} )

map(zipfiles.list, my_unzip)

?list.dirs
(dirs.list <- list.dirs(COPERNICUS_DATADIR, recursive = FALSE))

?terra::vect
floodDepthA.list <-  map(dirs.list, ~terra::vect(.x,2))

sprintf("E:/Datasets/EU/Copernicus/EMSR773_AOI%02i_DEL_PRODUCT_v1",1:4)
sprintf("E:/Datasets/EU/Copernicus/EMSR773_AOI%02i_DEL_PRODUCT_v1",1:4)

floodDepthA.list <- map(1:4L,~terra::vect(sprintf("EMSR773_AOI%02i_DEL_PRODUCT_v1",.x),sprintf("EMSR773_AOI%02i_DEL_PRODUCT_floodDepthA_v1", .x)))
names(floodDepthA.list) <- sprintf("EMSR773_AOI%02i",1:4)
str(floodDepthA.list, max.level=1L)
?lmap
lmap(floodDepthA.list, function(x) {
  browser()
  str(x[[1]])
  terra::plot(x[[1]],"value", main=names(x)[1])
  return(NULL)
})

# EMSR773_AOI01_DEL_PRODUCT_v1 --------------------------------------------

## EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1 ----------------------------
getwd()
?terra::vect
?terra::query
EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1 <- terra::vect(file.path(COPERNICUS_DATADIR,"EMSR773_AOI01_DEL_PRODUCT_v1"),"EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1") %>%
  terra::project( "EPSG:3035")

class(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1)

plot(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1)

writeLines(crs(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1))

?terra::expanse
showMethods("expanse")
terra::expanse(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1)

## EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1 --------------------------------

?terra::vect
#library(rgdal)
(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1 <- terra::vect("EMSR773_AOI01_DEL_PRODUCT_v1"
                                                         ,layer="EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1") %>%
    terra::project( "EPSG:3035"))

# EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value <- factor(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value, levels = c("Below 0.50","0.50 - 1.00", "1.00 - 2.00","2.00 - 4.00", "4.00 - 6.00"))
str(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value)
table(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value, useNA = "ifany")

# ?terra::colorize
?grDevices::colorRamp
?grDevices::rgb
?terra::plot
?par
?pch
??graphics::legend
ext(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1)
?SpatExtent

# (col.df <- data.frame(value=c("Below 0.50","0.50 - 1.00", "1.00 - 2.00","2.00 - 4.00", "4.00 - 6.00"), color=colorRamp(c("white", "blue"))( (0:4)/4 ) %>% rgb( maxColorValue = 255)))
EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1 %>% plot("value"
                                                  , sort=c("Below 0.50","0.50 - 1.00", "1.00 - 2.00","2.00 - 4.00", "4.00 - 6.00")
                                                  #                                                  , legend=FALSE
                                                  , main="EMSR773_AOI01"
                                                  #                                                  , ext=c(-0.5,-0.1,39.0,39.6)
                                                  , col=colorRamp(c("blue", "red"))( (0:4)/4 ) %>% rgb( maxColorValue = 255)
                                                  , border=NA

)
# terra::add_legend("topright"
#                   , legend= c("Below 0.50","0.50 - 1.00", "1.00 - 2.00","2.00 - 4.00", "4.00 - 6.00")
#                   , col = rainbow(5)
#                   , pch=15
#                   , bty="n"
#                   , cex=0.65
#                   )
gadm3 %>% polys(border="grey")
terra::text(gadm3, gadm3$NAME_3, cex=0.5, col="grey")

## EMSR773_AOI01_DEL_PRODUCT_imageFootprintA_v1 ----------------------------

(EMSR773_AOI01_DEL_PRODUCT_imageFootprintA_v1 <- terra::vect("EMSR773_AOI01_DEL_PRODUCT_v1","EMSR773_AOI01_DEL_PRODUCT_imageFootprintA_v1")%>%
   terra::project( "EPSG:3035"))
plot(EMSR773_AOI01_DEL_PRODUCT_imageFootprintA_v1)

## EMSR773_AOI01_DEL_PRODUCT_observedEventA_v1 ----------------------------


(EMSR773_AOI01_DEL_PRODUCT_observedEventA_v1 <- terra::vect("EMSR773_AOI01_DEL_PRODUCT_v1","EMSR773_AOI01_DEL_PRODUCT_observedEventA_v1")%>%
   terra::project( "EPSG:3035"))
plot(EMSR773_AOI01_DEL_PRODUCT_observedEventA_v1,"event_type")

## EMSR773_AOI01_DEL_PRODUCT_source_v1  ----------------------------


(EMSR773_AOI01_DEL_PRODUCT_source_v1  <- terra::vect("EMSR773_AOI01_DEL_PRODUCT_v1","EMSR773_AOI01_DEL_PRODUCT_source_v1")%>%
   terra::project( "EPSG:3035"))
plot(EMSR773_AOI01_DEL_PRODUCT_source_v1 ,"source_nam")

