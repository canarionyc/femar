# file_hdf5.list ----------------------------------------------------------

# ?readLines
(file_hdf5.list <- readLines(file.path(DISC_DATADIR, "subset_GPM_3IMERGHHE_07_20241031_070927_.txt"), warn = FALSE) %>% basename()%>%
   grep(pattern="\\.HDF5$",value = TRUE) %>%
   fs::path_abs(start = DISC_DATADIR))
warnings()

stopifnot(all(file.exists(file_hdf5.list)))


# file_hdf5.list[!file.exists(file_hdf5.list)]


# import_gpm -------------------------------------------------------------------------

source("~/Spatial/terra-master/R/Agenerics.R", echo=TRUE)

(files <- file_hdf5.list[length(file_hdf5.list)])
import_gpm <- function(files, output_class= "SpatRaster", proj_epsg="4326", roi_mask=NULL) {

  print(files)
  stopifnot(all(file.exists(files)))
  nfiles <- length(files)

  if (output_class == "SpatRaster") {

    help("rast", package="terra")

    # browser()
    # options(verbose=FALSE)

    r <- terra::rast(files
                     # , drivers="NETCDF"
    ) %>% terra::trans() %>% terra::flip("vertical") %>% terra::flip("horizontal")

    names(r)
    plot(r, "precipitation" )
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
