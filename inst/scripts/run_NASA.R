
# setup -------------------------------------------------------------------

library(units)
help(package="units")

library(terra)

library(rasterVis)
# help(package="rasterVis")

# BiocManager::install("rhdf5")

# install.packages('terra', repos='https://rspatial.r-universe.dev')

Sys.getenv()

DISC_DATADIR <- file.path(Sys.getenv("DATADIR"), "GOV", "NASA", "GSFC", "DISC"); stopifnot(dir.exists(DISC_DATADIR))
setwd(DISC_DATADIR)
getwd()




# wget --------------------------------------------------------------------
library(RCurl)
help(package="RCurl")
?download.file

getwd()

subset_txt <- file.path(DISC_DATADIR, "subset_GPM_3IMERGHHL_07_20241025_162247_.txt"); stopifnot(file.exists(subset_txt))

(url.list <- readLines(subset_txt, warn = FALSE))

Sys.which("wget")

?download.file
download.file(url.list[1], destfile = basename(url.list[1]), method = "wget")

# download files ----------------------------------------------------------

setwd(DISC_DATADIR); getwd()
url.list[-c(1:2)]
download.file
debugonce(download.file)

# my.download.file<-function (url, destfile, method, quiet = FALSE, mode = "w", cacheOK = TRUE,
#                             extra = getOption("download.file.extra"), headers = NULL,
#                             ...)
# {
#
#   destfile
#   if (missing(method))
#     method <- getOption("download.file.method", default = "auto")
#   method <- match.arg(method, c("auto", "internal", "wininet",
#                                 "libcurl", "wget", "curl", "lynx"))
#   if (missing(mode) && length(grep("\\.(gz|bz2|xz|tgz|zip|jar|rd[as]|RData)$",
#                                    URLdecode(url))))
#     mode <- "wb"
#   if (method == "auto") {
#     if (length(url) != 1L || typeof(url) != "character")
#       stop("'url' must be a length-one character vector")
#     method <- if (startsWith(url, "file:"))
#       "wininet"
#     else "libcurl"
#   }
#   nh <- names(headers)
#   if (length(nh) != length(headers) || any(nh == "") || anyNA(headers) ||
#       anyNA(nh))
#     stop("'headers' must have names and must not be NA")
#   switch(method, internal = , wininet = {
#     headers <- if (length(headers)) paste0(nh, ": ", headers,
#                                            "\r\n", collapse = "")
#     status <- .External(C_download, url, destfile, quiet,
#                         mode, cacheOK, headers, method == "wininet")
#   }, libcurl = {
#     headers <- if (length(headers)) paste0(nh, ": ", headers)
#     status <- .Internal(curlDownload(url, destfile, quiet,
#                                      mode, cacheOK, headers))
#   }, wget = {
#     if (length(url) != 1L || typeof(url) != "character") stop("'url' must be a length-one character vector")
#     if (length(destfile) != 1L || typeof(destfile) != "character") stop("'destfile' must be a length-one character vector")
#     if (quiet) extra <- c(extra, "--quiet")
#     if (!cacheOK) extra <- c(extra, "--cache=off")
#     browser()
#     cmd <- paste("wget", paste(extra, collapse = " "),
#                  shQuote(url), "-O", shQuote(path.expand(destfile)))
#     cat(cmd)
#     status <- system(cmd)
#     if (status) warning("'wget' call had nonzero exit status")
#   }, curl = {
#     if (length(url) != 1L || typeof(url) != "character") stop("'url' must be a length-one character vector")
#     if (length(destfile) != 1L || typeof(url) != "character") stop("'destfile' must be a length-one character vector")
#     if (quiet) extra <- c(extra, "-s -S")
#     if (!cacheOK) extra <- c(extra, paste("-H", shQuote("Pragma: no-cache")))
#     status <- system(paste("curl", paste(extra, collapse = " "),
#                            shQuote(url), " -o", shQuote(path.expand(destfile))))
#     if (status) stop("'curl' call had nonzero exit status")
#   }, lynx = stop("method 'lynx' is defunct", domain = NA),
#   stop("invalid method: ", method))
#   if (status > 0L)
#     warning("download had nonzero exit status")
#   invisible(status)
# }

lapply(url.list[-c(1:2)], function(x) {
  destfile <- basename(x)
  if(! (file.exists(destfile) && file.size(destfile)>0 )){
    download.file(x, destfile = destfile
                     , method = "wget"
                     , mode = "wb"
                     , extra = "--load-cookies C:/users/admin/.urs_cookies --save-cookies C:/users/admin/.urs_cookies --keep-session-cookies --user=canarionyc --password=fd@2KzRrQ7bXbN7 -nc -nv"
    )}
}
)


# hdf5 files --------------------------------------------------------------



# https://gpm.nasa.gov/data/directory

(hdf5_files.list <- list.files(DISC_DATADIR, pattern = "3B-HHR-L\\.MS\\.MRG\\.3IMERG\\.202407.*\\.HDF5$", full.names = TRUE))


# rhdf5 -------------------------------------------------------------------
library("rhdf5")
help(package="rhdf5")

(file_hdf5 <- hdf5_files.list[1]); stopifnot(H5Fis_hdf5(file_hdf5))


h5ls(file_hdf5)

h5dump(file_hdf5,load=FALSE)

?h5read
debugonce(h5read)
precipitation.ary <- h5read(file_hdf5, name = "Grid/precipitation", read.attributes = TRUE)
dim(precipitation.ary)
str(precipitation.ary)
attributes(precipitation.ary)
attr(precipitation.ary, '_FillValue')


# precipitation.ary2 <- aperm(precipitation.ary,c(2,1,3))
# dim(precipitation.ary2)
#range(precipitation.ary2)

dim(precipitation.ary3)
range(precipitation.ary3, na.rm = TRUE)
# image(tmp[,,1])

# geometry -------------------------------------------------------------------------


# latitude
(1:360)/2-90-0.25

# longitude
seq(1,72*5, 0.5)-180-0.25

# Grids_G1_rds ------------------------------------------------------------


# File:   3B-DAY.GPM.DPRGMI.CORRAGD.20240927-S000000-E235959.271.V07C.HDF5
# Object: /Grids/G1/G1_GridHeader

# BinMethod=ARITHMETIC_MEAN;
# Registration=CENTER;
# LatitudeResolution=5;
# LongitudeResolution=5;
# NorthBoundingCoordinate=70;
# SouthBoundingCoordinate=-70;
# EastBoundingCoordinate=180;
# WestBoundingCoordinate=-180;
# Origin=SOUTHWEST;

Grids_G1_rds <- file.path(Sys.getenv("R_WORK_DIR"), "Grids_G1.rds"); print(file.info(Grids_G1_rds)['size'])
if(file.exists(  Grids_G1_rds)) {
  Grids_G1 <- readRDS(Grids_G1_rds)
} else {
  Grids_G1 <- h5read(hdf5_files.list[6], name = "/Grids/G1")
  #attr(Grids_G1, "path") <- file_hdf5
  saveRDS(Grids_G1, Grids_G1_rds); print(file.info(Grids_G1_rds)['size'])
}; str(Grids_G1)
attributes(Grids_G1)


# Grids_G2_rds ------------------------------------------------------------

# File:   3B-DAY.GPM.DPRGMI.CORRAGD.20240927-S000000-E235959.271.V07C.HDF5
# Object: /Grids/G2/G2_GridHeader
#
# BinMethod=ARITHMETIC_MEAN;
# Registration=CENTER;
# LatitudeResolution=0.25;
# LongitudeResolution=0.25;
# NorthBoundingCoordinate=67;
# SouthBoundingCoordinate=-67;
# EastBoundingCoordinate=180;
# WestBoundingCoordinate=-180;
# Origin=SOUTHWEST;

Grids_G2_rds <- file.path(Sys.getenv("R_WORK_DIR"), "Grids_G2.rds"); print(file.info(Grids_G2_rds)['size'])
if(file.exists(  Grids_G2_rds)) {
  Grids_G2 <- readRDS(Grids_G2_rds)
} else {
  Grids_G2 <- h5read(hdf5_files.list[6], name = "/Grids/G2")
  attributes(Grids_G2)
  #attr(Grids_G2, "path") <- file_hdf5
  saveRDS(Grids_G2, Grids_G2_rds); print(file.info(Grids_G2_rds)['size'])
}; str(Grids_G2)

str(out)
fid <- H5Dopen(hdf5_files.list[6])

H5Dclose()

(0:1440)*0.25- 180 + 0.125 # longitude
(0.0:720.0) * 0.25 - 90 + 0.125 # latitude

# py -------------------------------------------------------------------------

lon = np.arange(0.0, 1440.0) * 0.25 - 180 + 0.125
lat = np.arange(0.0, 720.0) * 0.25 - 90 + 0.125

# file_nc4.list ---------------------------------------------------------------------

library(ncdf4)
help(package="ncdf4")

(file_nc4.list <- list.files(DISC_DATADIR, pattern = "3B-DAY-L\\.MS\\.MRG\\.3IMERG\\.\\d{8}.*\\.nc", full.names = TRUE))

?nc_open
nc <- nc_open(file_nc4.list[[1]], verbose = TRUE)
methods(class = "ncdf4")
print(nc)


# precipitation.RasterLayer -----------------------------------------------

library(raster)
help(package = "raster")
help("raster", package = "raster")
(precipitation.RasterLayer <- raster::raster(file_nc4.list[[1]]
                                             , varname='precipitation' #   "GPM_3IMERGDL_07_precipitation"
))
#precipitation.RasterLayer <- raster::raster(precipitation)
methods(class="RasterLayer")
dim(precipitation.RasterLayer)
isLonLat(precipitation.RasterLayer)
plot(precipitation.RasterLayer) # it is transposed? rotated?
?raster::flip
precipitation.RasterLayer %>% plot()
raster::flip(precipitation.RasterLayer,direction='x') %>% plot(main="flip(direction='x')")
raster::flip(precipitation.RasterLayer,direction='y') %>% plot(main="flip(direction='y')")

# precipitation_grd -------------------------------------------------------

dir.exists(file.path(Sys.getenv("R_WORK_DIR"), "Spatial"))
precipitation_grd <- file.path(Sys.getenv("R_WORK_DIR"), "Spatial", "precipitation.grd"); print(file.info(precipitation_grd))
raster::writeRaster(precipitation.RasterLayer, filename = precipitation_grd)

?slotNames
slotNames("raster")

as(precipitation.RasterLayer, "SpatRaster") %>% terra::plot()


rasterTheme()

?hsv
debugonce(hsv)
?magma
?magmaTheme
debugonce(rasterTheme)
contourplot(precipitation.RasterLayer)

?`levelplot,Raster,missing-method`
levelplot(precipitation.RasterLayer)

# precipitation matrix -----------------------------------------------------------

#varid <- "MWprecipitation"
varid <- "GPM_3IMERGDL_07_precipitation"
?ncvar_get
precipitation <- ncvar_get(nc, varid = varid, verbose=TRUE)
class(precipitation)
dim(precipitation)
str(precipitation)

precipitation.aperm <- aperm(precipitation, c(2,1))
dim(precipitation.aperm)
all.equal(precipitation.aperm, t(precipitation))

lat <- ncvar_get(nc, varid = "lat", verbose=FALSE)
length(lat); range(lat)
# (lat_bnds <- ncvar_get(nc, varid = "lat_bnds", verbose=FALSE))

??raster
lon <- ncvar_get(nc, varid = "lon", verbose=FALSE)
# str(lon)
length(lon); range(lon)

str(precipitation)
attributes(precipitation)

precipitation_max <- max(precipitation, na.rm = TRUE)
units(precipitation_max) <- "mm"
precipitation_max
units(precipitation_max) <- "inches"

?contour
contour(x=lon, y=lat,precipitation)


# precipitation_rast GOOD not needed -------------------------------------------------

# grDevices::contourLines(x=lon, y=lat,precipitation)
?rasterize
?terra::rast
str(precipitation)
precipitation.mat <- precipitation.ary[,,1]
dim(precipitation.mat)
precipitation.mat[precipitation.mat<0] <- NA_real_
anyNA(precipitation.mat)
range(precipitation.mat, na.rm = TRUE)


precipitation.mat <- precipitation.mat[precipitation.mat>0]

(precipitation_rast <- terra::rast(precipitation.mat
                                   #                                   , crs= "+proj=longlat"
                                   #                                  , extent= c(range(lon), range(lat)))
)
) %>% plot()


(precipitation_rast <- terra::rast(t(precipitation.mat)[ncol(precipitation.mat):1,]
                                   #                                   , crs= "+proj=longlat"
                                   #                                  , extent= c(range(lon), range(lat)))
)
) %>% plot()

# (precipitation_rast_rot <- rotate(precipitation_rast)) %>% plot()
methods(class='SpatRaster')
?ext
range(lon)
#ext(precipitation_rast) <-  c(range(lon), range(lat))
precipitation_rast
terra::plot(precipitation_rast)
?terra::flip
?terra::rotate

?crs

# crs(precipitation_rast) <- "+proj=longlat"
cat(crs(precipitation_rast))

?terra::`plot,SpatRaster,numeric-method`
?terrain.colors

palette.colors()
?colorRampPalette

?terra::plot
?st_crs
st_crs(precipitation_rast)

cbsa_crs <- st_crs(cbsa_vec)
str(cbsa_crs)

?set.crs
?crs
crs(cbsa_vec)
precipitation_rast %>% set.crs(st_crs(cbsa_vec)$input)

ext(cbsa_vec)
# terra::plot(cbsa_vec
#             ,ext=c( -100, -60, 20, 40 )
#             )


# fig-precipitation.RasterLayer -------------------------------------------

?par
(usr <- par(usr=c(-100,  -70,   20,40)))
par("usr")
clip(-100,  -70,   20,40)
precipitation.RasterLayer %>% ext %>% {do.call("clip",list(xmin(.), xmax(.), ymin(.), ymax(.)))}

par(xaxs="i")
?raster::plot
?axis
undebug(axis)
?par
debugonce(raster::plot)
library(gstat)
debugonce(gstat)
?raster::disaggregate
precipitation.RasterLayer %>% raster::disaggregate(fact=c(2,2)) %>% plot()


debugonce(raster::disaggregate)
debugonce(raster::focal)
raster::plot(precipitation.RasterLayer
             #             ,col=rev(terrain.colors(255))
             , col= colorRampPalette(c("white","yellow", "red"))(2^8-1)
             , interpolate = TRUE
             , main="2024-09-27"
             #  , asp=1, xpd=NA
             # , ext=c(-100,-70,20,40)
)

# ?rasterVis::`gplot,Raster-method`
# library(ggplot2)
# rasterVis::gplot(precipitation.RasterLayer)+ geom_tile(aes(fill = value))


# precipitation.trellis ---------------------------------------------------


dev.new()
?rasterVis::levelplot
?xscale.raster
library(latticeExtra)
?llines.SpatVector

my.theme <- rasterTheme()
my.theme <- viridisTheme()
my.theme <- infernoTheme()
my.theme <- plasmaTheme()

(my.theme$regions$col <- colorRampPalette(c("white","yellow", "red"))(100L))


(precipitation.trellis <- rasterVis::levelplot(precipitation.RasterLayer%>% raster::disaggregate(fact=c(2,2))
                                               , par.settings=my.theme
                                               , main='Precipitation 2024-09-27'
                                               , xlab=NA, ylab=NA
                                               ,margin=FALSE)) + layer(lpolygon(states_vec))




methods(class="SpatExtent")
?as.matrix
clip.mat <- ext(precipitation.RasterLayer)%>% as.matrix()
c(clip.mat)
?clip

# fig-precipitation.SpatRaster --------------------------------------------
# rasterVis::xyplot(as(precipitation.RasterLayer, "SpatRaster"))
?terra::plot
debugonce(clip)
terra::plot(as(precipitation.RasterLayer, "SpatRaster")
            , col= colorRampPalette(c("white","yellow", "red"))(2^8-1)
            #           ,ext=c( -100, -70, 20, 40 )
            , main="2024-09-27"
            , reset = FALSE
            , clip = TRUE
)
lines(states_vec, lwd=1)
#lines(cbsa_vec)
#precipitation_df <- as.data.frame(pmypal()#precipitation_df <- as.data.frame(precipitation)

# precipitation_png ----

precipitation_png <- file.path(Sys.getenv("R_WORK_DIR"), format(Sys.time(),"precipitation_%Y%m%d_%H%M.png")); print(file.info(precipitation_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = precipitation_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(precipitation_png)['size'])
browseURL(dirname(precipitation_png))


# -------------------------------------------------------------------------


?vect
?SpatVector

# as.raster(precipitation)
?SpatRaster
?contour

range(lat <- (1:1800)*0.1-90)
range(lon <- (1:3600)*0.1-180)



1600/28
3600/72 # 50

536/28
1600/536
3600/1440 # 2.5

# precipitation GOOD ------------------------------------------------------

dim(precipitation)
length(lon)
length(lat)
?`contour,SpatRaster-method`
contour(x=lon, y=lat, z =  precipitation)
dim(precipitation); c(length(lon), length(lat))
nc <- file.path(DISC_DATADIR, "3B-DAY-E.MS.MRG.3IMERG.20240927-S000000-E235959.V07B.nc")



# tif ---------------------------------------------------------------------
(tif_files.list <- list.files("E:/Geo", pattern = "\\.tiff?$", full.names = TRUE))

#file.choose()
debugonce(rast)
MWprecipitation_rast <- terra::rast(tif_files.list[3])
str(MWprecipitation_rast)
methods(class='SpatRaster')
ext(MWprecipitation_rast)
terra::plot(MWprecipitation_rast)

MWprecipitation_rast %>% as.data.frame() -> MWprecipitation
str(MWprecipitation)

# cleanup -----------------------------------------------------------------

h5closeAll()

