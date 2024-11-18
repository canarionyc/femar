
# setup -------------------------------------------------------------------

rm(list = ls())
source("~/.RProfile", echo=TRUE)
(dllinfo.list <- getLoadedDLLs())
str(dllinfo.list)
names(dllinfo.list)

"terra" %in% names(dllinfo.list)
dyn.unload()
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

#if(!isNamespaceLoaded("terra")) devtools::load_all("~/Spatial/terra-master/")
print(getLoadedDLLs())

ns <- getNamespace("terra")
ls(ns)

# https://rapidmapping.emergency.copernicus.eu/EMSR773/download

# https://disc.gsfc.nasa.gov/information/data-in-action?title=Precipitation%20impacts%20from%20Hurricane%20Beryl

my_ext <- ext(3.4e6,3.45e6,1.83e6,1.88e6)

# Valencia_VHR2021 --------------------------------------------------------
list.files(COPERNICUS_DATADIR, full.names = TRUE)

# ?system.file
# list.files(system.file("extdata", package = "femar"))
# system.file("extdata", "Valencia_VHR2021.tif", package = "femar")
?terra::rast
(Valencia_20241026.rast <- terra::rast("E:\\Datasets/EU/Copernicus/2024-10-26-00_00_2024-10-26-23_59_Sentinel-2_L2A_True_color_4326.tiff" , raw=FALSE  ))
terra::is.lonlat(Valencia_20241026.rast)

terra::nlyr(Valencia_20241026.rast)
?terra::project
Valencia_20241026_3035.rast <- terra::project(Valencia_20241026.rast, "EPSG:3035")
range(values(Valencia_20241026.rast), na.rm=TRUE)
?terra::plotRGB
Valencia_20241026_3035.rast %>% terra::plotRGB(r=1, g=2, b=3, scale=1)
polys(lau_es)

# Valencia_20241026_3035_png ----

Valencia_20241026_3035_png <- file.path(the$FEMA_WORKDIR, format(Sys.time(),"Valencia_20241026_3035_%Y%m%d_%H%M.png")); print(file.info(Valencia_20241026_3035_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = Valencia_20241026_3035_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(Valencia_20241026_3035_png)['size'])
browseURL(dirname(Valencia_20241026_3035_png))

# (lau_es_lonlat <- terra::project(lau_es, "+proj=longlat +datum=WGS84"))
# crs(lau_es) <- "EPSG:4326"
# polys(lau_es_lonlat)


crs(lau_es)

Valencia_20241026.RasterLayer <- raster::raster("E:\\Datasets/EU/Copernicus/2024-10-26-00_00_2024-10-26-23_59_Sentinel-2_L2A_True_color_UTM30.tiff")
Valencia_20241026.RasterLayer
raster::plot(Valencia_20241026.RasterLayer)
?plotRGB

terra::nlyr(Valencia_VHR2021)

?terra::plot
terra::plot(Valencia_VHR2021, y=1L)

# # before
# browseURL("https://disc.gsfc.nasa.gov/")
#
# (land0_tif.list <- list.files(file.path(DISC_DATADIR,"OPERA_L3_DSWX-HLS_V1_1.0-20241101_074233")
#                               , pattern="^OPERA_L3_DSWx-HLS_T30SYJ_20241008T103829Z_20241010T103453Z_S2B_30_v1.0"
#                               #                              , pattern = "LAND.tif"
#                               , full.names = TRUE))
# land0_tif <- terra::rast(land0_tif.list)
# names(land0_tif)
# land0_tif %>%plot()
#
# # after
# ?list.files
# (land_tif.list <- list.files(file.path(DISC_DATADIR,"OPERA_L3_DSWX-HLS_V1_1.0-20241101_060318")
#                              , pattern="^OPERA_L3_DSWx-HLS_T30SYJ_20241030T103658Z_20241101T030810Z_L8_30_v1.0"
#                              #                             , pattern = "LAND.tif"
#                              , full.names = TRUE))
#
#
# land_tif <- terra::rast(land_tif.list)
# land_tif
# names(land_tif)==names(land0_tif)
# land_tif %>% terra::plot()
#
#
# # plot compare
# plot_compare <- function(layer) {
#   opar <- par(mfrow=c(1,2))
#   land0_tif %>%plot(layer, main="Oct 8, 2024")
#   land_tif %>% terra::plot(layer, main="Oct 30, 2024")
#   par(opar)
#   title(layer)
# }
#

#
#
# library(purrr)
# ?terra::plot
# # terra::panel(land_tif)
#
# lapply(names(land_tif), plot_compare)
#
# ?map
# map(land_tif.list, ~terra::rast(.x) %>% terra::plot())


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
run_AOI <- function(i=1L, lau_es) {
  stopifnot(is.integer(i))
  product <- sprintf("EMSR773_AOI0%d_DEL_PRODUCT",i)
  version <- "v1"
  AOI.dir <- file.path(COPERNICUS_DATADIR,paste(product, version, sep="_"))
  # print(list.files(AOI.dir))


  ## areaOfInterestA ----------------------------
  ret_val <- list()
  # ?terra::vect
  # ?terra::query
  for(tag in  c("areaOfInterestA", "observedEventA")) {
    ret_val[[tag]] <- terra::vect(AOI.dir,layer= paste(product, tag,version, sep="_")) %>%
      terra::project( "EPSG:3035")
    terra::plot(ret_val[[tag]] , main=tag)
  }


  if(!missing(lau_es)){
    lau_es_crp<- terra::crop(lau_es, ret_val[["areaOfInterestA"]])
    # terra::plot(lau_es_crp, border="grey")
    # ?terra::text
    # terra::text(lau_es_crp, labels=lau_es_crp$LAU_NAME, cex=0.65)
  } else {
    lau_es_crp<- NULL
  }
  browser()
  CLMS_CLCplus <- terra::rast(file.path(COPERNICUS_DATADIR, "CLMS_CLCplus_RASTER_2021","CLMS_CLCplus_RASTER_2021_010m_eu_03035_V1_1/CLMS_CLCplus_RASTER_2021_010m_eu_03035_V1_1.tif")
                              ,win=ext(ret_val[["areaOfInterestA"]])
  )
  CLMS_CLCplus_stk <- terra::segregate(CLMS_CLCplus, classes=c(1L)); print(CLMS_CLCplus_stk)
  (CLMS_CLCplus_stk_msk <- terra::mask(CLMS_CLCplus_stk, ret_val[["areaOfInterestA"]]))
  # ?terra::coltab
  coltab(CLMS_CLCplus_stk_msk,1L) <- data.frame(value=c(0L,1L), color=c(NA, "grey"))
  print(CLMS_CLCplus_stk_msk)
  ?terra::plot

  # class(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1)

  terra::plot(ret_val[["areaOfInterestA"]], main="areaOfInterestA")

  terra::plot(CLMS_CLCplus_stk_msk, y = 1L, add = TRUE
              , alpha = 0.5
              ,legend=FALSE)
  if(!missing(lau_es)) {
    terra::polys(lau_es_crp, border="black")
    terra::text(lau_es_crp, labels=lau_es_crp$LAU_NAME, cex=0.65)
  }
  # writeLines(crs(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1))

  # ?terra::expanse
  # showMethods("expanse")
  # terra::expanse(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1)

  ## floodDepthA
  if(FALSE){
    # ?terra::vect
    #library(rgdal)
    tag <- "floodDepthA"; (ret_val[[tag]] <- terra::vect(AOI.dir,layer=paste(product, tag,version, sep="_")) %>%
                             terra::project( "EPSG:3035"))

    # EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value <- factor(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value, levels = c("Below 0.50","0.50 - 1.00", "1.00 - 2.00","2.00 - 4.00", "4.00 - 6.00"))
    # str(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value)
    # table(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value, useNA = "ifany")

    # ?terra::colorize
    # ?grDevices::colorRamp
    # ?grDevices::rgb
    # ?terra::plot
    # ?par
    # ?pch
    # ??graphics::legend
    # ext(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1)
    # ?SpatExtent
    # # emsr773_png
    #
    # # (col.df <- data.frame(value=c("Below 0.50","0.50 - 1.00", "1.00 - 2.00","2.00 - 4.00", "4.00 - 6.00"), color=colorRamp(c("white", "blue"))( (0:4)/4 ) %>% rgb( maxColorValue = 255)))
    # ?par
    # ?terra::plot
    # par(las=1)
    ret_val[["floodDepthA"]] %>% plot("value"
                                      , sort=c("Below 0.50","0.50 - 1.00", "1.00 - 2.00","2.00 - 4.00", "4.00 - 6.00")
                                      #                                                  , legend=FALSE
                                      , main=sprintf("Flood in Valencia Region, Spain EMSR773 - Situational reporting AOI%d", i)
                                      #                                                  , ext=c(-0.5,-0.1,39.0,39.6)
                                      , col=colorRamp(c("lightblue", "darkblue"))( (0:4)/4 ) %>% rgb( maxColorValue = 255)
                                      , border=NA
                                      #                                                  , ext=my_ext
                                      ,plg=list(title="Flood depth (m)")
                                      , mar=c(5.1, 3.1, 2.1, 7.1)

    )
    terra::polys(ret_val[["areaOfInterestA"]])
    if(!missing(lau_es)) if(!missing(lau_es)) {
      terra::polys(lau_es_crp, border="grey")
      terra::text(lau_es_crp, labels=lau_es_crp$LAU_NAME, cex=0.65)
    }
    #terra::polys(lau_es, border="grey")
    #lau_es_crp[muni_is_related,] %>% terra::text("LAU_NAME", cex=0.5, col="grey")

    #?graphics::mtext
    mtext("Data source: Copernicus Emergency Management Service", side = 1, line=2, outer = FALSE)
    terra::plot(CLMS_CLCplus_stk_msk, y = 1L, add = TRUE

                , alpha = 0.5
                ,legend=FALSE)
    if(FALSE){
      emsr773_png <- file.path(the$FEMA_WORKDIR, format(Sys.time(),"emsr773_%Y%m%d_%H%M.png")); print(file.info(emsr773_png))
      library(Cairo)
      dev.copy(device=Cairo::CairoPNG,filename = emsr773_png, width = 10.0, height = 6.0, dpi=300, units="in")
      dev.off()
      print(file.info(emsr773_png)['size'])
    }
    # browseURL(dirname(emsr773_png))
  }

  ### aggregate flood levels
  # EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1 %>% plot("value")


  # ?terra::aggregate

  # (EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg <- aggregate(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1[,"value"], by="value", fun=base::length))
  # EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg
  # EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg%>% as.data.frame() %>% print()
  #
  # EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg %>% plot("value")
  # EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg$agg_n %>% sum() ; dim(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1)[1] # number of polygons with some level of flood in the AOI!

  # ?terra::crosstab
  # ?terra::zonal
  # ?terra::merge

  # CLMS_CLCplus_stk_msk
  # CLMS_CLCplus_stk_msk_lau_es_AOI1
  # names(lau_es_AOI1)
  # lau_es_AOI1 %>% terra::plot("developed_cells_area_pct")

  ## EMSR773_AOI01_DEL_PRODUCT_imageFootprintA_v1
  # tag <- "imageFootprintA"
  #   (EMSR773_AOI01_DEL_PRODUCT_imageFootprintA_v1 <- terra::vect(AOI.dir,paste(product, tag,version, sep="_"))%>%
  #      terra::project( "EPSG:3035"))
  #   plot(EMSR773_AOI01_DEL_PRODUCT_imageFootprintA_v1, main=tag)

  ## observedEventA----------------------------


  #CLMS_CLCplus_stk_msk2 <- terra::mask(CLMS_CLCplus_stk_msk,ret_val[["observedEventA"]])
  # print(CLMS_CLCplus_stk_msk2)
  terra::plot(CLMS_CLCplus_stk_msk, y = 1L
              , alpha = 0.5
              ,legend=FALSE)
  plot(ret_val[["observedEventA"]],"event_type", main="Dana Floods, Oct 30, 2024", border=NA, add = TRUE)
  terra::polys(ret_val[["areaOfInterestA"]])
  ?terra::is.related
  lau_has_flash_flood <- terra::is.related(lau_es_crp,ret_val[["observedEventA"]], "intersects")
  if(!is.null(lau_es_crp)) if(!missing(lau_es)) {
    terra::polys(lau_es_crp, border="black")
    lau_es_crp%>% subset(lau_has_flash_flood) %>% {terra::text(., labels=.$LAU_NAME, cex=0.65)}
  }

  # observedEventA_png ----

  observedEventA_png <- file.path(the$FEMA_WORKDIR, format(Sys.time(),"observedEventA_%Y%m%d_%H%M.png")); print(file.info(observedEventA_png))
  library(Cairo)
  dev.copy(device=Cairo::CairoPNG,filename = observedEventA_png, width = 10.0, height = 6.0, dpi=300, units="in")
  dev.off()
  print(file.info(observedEventA_png)['size'])
  browseURL(dirname(observedEventA_png))
  # lau_es_crp %>% polys(border="grey")
  # # ?terra::text
  # lau_es_crp[muni_is_related,] %>% terra::text("LAU_NAME", cex=0.5, col="black")

  return(ret_val)
}

aoi1.lst <- run_AOI(1L)
aoi2.lst <- run_AOI(2L, lau_es)
aoi3.lst <- run_AOI(3L, lau_es)
aoi4.lst <- run_AOI(4L, lau_es)
# dana_out -----------------------------------------------------------------


dana_out_gpkg <- file.path(the$FEMA_WORKDIR, "dana_out.gpkg"); print(file.info(dana_out_gpkg))

if(file.exists(dana_out_gpkg)){
  dana_out <- terra::vect(dana_out_gpkg)
} else {
  dana_out <- terra::intersect(lau_es_crp, EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg)
  dana_out$value_area <- terra::expanse(dana_out)
  names(dana_out)

  ?terra::as.raster
  lau_es_crp
  dana_out$value_area_developed <- 100*terra::zonal(CLMS_CLCplus_stk_msk, dana_out, fun=base::sum, na.rm=TRUE)
  names(dana_out)

  dana_out$value_area_developed_pop_2021 <- round(dana_out$POP_DENS_2021*dana_out$value_area_developed*1e-6,0L)

  dana_out$value_area_developed_pop_2021_density <- round(dana_out$POP_DENS_2021*dana_out$value_area/dana_out$value_area_developed,0L)


  dana_out %>% terra::subset(dana_out$GISCO_ID=="ES_46250", select=c( "GISCO_ID"  #,"POP_2021",  "POP_DENS_2021"
                                                                      # , "Shape_Area"
                                                                      #  ,           "area", "developed_area"
                                                                      #  , "area_crp", "area_crp_pct_of_area", "developed_area_crp"
                                                                      ,"value"    , "value_area","value_area_developed",'value_area_developed_pop_2021'   ))
  dana_out %>% terra::subset(dana_out$GISCO_ID=="ES_46250" & dana_out$value=="Below 0.50", select=c( "GISCO_ID"  #,"POP_2021",  "POP_DENS_2021"
                                                                                                     # , "Shape_Area"
                                                                                                     #  ,           "area", "developed_area"
                                                                                                     #  , "area_crp", "area_crp_pct_of_area", "developed_area_crp"
                                                                                                     ,"value"    , "value_area","value_area_developed",'value_area_developed_pop_2021'   ))
  dana_out$value <- as.factor(dana_out$value) %>% fct_relevel("Below 0.50")
  levels(dana_out$value)
  ?terra::plot
  ?terra::SpatExtent

  ?terra::relate

  writeVector(dana_out, dana_out_gpkg, overwrite=TRUE)
  dana_out
}
dana_out

muni_is_related <- terra::is.related(lau_es_crp, EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1, "intersects")
lau_es_crp[muni_is_related,]

dana_out %>%
  #  terra::subset(dana_out$GISCO_ID=="ES_46250") %>%
  plot("value"
       #       , col=rev(heat.colors(5))
       , col=colorRamp(c("lightblue", "darkblue"))( (0:4)/4 ) %>% rgb( maxColorValue = 255)
       , border=NA,
       , sort=levels(dana_out$value)
       , ext=my_ext
       , main="Dana in Valencia, Oct 29, 2024"
       , plg=list(title="Flood level (m)"))
lau_es_crp %>% polys(border="grey")
# ?terra::text
lau_es_crp[muni_is_related,] %>% terra::text("LAU_NAME", cex=0.5, col="darkgrey")


# dana_out_png ----

dana_out_png <- file.path(the$FEMA_WORKDIR, format(Sys.time(),"dana_out_%Y%m%d_%H%M.png")); print(file.info(dana_out_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = dana_out_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(dana_out_png)['size'])
browseURL(dirname(dana_out_png))

# population density by flood level ---------------------------------------

names(dana_out)
dana_out %>% terra::subset(#dana_out$GISCO_ID=="ES_46250" &
  dana_out$value=="Below 0.50"
  , select=c( "GISCO_ID"  #,"POP_2021",  "POP_DENS_2021"
              # , "Shape_Area"
              #  ,           "area", "developed_area"
              #  , "area_crp", "area_crp_pct_of_area", "developed_area_crp"
              ,"value"    , "value_area","value_area_developed",'value_area_developed_pop_2021' ,'value_area_developed_pop_2021_density'  )) %>%
  plot("value_area_developed_pop_2021_density"
       , ext=my_ext)



# clean up --------------------------------------------------------------

