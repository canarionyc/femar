
# setup -------------------------------------------------------------------
help(package="terra")

browseURL(ORG_DATADIR)
list.files(file.path(ORG_DATADIR, "GADM"), full.names=TRUE)

land_tif


gadm2 <- terra::vect(file.path(ORG_DATADIR, "GADM","gadm41_ESP.gpkg"),"ADM_ADM_2")
gadm2 %>% plot()

?terra::project
EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1 <- terra::project(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1, crs(CLMS_CLCplus))


(gadm3 <- terra::vect("E:\\Datasets/ORG/GADM/gadm41_ESP.gpkg","ADM_ADM_3"))
gadm3_3035 <- gadm3 %>% terra::project("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")


# gadm3 %>% set.crs("EPSG:32630")
# cat(crs(gadm3))
gadm3 %>% plot()

gadm3 <- gadm3 %>% terra::project("+proj=utm +zone=30 +datum=WGS84")
gadm3
cat(crs(gadm3))
?terra::is.lonlat
terra::is.lonlat(gadm3)

#
# gadm4 <- terra::vect("E:\\Datasets/ORG/GADM/gadm41_ESP.gpkg","ADM_ADM_4")
# gadm4
#
# gadm4 %>% plot()


# plot layers -------------------------------------------------------------

land_tif%>% plot()
gadm3 %>% polys()

