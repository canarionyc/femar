
# setup -------------------------------------------------------------------

library(sp)
library(raster)
library(spdep)

library(sf); library(tigris)

library(terra)
devtools::load_all("~/Spatial/FEMA/femar/")

options(digits = 3L)

# CA_state ----------------------------------------------------------------

(CA_state <- states(cb=TRUE, year=2020, keep_zipped_shapefile =TRUE) %>% subset(STATEFP=='06') %>%
   terra::vect()%>% project('EPSG:3857'))
CA_state %>% plot(lwd=2)

# cty06 -------------------------------------------------------------

cty06_sf <- counties(state='06', cb=TRUE, year=2020, keep_zipped_shapefile =TRUE) %>% st_transform('EPSG:3857')

st_crs(cty06_sf)

cty06 <- cty06_sf %>% terra::vect()%>% project('EPSG:3857')
cty06
geome(cty06)

## cty06 centroids ---------------------------------------------------------------
?st_centroid
cty06_ct_sf <- st_centroid(st_geometry(cty06_sf))
cty06_ct_sf

?terra::centroids
cty06_ct <- terra::centroids(cty06, inside=TRUE)
cty06_ct
names(cty06_ct)
?terra::geom
geom(cty06_ct)

plot(cty06)
points(cty06_ct, col=2)

?spdep::tri2nb
debugonce(spdep::tri2nb)
cty06_ct_sp <- as(cty06_ct,"Spatial")
coordinates(cty06_ct_sp)


# Neighbours --------------------------------------------------------------

?poly2nb
cty06_nb <- spdep::poly2nb(cty06_sf)
cty06_nb

par(mfrow=c(1,1))
plot(st_geometry(cty06_sf), reset=FALSE, main="CA counties", lwd=2)
plot(cty06_nb, coords=st_coordinates(st_centroid(cty06_sf)), col=2, add=TRUE)

## Delaunay triangularization and Dirichlet tessellation ----------------------------------------------

library(deldir)
?deldir
cty06_ct_sp.deldir <- deldir::deldir(coordinates(cty06_ct_sp))
plot(cty06_ct_sp.deldir)

?tri2nb
cty06_nb <- spdep::tri2nb(cty06_ct_sp)
cty06_nb

plot(cty06_nb, coords=coordinates(cty06_ct_sp))


# NRI_counties ------------------------------------------------------------

NRI_cty06 <- get_NRI_counties_vect(state='CA')
NRI_cty06

NRI_cty06_sf <- get_NRI_counties_sf(state='CA')

NRI_cty06_sp <- as(NRI_cty06, "Spatial")

NRI_cty06_ct <- terra::centroids(NRI_cty06,  inside=TRUE)


## Delaunay triangularization ----------------------------------------------

NRI_cty06_nb <- tri2nb(as(NRI_cty06_ct, "Spatial"))
NRI_cty06_nb

## polynomial neighbours ----
NRI_cty06_nb <- poly2nb(NRI_cty06_sp)
NRI_cty06_nb

plot(NRI_cty06_nb, coords=coordinates(as(NRI_cty06_ct,"Spatial")))

help("moran", package = "spdep")
NRI_cty06.W <- nb2listw(NRI_cty06_nb)
NRI_cty06.W

# moran(NRI_cty06$ALR_VALB, listw = NRI_cty06.W, n=length(NRI_cty06), Szero(NRI_cty06.W))
plot(NRI_cty06, 'ALR_VALB')
spdep::moran.test(NRI_cty06$ALR_VALB, listw = NRI_cty06.W)

summary(NRI_cty06$SOVI_SCORE)
?plot.sp
plot(NRI_cty06, 'SOVI_SCORE')

# cty0637: Los Angeles county ------------------------------------------------------

cty0637 <- cty06 %>% subset(COUNTYFP=='037', NSE=TRUE) %>% terra::disagg()
expanse(cty0637)
cty0637 <- cty0637[3,]

cty0637

?area

?terra::plot
plot(cty0637

#     , plg=list(ncol=4L, cex=2/3, horiz=FALSE,text.width=NA)
     , border=3
     )
text(cty0637, 'NAMELSAD',cex=2/3)

# CA_cbsas ----------------------------------------------------------------

?tigris::core_based_statistical_areas
CA_cbsas <- tigris::core_based_statistical_areas(cb = TRUE, year = 2020, filter_by=states(cb=TRUE, year=2020, keep_zipped_shapefile =TRUE) %>%
                                                   subset(STATEFP=='06')
                                                 , keep_zipped_shapefile =TRUE)%>%
  terra::vect()%>% project('EPSG:3857')
CA_cbsas
plot(CA_cbsas, add=TRUE)


# CA_places -------------------------------------------------------------
?places
CA_places <- places(state='06', cb=TRUE, year=2020, keep_zipped_shapefile =TRUE) %>%
  subset(LSAD=="25")%>% terra::vect()%>% project('EPSG:3857')

CA_places
CA_places[grepl("Los Angeles", CA_places$NAME) & CA_places$LSAD=="25",] %>% plot()

?terra::plot
plot(CA_places,add=TRUE, border=2)
text(CA_places, 'NAMELSAD', cex=1/3)


# tracts06 -------------------------------------------------------------
help(tracts, package="tigris")
tracts06037_sf <- tracts(state='06', county='037', cb=TRUE, year=2020, keep_zipped_shapefile =TRUE) %>%
  subset( ! TRACTCE %in% c('599000','599100') ) %>%
  sf::st_transform('EPSG:3857')# %>% terra::vect()%>% project('EPSG:3857')
tracts06037_sf

tracts06
?terra::subset
tracts06037 <- tracts06%>% subset(COUNTYFP=='037', NSE=TRUE) %>% subset( ! TRACTCE %in% c('599000','599100'), NSE=TRUE)

tracts06[grepl("Los Angeles", tracts06$NAME) & tracts06$LSAD=="25",] %>% plot()

?terra::plot



plot(cty0637, lwd=2)
plot(tracts06037,add=TRUE, border=2)

# text(tracts06, 'NAMELSAD', cex=1/3)

# NRI_tracts --------------------------------------------------------------
debugonce(get_NRI_tracts_vect)
devtools::load_all("~/Spatial/FEMA/femar/");  system.time(NRI_tracts06037 <- get_NRI_tracts_vect(statefips='06', countyfips='037') %>%
  subset(STATEFIPS=="06"&COUNTYFIPS=="037" & ! TRACT %in% c('599000','599100'), NSE=TRUE))
names(NRI_tracts06037)
# NRI_tracts <- NRI_add_categoricals(NRI_tracts)

NRI_tracts06037 %>% subset(STATEFIPS=="06"&COUNTYFIPS=="037" & ! TRACT %in% c('599000','599100'), NSE=TRUE)

NRI_tracts06037_sf <- sf::st_as_sf(NRI_tracts06037)
NRI_tracts06037_sf

NRI_tracts06037
print(NRI_tracts06037[, c('TRACTFIPS', 'EAL_VALB',"ALR_VALB")])

ALRB_cols <- grep("ALRB", names(NRI_tracts06037), value = TRUE)

arlb_dt <- as.data.table(NRI_tracts06037)[, ..ALRB_cols] %>% remove_na_cols()
arlb_dt

lapply(arlb_dt, range, na.rm=TRUE)

library(terra)
?map.pal
plot(NRI_tracts06037, "ALR_VALB", col=map.pal("reds"), main="ALR_VALB")

plot(NRI_tracts06037, "WFIR_ALRB", col=map.pal("reds"), main="WFIR_ALRB")
plot(NRI_tracts06037, "ERQK_ALRB", col=map.pal("reds"), main="ERQK_ALRB")


# NRI_tracts06037_ct.sp ---------------------------------------------------

NRI_tracts06037_ct.sp <- terra::centroids(NRI_tracts06037, inside=TRUE) %>% as("Spatial")

plot(NRI_tracts06037)
points(NRI_tracts06037_ct.sp, pch=20, col=2)

## K-nearest neighbours ----------------------------------------------------

?knearneigh
NRI_tracts06037.nb <- spdep::knn2nb(knearneigh(NRI_tracts06037_ct.sp, k=3L))
NRI_tracts06037.nb

?coordinates
plot(NRI_tracts06037)
plot(NRI_tracts06037.nb, coords=NRI_tracts06037_ct.sp, pch=20, col=2, add=TRUE); title(main="3 Knn")

NRI_tracts06037.W <- nb2listw(NRI_tracts06037.nb)

moran.test(NRI_tracts06037$ALR_VALB, NRI_tracts06037.W, randomisation = TRUE)
moran.test(NRI_tracts06037$WFIR_ALRB, NRI_tracts06037.W, randomisation = TRUE)
moran.test(fcoalesce(NRI_tracts06037$TRND_ALRB,0), NRI_tracts06037.W, randomisation = TRUE)

table(NRI_tracts06037$TRND_ALRB, useNA = "ifany")

moran.test(NRI_tracts06037$SOVI_SCORE, NRI_tracts06037.W, randomisation = TRUE)

## Construct neighbours list from polygon list -----------------------------

eps <- sqrt(.Machine$double.eps)
?poly2nb
NRI_tracts06037.nb <- spdep::poly2nb(as(NRI_tracts06037,"Spatial"), queen=TRUE, snap=eps)
NRI_tracts06037.nb

?coordinates
sp::coordinates(as(NRI_tracts06037_ct,"Spatial"))

plot(NRI_tracts06037.nb, coords= sp::coordinates(as(NRI_tracts06037_ct,"Spatial"))); title(main = "Boundary neighbours (queen)")

NRI_tracts06037.W <- spdep::nb2listw(NRI_tracts06037.nb)

?moran
spdep::moran.test(NRI_tracts06037$ALR_VALB, listw=NRI_tracts06037.W, randomisation = TRUE)

?moran.mc
moran.mc(NRI_tracts06037$ALR_VALB, listw=NRI_tracts06037.W,nsim=1e3L)

?moran.plot
moran.plot(NRI_tracts06037$ALR_VALB, listw=NRI_tracts06037.W)
