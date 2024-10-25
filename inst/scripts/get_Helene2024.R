
# setup -------------------------------------------------------------------


# library(rgdal)

# library(terra)
# help(package="rgdal")

library(sf)
library(terra)
devtools::load_all("~/Spatial/FEMA/femar/");
# Helene_kmz <- file.path(Sys.getenv("DATADIR"), "ArcGIS", "al092024_best_track.kmz")
# unzip(Helene_kmz, exdir = file.path(Sys.getenv("DATADIR"), "ArcGIS"))


# Helene_vec --------------------------------------------------------------

Helene_kml <- file.path(Sys.getenv("DATADIR"), "ArcGIS", "al092024.kml"); print(file.info(Helene_kml))
Helene_vec <- terra::vect(Helene_kml) %>% set.crs("EPSG:4269")
Helene_vec

(Helene_vec %>% as.lines() ) %>% lines()

# (Helene_vec <- read_sf(Helene_kml) %>% terra::vect() %>% set.crs("EPSG:4269") ) # NAD83
rm(Helene_kml)
print(st_crs(Helene_vec))

# debugonce(set.crs)
# set.crs(Helene_vec, "EPSG:4269" )
# print(st_crs(Helene_vec))


# Helene_vec %>% as.lines() %>% plot()


states_vec <- get_states_vec()
print(st_crs(states_vec))

# devtools::load_all("~/Spatial/FEMA/femar/");
cbsa_vec <- get_cbsa_vec(year = 2020L)
# methods(class="SpatVector")
# st_crs(cbsa_vec )


# plot with sf ------------------------------------------------------------

#
# Helene_sf <- st_transform(Helene_sf, crs = st_crs(cbsa_sf ))
# Helene_sf %<>% st_combine() %>% st_cast("LINESTRING")
#
# ?plot.sf
#
# par(mfrow=c(1,1))
# Helene_sf %>% st_geometry()%>% plot(axes=TRUE, graticule=TRUE, reset=FALSE, pch=20, col="blue", lwd=2
#                                     , xlim=c(-90,-80)
#                                     ,ylim=c(20, 40)
# )
# cbsa_sf  %>% st_geometry() %>% plot(add=TRUE, border="grey")
# states_sf  %>% st_geometry() %>% plot(add=TRUE, border="darkgrey", lwd=2)


# Helene120km -------------------------------------------------------------


Helene120km <- Helene_vec%>% as.lines() %>% buffer(width=120e3)

# plot with terra --------------------------------------------------------------
#help(package="terra")
# ?terra::plot

plot(cbsa_vec
     , xlim=c(-90,-75)
     ,ylim=c(25, 40)
     , border="grey"
     ,main="Hurricane Helene 2024")
lines(states_vec, col="black", lwd=1, alpha=1)
points(Helene_vec,col="blue")
lines(Helene_vec,col="blue",lwd=1)
polys(Helene120km, col="cyan", alpha=0.2, border=NA)
# states_vec
terra::text(states_vec, 'STUSPS', cex=0.65)
# # Helene_png ----
#
# Helene_png <- file.path(Sys.getenv("R_WORK_DIR"), format(Sys.time(),"Helene_%Y%m%d_%H%M.png")); print(file.info(Helene_png))
# library(Cairo)
# dev.copy(device=Cairo::CairoPNG,filename = Helene_png, width = 10.0, height = 6.0, dpi=300, units="in")
# dev.off()
# print(file.info(Helene_png)['size'])
# browseURL(dirname(Helene_png))
