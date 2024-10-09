# best_tracks_sf GOOD -------------------------------------------------------------

devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE); (best_tracks_sf <- get_best_tracks_sf())
best_tracks_sf
st_crs(best_tracks_sf)
str(best_tracks_sf)

(Kt_tracks_sf <- best_tracks_sf %>% subset(NAME=="KATRINA" & SEASON==2005))
Kt_tracks_4269_sf <- st_transform(Kt_tracks_sf, crs = 4269)
Kt_tracks_4269_vec <- vect(Kt_tracks_4269_sf)
methods(class = "SpatVector")
xmin(Kt_tracks_4269_vec)

?terra::crop
st_bbox(Kt_tracks_4269_vec)
par(mfrow=c(1,1))




cbsa_vec <- terra::vect(cbsa_sf)
plot(cbsa_sf)
st_crs(cbsa_sf)

st_bbox(Kt_tracks_4269_sf) %>% multiply_by(0.8)

Kt_bbox <- st_bbox(Kt_tracks_4269_sf)
library(magrittr)
c(xmin, ymin, xmax, ymax) %$% 0.80*Kt_bbox
?st_crop
Kt_tracks_4269_sf %>% # st_geometry() %>%
  st_crop(0.80*Kt_bbox) %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE, lwd=2)

?terra::crop
?terra::expanse
?terra::clearance
0.8*ext(Kt_tracks_4269_vec)
Kt_tracks_4269_vec %>% plot()


# Kt_tracks_4269_sf -------------------------------------------------------
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE); (best_tracks_sf <- get_best_tracks_sf())

(Kt_tracks_sf <- best_tracks_sf %>% subset(NAME=="KATRINA" & SEASON==2005))
Kt_tracks_4269_sf <- st_transform(Kt_tracks_sf, crs = 4269)
Kt_tracks_4269_vec <- vect(Kt_tracks_4269_sf)

cbsa_sf <- get_cbsa_sf(year = 2010L)
st_crs(Kt_tracks_4269_sf)==st_crs(cbsa_sf)

terra::crop(cbsa_vec, 1.5*ext(Kt_tracks_4269_vec)) %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)
Kt_tracks_4269_sf %>% plot(add=TRUE, lwd=2, col="blue")


# -------------------------------------------------------------------------


plot(cbsa_sf['NAME'], add=TRUE)
