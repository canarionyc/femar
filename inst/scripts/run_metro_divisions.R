
# setup -------------------------------------------------------------------

devtools::load_all("~/Spatial/FEMA/femar/")


metdiv_sf <- tigris::metro_divisions(#year=2022
  , keep_zipped_shapefile=TRUE)

metdiv <- metdiv_sf  %>% terra::vect()
as.data.frame(metdiv)

metdiv %>% subset(metdiv$CSAFP=="480")
metdiv %>% subset(metdiv$CBSAFP=="28880")
metdiv %>% subset(grepl("NY",metdiv$NAME))
terra::plot(metdiv)
