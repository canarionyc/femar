
# sovi.tmap ---------------------------------------------------------------

levels(NRI_ctys_sf$SOVI_RATNG)
?fct_na_value_to_level
NRI_ctys_sf$SOVI_RATNG <- fct_na_level_to_value(NRI_ctys_sf$SOVI_RATNG,extra_levels  = "Data Unavailable" )
levels(NRI_ctys_sf$SOVI_RATNG)
str(NRI_ctys_sf$SOVI_RATNG)

(sovi.tmap <- NRI_ctys_sf %>% subset(STATEFIPS  =='06') %>%
    tm_shape() + tm_polygons(fill='SOVI_RATNG',fill_alpha = 0.5))

print(sovi.tmap)
