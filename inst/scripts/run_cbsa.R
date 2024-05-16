# cbsa --------------------------------------------------------------------

cbsa_sf <- tigris::core_based_statistical_areas(cb=TRUE)
(cbsa_lcc_sf <- cbsa_sf %>% st_transform(st_crs(lcc)))
