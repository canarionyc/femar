
?load_tiger
(counties_sf <- tigris::counties(cb=FALSE,
                                 keep_zipped_shapefile = TRUE ))

(counties_lcc_sf <- counties_sf %>% st_transform(st_crs(lcc)))

?st_buffer
(counties_lcc_buff_sf <- st_buffer(counties_lcc_sf, dist=dist))

# coastal counties --------------------------------------------------------

plot(counties_lcc_buff_sf['NAME'])

(counties.coastline_lst <- st_intersects(counties_sf, coastline_sf))

(coastal_counties_sf <- counties_sf[lengths(counties.coastline_lst)>0,])

# coastal counties buffered -----------------------------------------------

(counties.coastline_buff_lst <- st_intersects(counties_lcc_buff_sf, coastline_lcc_sf))

(coastal_counties_lcc_sf  <- counties_lcc_sf %>% subset(lengths(counties.coastline_buff_lst)>0)) %>%
  subset(subset=STATEFP!="02" & STATEFP !="15" & STATEFP<60) %>%
  st_geometry() %>% plot()
