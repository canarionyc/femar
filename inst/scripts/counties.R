# function to obtain US county shape
get_US_county_2010_shape <- function() {
  dir <- Sys.getenv("TIGRIS_CACHE_DIR")
  url <- "http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip"
  destfile <- file.path(dir, basename(url))
  download.file(url, destfile = destfile)
  exdir <- fs::path_ext_remove(destfile)
  unzip(destfile, exdir = exdir)
  US <- read_sf(exdir)
  levels(US$NAME) <- iconv(levels(US$NAME), from = "latin1", to = "utf8")
  US
}

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
