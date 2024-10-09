# configr::read.config()
lcc <- "+proj=lcc +lat_1=60 +lat_2=30 +lon_0=-60"

# get_cbsa_sf --------------------------------------------------------------------
#' @export
get_cbsa_sf <- function(year = getOption("tigris_year",2020L)){
  # ?core_based_statistical_areas
  #  tigris::core_based_statistical_areas is not currently available for years prior to 2010.
  cbsa_sf <- tigris::core_based_statistical_areas(cb=TRUE, year=year, keep_zipped_shapefile=TRUE)
  # (cbsa_lcc_sf <- cbsa_sf %>% st_transform(st_crs(lcc)))
  # cbsa_sf%>% subset(CBSA=="37700")
  return(cbsa_sf)
}

#' @import terra
get_cbsa_vec <- purrr::compose(terra::vect, get_cbsa_sf)

# cbsa_names --------------------------------------------------------------
#' @export
get_cbsa_names <- function(){

  cbsa_names_fst <- file.path(.census_workdir, "cbsa_names.fst"); print(file.info(cbsa_names_fst))

  if(file.exists(cbsa_names_fst)) {
    print(fst.metadata(cbsa_names_fst))
    cbsa_names <- read_fst(cbsa_names_fst, as.data.table = TRUE)
  } else {
    cbsa_sf <- get_cbsa_sf()
    cbsa_names <- st_drop_geometry(cbsa_sf)
    cbsa_names
    setDT(cbsa_names, key=c('CBSAFP')) %>% setcolorder()
    cbsa_names[, STUSPS := stri_extract_last_regex(NAME, pattern = "[A-Z]{2}(-[A-Z]{2})*")]
    print(cbsa_names[, sort(table(STUSPS, useNA = "ifany"), decreasing = TRUE)])
    cbsa_names[, NAMELSAD := NAMELSAD %>% iconv(to="ASCII//TRANSLIT") %>% toupper()]
    str(cbsa_names)
    write_fst(cbsa_names, path = cbsa_names_fst);   print(file.info(cbsa_names_fst)['size'])
  }; str(cbsa_names)
  return(cbsa_names)
}
# str(cbsa_names)
# cbsa_names$NAME
# sort(cbsa_names$NAMELSAD) %>% clipr::write_clip()

# print(cbsa_names$NAMELSAD)



