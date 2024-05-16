# configr::read.config()
lcc <- "+proj=lcc +lat_1=60 +lat_2=30 +lon_0=-60"
# cbsa_names --------------------------------------------------------------


get_cbsa_names <- function(){
  cbsa_names_fst <- file.path(.census_workdir, "cbsa_names.fst"); print(file.info(cbsa_names_fst))

  if(file.exists(cbsa_names_fst)) {
    print(fst.metadata(cbsa_names_fst))
    cbsa_names <- read_fst(cbsa_names_fst, as.data.table = TRUE)
  } else {
    cbsa_sf <- core_based_statistical_areas(cb=TRUE, keep_zipped_shapefile=TRUE)
    cbsa_names <- st_drop_geometry(cbsa_sf)
    cbsa_names
    setDT(cbsa_names, key=c('CBSAFP')) %>% setcolorder()
    cbsa_names$NAMELSAD <- cbsa_names$NAMELSAD %>% iconv(to="ASCII//TRANSLIT") %>% toupper()
    write_fst(cbsa_names, path = cbsa_names_fst);   print(file.info(cbsa_names_fst))
  }; str(cbsa_names)

  cbsa_names
}

# str(cbsa_names)
# cbsa_names$NAME
# sort(cbsa_names$NAMELSAD) %>% clipr::write_clip()

# print(cbsa_names$NAMELSAD)




