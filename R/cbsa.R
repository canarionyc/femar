# configr::read.config()
lcc <- "+proj=lcc +lat_1=60 +lat_2=30 +lon_0=-60"

options("tigris_year"=2020L)
# get_cbsa_sf --------------------------------------------------------------------
#' @export
get_cbsa_sf <- function(){
  # ?core_based_statistical_areas
  #  tigris::core_based_statistical_areas is not currently available for years prior to 2010.

  states_sf <- tigris::states(cb=TRUE

                              , class="sf"
                              , keep_zipped_shapefile =TRUE, progress_bar = FALSE) %>%
    subset(STATEFP!="02" & STATEFP !="15" & STATEFP<60)
  ?tigris::load_tiger
  cbsa_sf <- tigris::core_based_statistical_areas(cb=TRUE, keep_zipped_shapefile=TRUE
                                                  , filter_by=sf::st_bbox(states_sf))
  print(cbsa_sf)
  # (cbsa_lcc_sf <- cbsa_sf %>% st_transform(st_crs(lcc)))
  # cbsa_sf%>% subset(CBSAFP=="37700")
  return(cbsa_sf)
}


# get_cbsa_names --------------------------------------------------------------
#' @export
get_cbsa_names <- function(){

  cbsa_names_fst <- file.path(the$CENSUS_WORKDIR, "cbsa_names.fst")

  if(file.exists(cbsa_names_fst)) {
    print(fst.metadata(cbsa_names_fst))
    cbsa_names <- read_fst(cbsa_names_fst, as.data.table = TRUE)
  } else {
    cbsa_names <-  get_cbsa_sf() %>% sf::st_drop_geometry()
    cbsa_names
    setDT(cbsa_names, key=c('CBSAFP')) %>% setcolorder()
    cbsa_names[, STUSPS := stri_extract_last_regex(NAME, pattern = "[A-Z]{2}(-[A-Z]{2})*")]
    print(cbsa_names[, sort(table(STUSPS, useNA = "ifany"), decreasing = TRUE)])
    cbsa_names[, NAMELSAD := NAMELSAD %>% iconv(to="ASCII//TRANSLIT") %>% toupper()]

    write_fst(cbsa_names, path = cbsa_names_fst);   print(file.info(cbsa_names_fst)['size'])
  }; str(cbsa_names)
  return(cbsa_names)
}
# str(cbsa_names)
# cbsa_names$NAME
# sort(cbsa_names$NAMELSAD) %>% clipr::write_clip()

# print(cbsa_names$NAMELSAD)

# MSA to County -----------------------------------------------------------

#' @importFrom readxl read_xlsx
get_cbsa_to_county <- function() {
# devtools::load_all("~/fstutils/",export_all = TRUE);
  list1_2023 <- readxl::read_xlsx(file.path(CENSUS_DATADIR, "list1_2023.xlsx")) %>% setDT() %>% sanitize()
  str(list1_2023)
  return(list1_2023)
  }



