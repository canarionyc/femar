devtools::load_all("~/Spatial/FEMA/femar/")

# cbsa --------------------------------------------------------------------

cbsa_sf <- tigris::core_based_statistical_areas(cb=TRUE)

cbsa_sf%>%subset(CBSAFP == 26420)

(cbsa_lcc_sf <- cbsa_sf %>% st_transform(st_crs(lcc)))

cbsa_names <- get_cbsa_names()
str(cbsa_names)
stri_ex
stri_extract_last_regex(cbsa_names$NAME, pattern = "[A-Z]{2}(-[A-Z]{2})*")
