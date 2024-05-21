
# states_sf ---------------------------------------------------------------


states_sf <- states(cb=TRUE,keep_zipped_shapefile =TRUE)

qtm(states_sf)

# NRI_states_dt --------------------------------------------------------

devtools::load_all("~/Spatial/FEMA/femar"); debugonce(get_NRI_states_dt); NRI_states_dt <- get_NRI_states_dt()


NRI_states_dt[, .(sum(HRCN_EVNTS, na.rm = TRUE))]


