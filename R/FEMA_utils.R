get_FEMA_regions <- function(){
  dsn <- file.path(.fema_datadir, "FEMA_Regions")

  FEMA_regions_sf <- st_read(dsn)
  print(FEMA_regions_sf)
  plot(st_geometry(FEMA_regions_sf))
  return(FEMA_regions_sf)
}

