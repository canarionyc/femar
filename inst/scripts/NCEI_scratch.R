
# IBTrACS.ALL.list.v04r00.points ------------------------------------------

get_best_tracks_sf <- function(){
  best <-get_best_tracks()
  browser()
  (sel <- unique(c(1:which(names(best)=='USA_SSHS')
                   , grep("^USA", names(best))
                   , which(names(best)=='AVERAGE_RADIUS_OF_HURRICANE_OR_TROPICAL_STORM_FORCE_WINDS')
  )))
  (best_sf <- st_as_sf( best[BASIN=="NA"&
                               #  SEASON==set_units(2005,"Year") &
                               WMO_WIND>set_units(34, "knot") # exclude Tropical Depressions
                             #                                   & WMO_AGENCY=="hurdat_atl"
                             , .SD, .SDcols = sel
  ]  ,crs = 'EPSG:4269', coords = c("LON","LAT")))
  return(invisible(best_sf))
}

