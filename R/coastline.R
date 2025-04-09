get_coastline_sf <- function(crs) {
  states_sf <- tigris::states(cb=TRUE
                              , class="sf"
                              , keep_zipped_shapefile =TRUE, progress_bar = FALSE) %>%
    subset(STATEFP!="02" & STATEFP !="15" & STATEFP<60)
  # print(st_bbox(states_sf))
  # ?tigris::coastline
  coastline_sf <- tigris::coastline(keep_zipped_shapefile =TRUE, filter_by=st_bbox(states_sf))  %>%
    subset(!NAME %in% c('Atlántico', "Arctic",'Caribe'))
  # print(coastline_sf)

  if(!missing(crs)) {
    coastline_sf <- coastline_sf %>% st_transform(st_crs(crs))
  }
  print(coastline_sf)
  # ?plot.sf
  # plot(coastline_sf['NAME'])

  return(coastline_sf)
}


#' Add length of coastline as 'coastal_length'
#'
#' @param counties_sf simple feature
#' @param coastline_sf simple feature
#'
#' @returns simple feature
#' @export
#'
add_coastal_length <- function(counties_sf, coastline_sf){
  print(counties_sf)
  cty_coastline.sgbp <- st_intersects(counties_sf,coastline_sf, sparse = TRUE)
  # print(cty_coastline.sgbp)
  counties_sf$coastal_length <- sapply(cty_coastline.sgbp  , function(i) {
    # browser()
    coastline_sf[i,] %>% st_geometry() %>% st_length() %>% sum() %>% round(digits=0L)}
  )

  return(invisible(counties_sf))
}

add_distance_to_coastline <- function(counties_sf,coastline_sf){
  out <- st_distance(st_centroid(st_geometry(counties_sf[,])), coastline_sf) %>% round(digits=0L)
  # str(out)

  out2 <- apply(out, 1, min)
  # str(out2)
  counties_sf$distance_to_coastline <- out2
  return(invisible(counties_sf))
}

add_coastline <- function(counties_sf,coastline_sf){
  if(missing(coastline_sf)) {
    coastline_sf <- get_coastline_sf(st_crs(counties_sf))
  }
  counties_sf <- add_coastal_length(counties_sf, coastline_sf)
  counties_sf <- add_distance_to_coastline(counties_sf, coastline_sf)
  return(invisible(counties_sf))
}
