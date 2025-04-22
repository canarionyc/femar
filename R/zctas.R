# zctas ----
# options(tigris_use_cache = TRUE)
# getOption("tigris_use_cache")

get_zctas_sf <-  function(year=getOption("tigris_year", 2020)){
  zctas_sf_rds <- file.path(the$CENSUS_WORKDIR, sprintf("zctas_%d.rds", year)); print(file.info(zctas_sf_rds))
  if(file.exists(  zctas_sf_rds)) {
    zctas_sf <- readRDS(zctas_sf_rds)
  } else {
    # ?load_tiger
    zctas_sf <- zctas(cb = TRUE, starts_with = NULL
                      , year = year
                      #                   , state = NULL
                      , keep_zipped_shapefile = TRUE
    )
    saveRDS(zctas_sf, zctas_sf_rds); print(file.info(zctas_sf_rds))
  }
  return(zctas_sf)
}


get_zctas_sf <- purrr::partial(tigris::zctas, keep_zipped_shapefile = TRUE)

# state_zcta3_sf ----

get_zip3_2010 <- function(state, year=2010){ # ZCTAs are only available by state for 2000 and 2010.

  debugonce(zctas)

  state_zcta_rds <- file.path(the$CENSUS_WORKDIR, sprintf("%s_zcta_%d_sf.rds",state, year)); print(file.info(state_zcta_rds))
  state_zcta3_rds <- file.path(the$CENSUS_WORKDIR, sprintf("%s_zcta3_%d_sf.rds",state, year)); print(file.info(state_zcta3_rds))
  state_zcta3_gpkg <- file.path(the$CENSUS_WORKDIR, sprintf("%s_zcta3_%d_sf.gpkg",state, year)); print(file.info(state_zcta3_gpkg))
  if(!file.exists(state_zcta3_gpkg)) {
    state_zcta_sf <- zctas(cb=FALSE,
                           year=year,
                           state = state
                           , keep_zipped_shapefile=TRUE)
    print(state_zcta_sf)
    # plot(st_geometry(state_zcta_sf))
    # terra::plot(terra::vect(state_zcta_sf), 'ZCTA5CE10', main=state)
    # ?aggregate.sf
    state_zcta_sf$ZCTA3CE10 <- substr(state_zcta_sf$ZCTA5CE10, 1,3)
    print(state_zcta_sf['ZCTA3CE10'])
    # ?st_union
    #    (tmp_sf <- state_zcta_sf[c(   'ALAND10', 'AWATER10'  ,'ZCTA3CE10')][1:3,])
    state_zcta3_sf <- aggregate(state_zcta_sf[,c('ALAND10', 'AWATER10')], by=list(ZCTA3CE10=state_zcta_sf$ZCTA3CE10), FUN=base::sum )
    # names(state_zcta3_sf)[1] <- 'ZCTA3CE10'

    # plot(st_geometry(state_zcta3_sf))
    # terra::plot(terra::vect(state_zcta3_sf), 'ZCTA3CE10')
    #
    # state_zcta3_sf <- unionSpatialPolygons(state_zcta_sf, substr(state_zcta_sf$zctaCE10, 1,3))
    # plot(state_zcta3_sf )
    saveRDS(state_zcta3_sf, file=state_zcta3_rds); cat(state_zcta3_rds, file.size(state_zcta3_rds), "bytes\n")
    st_write(state_zcta3_sf, dsn = state_zcta3_gpkg , layer = "State_ZCTA3")
  }
  else {
    #   state_zcta3_sf <- readRDS(file=state_zcta3_rds)
    state_zcta3_sf <- st_read(dsn = state_zcta3_gpkg , layer = "State_ZCTA3")
  }
  state_zcta3_sf
}


# zip_code_short_sf-----------------------------------------------------

get_zip_code_short_sf <- function() {

  zip_code_short_gpkg <- file.path(the$FEMA_WORKDIR, "zip_code_short.gpkg")
  if(file.exists(zip_code_short_gpkg)) {
    return(sf::st_read(zip_code_short_gpkg))
  }
  # ?aggregate

  # ?sum
  # myfun <- function(..., na.rm=FALSE) {
  # #  browser()
  #   str(list(...))
  #   base::sum(..., na.rm = na.rm)
  #   }
  #
  # debugonce(myfun)

?states
  states_sf <- tigris::states(cb=TRUE, resolution =  '20m', keep_zipped_shapefile =TRUE) %>%
    subset(! STATEFP %in% c('02','15') & as.integer(STATEFP) <60)
  str(states_sf)
  st_crs(states_sf)
states_sf %>% st_geometry() %>% plot(axes=TRUE, graticule=TRUE, reset=TRUE, las=2)

  ?tigris::zctas
?load_tiger
  zctas_sf <-zctas(cb=TRUE
                   # , starts_with = '902'
                   #                     , state='CA'
                   , year=2020
                   , keep_zipped_shapefile =TRUE
                   , filter_by=st_bbox(states_sf))
print(st_bbox(zctas_sf))
  # zctas_sf$ZCTA3CE20 <- substr(zctas_sf$ZCTA5CE20, 1,3)
  ?aggregate.sf
  # methods("aggregate")
  # debugonce(sf:::aggregate.sf)
  zip_code_short_sf<- aggregate(zctas_sf[, c( 'ALAND20', 'AWATER20')]
                                , by=list(ZCTA3CE20=substr(zctas_sf$ZCTA5CE20, 1,3)), FUN=sum ) %>%
    st_transform("epsg:3857")

  print(zip_code_short_sf)

  zip_code_short_sf %>% st_geometry() %>% plot(axes=TRUE, graticule=TRUE, reset=TRUE, las=2, col="grey")

  print(object.size(zip_code_short_sf)/1024^2)
  print(st_crs(zip_code_short_sf))
  print(st_is_longlat(st_geometry(zip_code_short_sf)))
  stopifnot(all(st_is_valid(zip_code_short_sf)))

  write_sf(zip_code_short_sf,zip_code_short_gpkg)
  return(zip_code_short_sf)
}


get_zip_code_short_neighbours <- function(get_zip_code_short_sf) {
  zip_code_short_sf <- st_simplify(zip_code_short_sf, preserveTopology = TRUE,dTolerance = 1e3)
  print(zip_code_short_sf)


  # zip_code_short_sf.2 <- st_crop(zip_code_short_sf, st_bbox(states_sf))
  which(st_is_empty(zip_code_short_sf))

  # zip_code_short_sf[81,] %>% st_geometry() %>% plot(axes=TRUE, graticule=TRUE, reset=TRUE, las=2)
  # zip_code_short_sf[81,]  %>% st_geometry() %>% st_simplify(preserveTopology = TRUE,dTolerance = 1e3) %>% plot(axes=TRUE, graticule=TRUE, reset=TRUE, las=2)
  # ?st_precision
  # # st_set_precision(zip_code_short_sf,1/500e3)
  # # st_precision(zip_code_short_sf)
  # ?st_simplify

  coords_sf <- st_centroid(st_geometry(zip_code_short_sf))

  # debugonce(poly2nb)
  poly.nb <- poly2nb(st_geometry(zip_code_short_sf),snap = 1e3)
  poly.nb
  plot(poly.nb, coords_sf); title(main="poly2nb")



  ?knearneigh
  knn4 <- knearneigh(coords_sf,k=4L)
  knn4.nb <- knn2nb(knn4, row.names = zip_code_short_sf$ZCTA3CE20)
  knn4.nb

  plot(knn4.nb, coords_sf); title( main="knn2nb")
  return(list(knn4.nb=knn4.nb, poly.nb=poly.nb, coords_sf=coords_sf))
}
