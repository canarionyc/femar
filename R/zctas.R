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

#' @import terra
#' @export



?purrr::compose
?tigris::zctas
zctas_vect <- purrr::compose(terra::vect, tigris::zctas)

# state_zcta3_sf ----

get_zip3 <- function(state, year=2010){ # ZCTAs are only available by state for 2000 and 2010.

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


# zip_code_short_vect -----------------------------------------------------

get_zip_code_short_vect <- function() {

  zip_code_short_vect_gpkg <- file.path(the$FEMA_WORKDIR, "zip_code_short_vect.gpkg")
  if(file.exists(zip_code_short_vect_gpkg)) {
    return(terra::vect(zip_code_short_vect_gpkg))
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

?tigris::zctas
  zip_code_short_vect <- terra::aggregate(zctas_vect[, c('ZCTA3CE20', 'ALAND20', 'AWATER20')]
                                          , by='ZCTA3CE20', fun=myfun )

  zip_code_short_vect
  class(zip_code_short_vect)

  writeVector(zip_code_short_vect,zip_code_short_vect_gpkg)
  return(zip_code_short_vect)
}


