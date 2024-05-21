get_states_sf <- function(){
  # states_dsn <- file.path(.census_workdir, sprintf("states_%d.shp", getOption("tigris_year"))); print(file.info(states_dsn))
  states_gpkg <- file.path(.census_workdir, sprintf("states_%d.gpkg", getOption("tigris_year"))); print(file.info(states_gpkg))
  states_sf_rds <- file.path(.census_workdir, sprintf("states_%d.rds", getOption("tigris_year"))); print(file.info(states_sf_rds))
  if(file.exists(states_sf_rds)) {
    states_sf <- readRDS(states_sf_rds)
  }
  if(file.exists(states_gpkg)) {
    states_sf <- st_read(states_gpkg)
  } else {
    print(getOption("tigris_year"))
    ?states
    (states_sf <- tigris::states(cb=TRUE
                                 , year=getOption("tigris_year")
                                 , class="sf"
                                 , keep_zipped_shapefile =TRUE, progress_bar = FALSE))
    # str(states_sf)
    # states_sf$INTPTLAT <- as.numeric(states_sf$INTPTLAT)
    # states_sf$INTPTLON <- as.numeric(states_sf$INTPTLON)

    # subset(fips_codes, subset= state %in% c('AS', 'MP','GU','VI','PR' # max longitud
    #                                         , 'HI','AK' # min longitud
    # ))

    # states_sf <- states_sf%>% subset(STATEFP!="02" & STATEFP !="15" & STATEFP<60)

    print(length(states_sf$STUSPS))


    ## projected CRS: WGS 84 / Pseudo-Mercator ---------------------------------

    states_sf <- st_transform(states_sf, st_crs(3857)) #

    # add coastline -----------------------------------------------------------

    (coastline_sf <- tigris::coastline(keep_zipped_shapefile =TRUE) %>% st_transform(st_crs(3857))) # %>% subset(NAME!="Arctic")
    coastline_lst <- split(coastline_sf, f = coastline_sf$NAME)

    (tmp_df <- lapply(coastline_lst, . %>% st_intersects(x=states_sf,y=.) %>% (function(x) lengths(x)>0)) %>% as.data.frame() )

    (states_sf <- cbind(states_sf, tmp_df))

    # show(states_sf)
    # ?which
    #
    # imax <- which.max(states_sf$INTPTLON==max(states_sf$INTPTLON)); print(states_sf[imax,])
    #
    #
    # imin <- which.max(states_sf$INTPTLON==min(states_sf$INTPTLON)); print(states_sf[imin,])
    # ?st_write.sf
    debugonce(st_write)
    # st_write(states_sf, dsn=states_dsn, append = FALSE)
    st_write(states_sf, dsn=states_gpkg, append = FALSE)
    saveRDS(states_sf, states_sf_rds);print(file.info(states_sf_rds))
  }; states_sf
}



#' @import sf
#' @import stats
get_counties_within_cbsas <- function() {
  counties_sf <- counties(cb = FALSE, year=2021)
  print(nrow(counties_sf))
  print(counties_sf %>% subset(CBSAFP == 26420)) # Houston, TX greater metropolitan area

  cbsa_sf <- aggregate(counties_sf[, c('ALAND', 'AWATER')], list(CBSAFP=counties_sf$CBSAFP), FUN=sum)
  print(cbsa_sf)

  counties_sf %>% subset(CBSAFP == 26420, select='NAMELSAD')  %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)
  cbsa_sf %>% subset(CBSAFP == 26420, select='CBSAFP')  %>% plot(add=TRUE, lwd=2)

  return(cbsa_sf)
}
#
# par(mfrow=c(1,1))
#
#
# (counties <- st_drop_geometry(counties_sf));
# print(nrow(counties))
# setDT(counties, key=c('GEOID'))
# ?anyDuplicated
# stopifnot(anyDuplicated(counties, by=key(counties))==0)
#
#
# (counties_gsf<- counties_sf %>% group_by(CBSAFP))
# (cbsa_agg_counties_sf <-  counties_gsf %>% summarise(COUNTYFP_LIST=list(COUNTYFP)))
# cbsa_agg_counties_sf
# cbsa_agg_counties_sf%>% subset(CBSAFP == 26420, select='COUNTYFP_LIST')
# cbsa_agg_counties_sf%>% subset(CBSAFP == 26420 ) %>% st_geometry()  %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)
#
# nrow(counties_sf)

#' @title get NRI Hazard Info by County with CBSA
#'
#' @return simple feature
#' @export
#'
get_cty_cbsa_NRI_sf <- function(){
  counties_sf <- counties(cb = FALSE, year=2021)
  stopifnot(anyDuplicated(counties_sf$GEOID)==0)

  NRI_ctys_dt <- get_NRI_ctys_dt()
  stopifnot(anyDuplicated(NRI_ctys_dt$STCOFIPS)==0)
  cty_cbsa_NRI_sf <- merge(counties_sf, NRI_ctys_dt, by.x=c('GEOID'), by.y=c('STCOFIPS'))
  return(cty_cbsa_NRI_sf)
}

