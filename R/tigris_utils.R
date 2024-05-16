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

