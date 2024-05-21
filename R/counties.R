get_counties_sf <- function(year=getOption("tigris_year")){
  # counties_dsn <- file.path(.census_workdir, sprintf("counties_%d.shp", getOption("tigris_year"))); print(file.info(counties_dsn))
  counties_gpkg <- file.path(.census_workdir, sprintf("counties_%d.gpkg", year)); print(file.info(counties_gpkg))
  counties_sf_rds <- file.path(.census_workdir, sprintf("counties_%d.rds", year)); print(file.info(counties_sf_rds))
  if(file.exists(counties_sf_rds)) {
    counties_sf <- readRDS(counties_sf_rds)
  } else if(file.exists(counties_gpkg)) {
    counties_sf <- st_read(counties_gpkg)
  } else {
    print(getOption("tigris_year"))
    # ?counties
    (counties_sf <- tigris::counties(cb=TRUE
                                 , year=getOption("tigris_year")
                                 , class="sf"
                                 , keep_zipped_shapefile =TRUE, progress_bar = FALSE))
    # str(counties_sf)
    # counties_sf$INTPTLAT <- as.numeric(counties_sf$INTPTLAT)
    # counties_sf$INTPTLON <- as.numeric(counties_sf$INTPTLON)

    # subset(fips_codes, subset= state %in% c('AS', 'MP','GU','VI','PR' # max longitud
    #                                         , 'HI','AK' # min longitud
    # ))

    # counties_sf <- counties_sf%>% subset(STATEFP!="02" & STATEFP !="15" & STATEFP<60)

    print(dim(counties_sf))


    ## projected CRS: WGS 84 / Pseudo-Mercator ---------------------------------

    counties_sf <- st_transform(counties_sf, st_crs(3857))

    # add coastline -----------------------------------------------------------

    (coastline_sf <- tigris::coastline(keep_zipped_shapefile =TRUE) %>% st_transform(st_crs(3857))) # %>% subset(NAME!="Arctic")
    coastline_lst <- split(coastline_sf, f = coastline_sf$NAME)

    tmp_df <- lapply(coastline_lst, . %>% st_intersects(x=counties_sf,y=.) %>% (function(x) lengths(x)>0)) %>% as.data.frame()
print(summary(tmp_df))
    (counties_sf <- cbind(counties_sf, tmp_df))

    # show(counties_sf)
    # ?which
    #
    # imax <- which.max(counties_sf$INTPTLON==max(counties_sf$INTPTLON)); print(counties_sf[imax,])
    #
    #
    # imin <- which.max(counties_sf$INTPTLON==min(counties_sf$INTPTLON)); print(counties_sf[imin,])
    # ?st_write.sf
    # debugonce(st_write)
    # st_write(counties_sf, dsn=counties_dsn, append = FALSE)
    st_write(counties_sf, dsn=counties_gpkg, append = FALSE)
    saveRDS(counties_sf, counties_sf_rds);print(file.info(counties_sf_rds))
  }; counties_sf
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


#' NRI Hazard Info by County
#'
#' @param state character
#'
#' @return sf simple feature
#' @import sf
#' @export
#'
get_NRI_ctys_sf <- function(state) {
  # browser()
  # browseURL(.NRI_datadir)
  NRI_ctys_sf_Rds <- file.path(.NRI_workdir, "NRI_ctys_sf.Rds"); print(file.info(NRI_ctys_sf_Rds))

  if(file.exists(NRI_ctys_sf_Rds)) {
    NRI_ctys_sf <- readRDS(NRI_ctys_sf_Rds)
    return(NRI_ctys_sf)
  } else {
    NRI_GDB_ctys_gdb <- file.path(.NRI_datadir, "NRI_GDB_Counties.gdb"); stopifnot(dir.exists(NRI_GDB_ctys_gdb))
    # print(st_layers(NRI_GDB_ctys_gdb))

    # ?st_read
    # debugonce(st_read)

    if(missing(state)) {
      NRI_ctys_sf <- sf::st_read(dsn = NRI_GDB_ctys_gdb
                                 , layer = "NRI_Counties"
                                 ,quiet=TRUE)
    } else{
      statefips <- get_fips_code(state)
      str(tigris::fips_codes)
      NRI_ctys_sf <- sf::st_read(dsn = NRI_GDB_ctys_gdb
                                 , layer = "NRI_Counties"
                                 , query = sprintf("SELECT * FROM NRI_Counties where STATEFIPS  = '%d'", statefips))

    }

    NRI_ctys_sf$RISK_RATNG<- factor(NRI_ctys_sf$RISK_RATNG, levels = c("Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"
                                                                       # ,"Insufficient Data"
                                                                       ))
    # NRI_ctys_sf$RISK_RATNG <- fct_na_level_to_value(NRI_ctys_sf$RISK_RATNG,extra_levels  = "Insufficient Data" )

    print(table(NRI_ctys_sf$RISK_RATNG, useNA = "ifany"))

    NRI_ctys_sf$SOVI_RATNG <- factor(NRI_ctys_sf$SOVI_RATNG, levels = c( "Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"
                                                                   #      ,"Data Unavailable"
                                                                         ))
    print(levels(NRI_ctys_sf$SOVI_RATNG))
    # NRI_ctys_sf$SOVI_RATNG <- fct_na_level_to_value(NRI_ctys_sf$SOVI_RATNG,extra_levels  = "Data Unavailable" )

    print(table(NRI_ctys_sf$SOVI_RATNG, useNA = "ifany"))
    NRI_ctys_sf$RISK_RATNG2 <- forcats::fct_collapse(NRI_ctys_sf$RISK_RATNG
, 'High'=c('Very High','Relatively High')
, 'Moderate'="Relatively Moderate"
, "Low"=c("Relatively Low","Very Low"))


# add coastline ----------------------------------------------------------
    (coastline_sf <- tigris::coastline(keep_zipped_shapefile =TRUE) %>% st_transform(st_crs(3857))) # %>% subset(NAME!="Arctic")
    coastline_lst <- split(coastline_sf, f = coastline_sf$NAME)

    tmp_df <- lapply(coastline_lst, . %>% st_intersects(x=NRI_ctys_sf,y=.) %>% (function(x) lengths(x)>0)) %>% as.data.frame()
    print(summary(tmp_df))
    (NRI_ctys_sf <- cbind(NRI_ctys_sf, tmp_df))



    # ?sf::st_is_valid.sf
    table(polygon_ok <- sf::st_is_valid(NRI_ctys_sf), useNA = "ifany")
    NRI_ctys_sf[!polygon_ok ,  'STCOFIPS']
    if(!all(polygon_ok)){

      NRI_ctys_sf <- NRI_ctys_sf %>% sf::st_make_valid()
    }



    saveRDS(NRI_ctys_sf, NRI_ctys_sf_Rds)
  }
  print(summary(NRI_ctys_sf))
  return(NRI_ctys_sf)
}



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

