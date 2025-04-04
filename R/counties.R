#' get counties
#'
#'
#' Tigris shapefile for counties
#' in WGS 84 / Pseudo-Mercator
#'
#' @param year integer
#'
#' @return simple feature
#' @export
#'
#'
get_counties_sf <- function(year=getOption("tigris_year", 2020L)){
  # counties_dsn <- file.path(the$CENSUS_WORKDIR, sprintf("counties_%d.shp", getOption("tigris_year"))); print(file.info(counties_dsn))
  # counties_gpkg <- file.path(the$CENSUS_WORKDIR, sprintf("counties_%d.gpkg", year)); print(file.info(counties_gpkg))
  # counties_sf_rds <- file.path(the$CENSUS_WORKDIR, sprintf("counties_%d.rds", year)); print(file.info(counties_sf_rds))
  # if(file.exists(counties_sf_rds)) {
  #   counties_sf <- readRDS(counties_sf_rds)
  # } else if(file.exists(counties_gpkg)) {
  #   counties_sf <- st_read(counties_gpkg)
  # } else {

  # ?counties
  counties_sf <- tigris::counties(cb=TRUE
                                  , year=year
                                  , class="sf"
                                  , keep_zipped_shapefile =TRUE, progress_bar = FALSE)%>%
    subset(subset=STATEFP!="02" & STATEFP !="15" & STATEFP<60)
  # str(counties_sf)
  # counties_sf$INTPTLAT <- as.numeric(counties_sf$INTPTLAT)
  # counties_sf$INTPTLON <- as.numeric(counties_sf$INTPTLON)

  # subset(fips_codes, subset= state %in% c('AS', 'MP','GU','VI','PR' # max longitud
  #                                         , 'HI','AK' # min longitud
  # ))

  # counties_sf <- counties_sf%>% subset(STATEFP!="02" & STATEFP !="15" & STATEFP<60)

  print(dim(counties_sf))

  ## projected CRS: WGS 84 / Pseudo-Mercator ---------------------------------


  counties_sf$STCOFIPS <- paste0(counties_sf$STATEFP, counties_sf$COUNTYFP)

  if(FALSE){
    # add coastline -----------------------------------------------------------

    (coastline_sf <- tigris::coastline(keep_zipped_shapefile =TRUE) ) # %>% subset(NAME!="Arctic")
    coastline_lst <- split(coastline_sf, f = coastline_sf$NAME)

    tmp_df <- lapply(coastline_lst, . %>% st_intersects(x=counties_sf,y=.) %>% (function(x) lengths(x)>0)) %>% as.data.frame()
    print(summary(tmp_df))
    (counties_sf <- cbind(counties_sf, tmp_df))
  }

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
  #st_write(counties_sf, dsn=counties_gpkg, append = FALSE)
  #   saveRDS(counties_sf, counties_sf_rds);print(file.info(counties_sf_rds))
  # };

  counties_sf$STCOFIPS <- paste0(counties_sf$STATEFP, counties_sf$COUNTYFP)

  stopifnot(unique(nchar(counties_sf$STCOFIPS))==5L)

  counties_sf <- st_transform(counties_sf, st_crs(3857))

  return(counties_sf)
}


#' @rawNamespace import(stats, except = filter)
#' @import sf

get_counties_within_cbsas <- function(year=2021) {
  counties_sf <- counties(cb = FALSE, year=2021)
  print(counties_sf)
  print(nrow(counties_sf))

  print(counties_sf %>% subset(CBSAFP == 26420)) # Houston, TX greater metropolitan area


  cbsa_sf <- tigris::core_based_statistical_areas(cb = FALSE, year=2021)

  # cbsa_sf <- aggregate(counties_sf[, c('ALAND', 'AWATER')], list(CBSAFP=counties_sf$CBSAFP), FUN=sum)
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

#' Get NRI Hazards table by County
#'
#' @return a data.table
#' @import forcats
#' @export
#'
get_NRI_counties_dt <- function() {
  # browser()
  .datatable.aware <- TRUE

  NRI_counties_rds <- file.path(the$NRI_WORKDIR, "NRI_counties.rds"); print(file.info(NRI_counties_rds))
  NRI_counties_fst <- file.path(the$NRI_WORKDIR, "NRI_counties.fst"); print(file.info(NRI_counties_fst))
  if(file.exists(NRI_counties_rds)) {

    NRI_counties_dt <- readRDS(NRI_counties_rds)
  } else if(file.exists(NRI_counties_fst)) {
    print(fst.metadata(NRI_counties_fst))
    NRI_counties_dt <- read_fst(NRI_counties_fst, as.data.table = TRUE)
  } else {
    # list.files(the$NRI_DATADIR)
    NRI_counties_dt <- fread(file.path(the$NRI_DATADIR, "NRI_Table_Counties.csv")
                             , na.strings = c("Data Unavailable","Insufficient Data","Not Applicable")
                             , colClasses = list(character='STATEFIPS','STCOFIPS'))
    str(NRI_counties_dt)
    NRI_counties_dt[, OID_:=NULL]
    # NRI_counties_dt[, STATEFIPS:=sprintf("%02d", STATEFIPS)]
    str(NRI_counties_dt$STATEFIPS)
    print(range(NRI_counties_dt$STCOFIPS))
    # NRI_counties_dt[, STCOFIPS:=sprintf("%05d", STCOFIPS)]
    str(NRI_counties_dt$STCOFIPS)
    # NRI_counties_sf <- get_NRI_counties_sf()
    # NRI_counties <- data.table(st_drop_geometry(NRI_counties_sf), stringsAsFactors = TRUE)
    area_cols <- grep("AREA$", names(NRI_counties_dt ), value=TRUE) # in sq miles
    print(area_cols)
    # NRI_counties_dt[, AREA:=set_units(AREA, "mile^2")]
    # str(NRI_counties_dt$AREA)
    NRI_counties_dt[, (area_cols):=lapply(.SD, set_units,  "mile^2"), .SDcols = area_cols]
    str(NRI_counties_dt[,  .SD,.SDcols = area_cols])
    # ?fct_relevel

    NRI_counties_dt[, RISK_RATNG:=factor(RISK_RATNG, levels = c("Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"
                                                                # ,"Insufficient Data"
    ))]
    print(table(NRI_counties_dt$RISK_RATNG, useNA = "ifany"))

    NRI_counties_dt[, SOVI_RATNG:=factor(SOVI_RATNG, levels = c( "Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"
                                                                 #    ,"Data Unavailable"
    ))]
    print(levels(NRI_counties_dt$SOVI_RATNG))
    print(table(NRI_counties_dt$SOVI_RATNG, useNA = "ifany"))
    write_fst(NRI_counties_dt, path = NRI_counties_fst);   print(file.info(NRI_counties_fst))
    saveRDS(NRI_counties_dt, NRI_counties_rds);   print(file.info(NRI_counties_rds))
  }# ; str(NRI_counties_dt)
  print(summary(NRI_counties_dt))
  return(NRI_counties_dt)
}


#' NRI Hazard Info by County Simple Features
#'
#' @param statefips character vector
#'
#' @return sf simple feature
#' @import sf
#' @export
#'
get_NRI_counties_sf <- function(statefips) {
  # browser()
  # browseURL(the$NRI_DATADIR)

  # layer <- "NRI_Counties"
  NRI_GDB_counties_gdb <- file.path(the$NRI_DATADIR, "NRI_GDB_Counties.gdb"); stopifnot(dir.exists(NRI_GDB_counties_gdb))
  # print(st_layers(NRI_GDB_counties_gdb))

  # ?st_read
  # debugonce(st_read)
  query <- "select STATEABBRV,STATEFIPS,COUNTY,COUNTYTYPE,COUNTYFIPS,STCOFIPS,POPULATION,BUILDVALUE,AREA,RISK_VALUE
      ,RISK_SCORE
    ,EAL_SCORE,EAL_RATNG,EAL_VALB
    ,ALR_VALB
    ,SOVI_SCORE,SOVI_RATNG,SOVI_SPCTL,RESL_SCORE,RESL_RATNG,RESL_SPCTL,RESL_VALUE,CRF_VALUE
    ,AVLN_ALRB,CFLD_ALRB,CWAV_ALRB,ERQK_ALRB,HAIL_ALRB,HWAV_ALRB,HRCN_ALRB,ISTM_ALRB,LNDS_ALRB,LTNG_ALRB,RFLD_ALRB,SWND_ALRB,TRND_ALRB,TSUN_ALRB,VLCN_ALRB,WFIR_ALRB,WNTW_ALRB
  FROM NRI_Counties"

  query <- "select * FROM NRI_Counties"

  if(!missing(statefips)) {
    conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    # str(tigris::fips_codes)
    # ?sf::st_read
    query <- paste(query,glue::glue_sql("where STATEFIPS in ({fips*})", fips=statefips, .con=conn)); cat(query,"\n")
  }
  NRI_counties_sf <- sf::st_read(dsn = NRI_GDB_counties_gdb
                                 , query = query
  )



  # NRI_counties_sf$RISK_RATNG<- factor(NRI_counties_sf$RISK_RATNG, levels = c("Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"
  #                                                                            # ,"Insufficient Data"
  # ))
  # # NRI_counties_sf$RISK_RATNG <- fct_na_level_to_value(NRI_counties_sf$RISK_RATNG,extra_levels  = "Insufficient Data" )
  #
  # print(table(NRI_counties_sf$RISK_RATNG, useNA = "ifany"))
  #
  # NRI_counties_sf$SOVI_RATNG <- factor(NRI_counties_sf$SOVI_RATNG, levels = c( "Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"
  #                                                                              #      ,"Data Unavailable"
  # ))
  # print(levels(NRI_counties_sf$SOVI_RATNG))
  # # NRI_counties_sf$SOVI_RATNG <- fct_na_level_to_value(NRI_counties_sf$SOVI_RATNG,extra_levels  = "Data Unavailable" )
  #
  # print(table(NRI_counties_sf$SOVI_RATNG, useNA = "ifany"))
  # NRI_counties_sf$RISK_RATNG2 <- forcats::fct_collapse(NRI_counties_sf$RISK_RATNG
  #                                                      , 'High'=c('Very High','Relatively High')
  #                                                      , 'Moderate'="Relatively Moderate"
  #                                                      , "Low"=c("Relatively Low","Very Low"))

  # NRI_counties_sf <- NRI_add_categoricals(NRI_counties_sf)
  # add coastline ----------------------------------------------------------
  # (coastline_sf <- tigris::coastline(keep_zipped_shapefile =TRUE) %>% st_transform(st_crs(3857))) # %>% subset(NAME!="Arctic")
  # coastline_lst <- split(coastline_sf, f = coastline_sf$NAME)
  #
  # tmp_df <- lapply(coastline_lst, . %>% st_intersects(x=NRI_counties_sf,y=.) %>% (function(x) lengths(x)>0)) %>% as.data.frame()
  # print(summary(tmp_df))
  # (NRI_counties_sf <- cbind(NRI_counties_sf, tmp_df))

  # ?sf::st_is_valid.sf
  # polygon_ok <- sf::st_is_valid(NRI_counties_sf)
  # print(table(polygon_ok, useNA = "ifany"))

  # if(!all(polygon_ok)){
  #   print(NRI_counties_sf[!polygon_ok , 'STCOFIPS'])
  #   NRI_counties_sf <- NRI_counties_sf %>% sf::st_make_valid()
  # }

  str(NRI_counties_sf)
  return(NRI_counties_sf)
}

NRI_add_categoricals <- function(NRI_dt) {
  NRI_dt$RISK_RATNG<- factor(NRI_dt$RISK_RATNG
                             , levels = c("Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"
                                          # ,"Insufficient Data"
                             ))
  # NRI_dt$RISK_RATNG <- fct_na_level_to_value(NRI_dt$RISK_RATNG,extra_levels  = "Insufficient Data" )

  print(table(NRI_dt$RISK_RATNG, useNA = "ifany"))

  NRI_dt$SOVI_RATNG <- factor(NRI_dt$SOVI_RATNG, levels = c( "Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"
                                                             #      ,"Data Unavailable"
  ))
  print(levels(NRI_dt$SOVI_RATNG))
  # NRI_dt$SOVI_RATNG <- fct_na_level_to_value(NRI_dt$SOVI_RATNG,extra_levels  = "Data Unavailable" )

  print(table(NRI_dt$SOVI_RATNG, useNA = "ifany"))
  NRI_dt$RISK_RATNG2 <- forcats::fct_collapse(NRI_dt$RISK_RATNG
                                              , 'High'=c('Very High','Relatively High')
                                              , 'Moderate'="Relatively Moderate"
                                              , "Low"=c("Relatively Low","Very Low"))

  return(invisible(NRI_dt))
}


#' @title get NRI Hazard Info by County with CBSA
#'
#' @return simple feature
#' @export
#'
get_cty_cbsa_NRI_sf <- function(){
  counties_sf <- counties(cb = FALSE, year=2021)
  stopifnot(anyDuplicated(counties_sf$GEOID)==0)

  NRI_counties_dt <- get_NRI_counties_dt()
  stopifnot(anyDuplicated(NRI_counties_dt$STCOFIPS)==0)
  cty_cbsa_NRI_sf <- merge(counties_sf, NRI_counties_dt, by.x=c('GEOID'), by.y=c('STCOFIPS'))
  return(cty_cbsa_NRI_sf)
}

# not used: function to obtain US county shape
# get_US_county_2010_shape <- function() {
#   dir <- Sys.getenv("TIGRIS_CACHE_DIR")
#   url <- "http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip"
#   destfile <- file.path(dir, basename(url))
#   download.file(url, destfile = destfile)
#   exdir <- fs::path_ext_remove(destfile)
#   unzip(destfile, exdir = exdir)
#   US <- read_sf(exdir)
#   levels(US$NAME) <- iconv(levels(US$NAME), from = "latin1", to = "utf8")
#   US
# }
