#' @title gt census tracts
#
#' Because blocks nest in tracts, and tract IDs even form the first part of block
#' IDs, it's relatively straightforward to generate tract crosswalks from block
#' crosswalks.
#' @param year int tigris year
#' @return simple feature
#' @export
get_tracts_sf <- function( stateabbr = NULL
                           ,county = NULL
                           ,year=getOption("tigris_year", 2022L)){
  message(paste("Retrieving census tracts for year", year))
  tracts_sf_rds <- file.path(the$CENSUS_WORKDIR, sprintf("tracts_%d.rds", year)); print(file.info(tracts_sf_rds))
  if(file.exists(  tracts_sf_rds)) {
    tracts_sf <- readRDS(tracts_sf_rds)
  } else {
    # (tracts_dsn <- list.files(path = Sys.getenv('TIGRIS_CACHE_DIR')
    #                           ,pattern = "tl_2023_\\d{2}_tract$*", full.names = TRUE))
    #
    #
    # tracts_sf.lst <- lapply(tracts_dsn, st_read)
    #
    # tracts_sf <- do.call("rbind", tracts_sf.lst)
    ?tracts
    tracts_sf <- tracts(cb=TRUE,year=year,keep_zipped_shapefile = TRUE) %>%
      st_transform( st_crs(3857))

    # add coastline -----------------------------------------------------------
    # ?tigris::coastline
    (coastline_sf <- tigris::coastline(keep_zipped_shapefile =TRUE) %>% st_transform(st_crs(3857))) # %>% subset(NAME!="Arctic")
    coastline_lst <- split(coastline_sf, f = coastline_sf$NAME)

    tmp_df <- lapply(coastline_lst, . %>% st_intersects(x=tracts_sf,y=.) %>% (function(x) lengths(x)>0)) %>% as.data.frame()
    print(summary(tmp_df))
    (tracts_sf <- cbind(tracts_sf, tmp_df))

    saveRDS(tracts_sf,   tracts_sf_rds); print(file.info(tracts_sf_rds))
  }; str(tracts_sf)
  tracts_sf
}



#' NRI Hazard Info by Census Tabulation Block
#'
#' @param statefips character
#'
#' @return simple feature
#' @import glue
#' @import RSQLite
#' @export
#'
get_NRI_tracts_sf <- function(statefips) {

  NRI_GDB_tracts_gdb <- file.path(the$NRI_DATADIR, "NRI_GDB_CensusTracts.gdb"); stopifnot(dir.exists(NRI_GDB_tracts_gdb))
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  query <- "select STATEABBRV,STATEFIPS,COUNTY,COUNTYTYPE,COUNTYFIPS,STCOFIPS
  ,TRACT,TRACTFIPS
POPULATION,BUILDVALUE,AREA,RISK_VALUE
      ,RISK_SCORE
    ,EAL_SCORE,EAL_RATNG,EAL_VALB
    ,ALR_VALB
    ,SOVI_SCORE,SOVI_RATNG,SOVI_SPCTL,RESL_SCORE,RESL_RATNG,RESL_SPCTL,RESL_VALUE,CRF_VALUE
    ,AVLN_ALRB,CFLD_ALRB,CWAV_ALRB,ERQK_ALRB,HAIL_ALRB,HWAV_ALRB,HRCN_ALRB,ISTM_ALRB,LNDS_ALRB,LTNG_ALRB,RFLD_ALRB,SWND_ALRB,TRND_ALRB,TSUN_ALRB,VLCN_ALRB,WFIR_ALRB,WNTW_ALRB
  FROM NRI_CensusTracts"

  query <- "select * FROM NRI_CensusTracts"

  if(!missing(statefips)) {
    # stopifnot(nchar(statefips)==2L)


    # statefips <- get_fips_code(stateabbr)
    message(sprintf("Getting NRI tracts for state fips %s",paste(statefips, collapse = ",")))


    query <- paste(query,glue::glue_sql("where STATEFIPS in ({fips*})", fips=statefips, .con=conn)); cat(query,"\n")

  }
  # ?sf::st_read
  NRI_tracts_sf <- sf::st_read(NRI_GDB_tracts_gdb
                               # , layer = "NRI_CensusTracts"
                               , query = query)

  # print(NRI_tracts)

  # saveRDS(NRI_tracts, NRI_tracts_Rds); print(file.info(NRI_tracts_Rds))


  # add coastline -----------------------------------------------------------
  # coastline_sf <- tigris::coastline(keep_zipped_shapefile =TRUE) %>% st_transform(st_crs(3857)) # %>% subset(NAME!="Arctic")
  # coastline_lst <- split(coastline_sf, f = coastline_sf$NAME)
  #
  # tmp_df <- lapply(coastline_lst, . %>% st_intersects(x=NRI_tracts,y=.) %>% (function(x) lengths(x)>0)) %>% as.data.frame()
  # print(summary(tmp_df))
  # (NRI_tracts <- cbind(NRI_tracts, tmp_df))
  #
  # # ?sf::st_is_valid.sf
  # print(table(polygon_ok <- sf::st_is_valid(NRI_tracts), useNA = "ifany"))
  # print(NRI_tracts[!polygon_ok ,  'STCOFIPS'])
  # if(!all(polygon_ok)){
  #
  #   NRI_tracts <- NRI_tracts %>% sf::st_make_valid()
  # }
str(NRI_tracts_sf)
  dbDisconnect(conn)
  return(NRI_tracts_sf)
}

#' Get NRI Hazards table by Census Tract
#'
#' @return a data.table
#' @import forcats
#' @export
#'
get_NRI_tracts_dt <- function() {
  # browser()
  NRI_tracts_dt_rds <- file.path(the$NRI_WORKDIR, "NRI_tracts_dt.rds"); print(file.info(NRI_tracts_dt_rds))
  NRI_tracts_fst <- file.path(the$NRI_WORKDIR, "NRI_tracts.fst"); print(file.info(NRI_tracts_fst))
  if(file.exists(NRI_tracts_dt_rds)) {

    NRI_tracts_dt <- readRDS(NRI_tracts_dt_rds)
  } else if(file.exists(NRI_tracts_fst)) {
    print(fst.metadata(NRI_tracts_fst))
    NRI_tracts_dt <- read_fst(NRI_tracts_fst, as.data.table = TRUE)
  } else {
    # list.files(the$NRI_DATADIR)
    browseURL(the$NRI_DATADIR)
    NRI_tracts_dt <- fread(file.path(the$NRI_DATADIR, "NRI_Table_CensusTracts.csv")
                           , colClasses = list(character=c('STATEFIPS','COUNTYFIPS','STCOFIPS','TRACT','TRACTFIPS')))
    str(NRI_tracts_dt)
    # NRI_tracts_dt[, STATEFIPS:=sprintf("%02d", STATEFIPS)]
    # str(NRI_tracts_dt$STATEFIPS)
    # print(range(NRI_tracts_dt$STCOFIPS))
    # NRI_tracts_dt[, STCOFIPS:=sprintf("%05d", STCOFIPS)]
    # str(NRI_tracts_dt$STCOFIPS)
    # NRI_tracts <- get_NRI_tracts()
    # NRI_tracts <- data.table(st_drop_geometry(NRI_tracts), stringsAsFactors = TRUE)
    area_cols <- grep("AREA$", names(NRI_tracts_dt ), value=TRUE) # in sq miles
    print(area_cols)
    # NRI_tracts_dt[, AREA:=set_units(AREA, "mile^2")]
    # str(NRI_tracts_dt$AREA)
    NRI_tracts_dt[, (area_cols):=lapply(.SD, set_units,  "mile^2"), .SDcols = area_cols]
    str(NRI_tracts_dt[,   .SD,.SDcols = area_cols])
    # ?fct_relevel

    NRI_tracts_dt <- NRI_add_categoricals(NRI_tracts_dt)
    write_fst(NRI_tracts_dt, path = NRI_tracts_fst);   print(file.info(NRI_tracts_fst))
    saveRDS(NRI_tracts_dt, NRI_tracts_dt_rds);   print(file.info(NRI_tracts_dt_rds))
  }# ; str(NRI_tracts_dt)
  print(summary(NRI_tracts_dt))
  return(NRI_tracts_dt)
}


