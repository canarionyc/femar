#' @title gt census tracts
#
#' Because blocks nest in tracts, and tract IDs even form the first part of block
#' IDs, it's relatively straightforward to generate tract crosswalks from block
#' crosswalks.
#' @param year int tigris year
#' @return simple feature
#' @export
get_tracts_sf <- function(year=getOption("tigris_year", 2020L)){
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
    (tracts_sf <- tracts(cb=TRUE,year=year,keep_zipped_shapefile = TRUE) %>% st_transform( st_crs(3857)))


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

#' @import terra
#' @export
tracts_vect <- purrr::compose(terra::vect, tigris::tracts)

#' NRI Hazard Info by Census Tabulation Block
#'
#' @param state character
#'
#' @return sf simple feature
#' @import sf
#' @export
#'
get_NRI_tracts_sf <- function(state) {

  # browseURL(.NRI_datadir)
  if(missing(state)) {
    NRI_tracts_sf_Rds <- file.path(the$NRI_WORKDIR, "NRI_tracts_sf.Rds"); print(file.info(NRI_tracts_sf_Rds))

    if(file.exists(NRI_tracts_sf_Rds)) {
      NRI_tracts_sf<- readRDS(NRI_tracts_sf_Rds)

    } else {
      NRI_GDB_tracts_gdb <- file.path(.NRI_datadir, "NRI_GDB_CensusTracts.gdb"); stopifnot(dir.exists(NRI_GDB_tracts_gdb))
      # print(st_layers(NRI_GDB_tracts_gdb))

      # ?st_read
      # debugonce(st_read)

      NRI_tracts_sf <- sf::st_read(dsn = NRI_GDB_tracts_gdb
                                   , layer = "NRI_CensusTracts"
                                   ,quiet=TRUE)
      saveRDS(NRI_tracts_sf, NRI_tracts_sf_Rds)
    }

  } else{
    # browser()
    state <- toupper(state)
    statefips <- get_fips_code(state)
    message(sprintf("Getting NRI tracts for state %s fips %s",state,statefips))
    NRI_GDB_tracts_gdb <- file.path(.NRI_datadir, "NRI_GDB_CensusTracts.gdb"); stopifnot(dir.exists(NRI_GDB_tracts_gdb))
    NRI_tracts_sf <- sf::st_read(dsn = NRI_GDB_tracts_gdb
                                 , layer = "NRI_CensusTracts"
                                 , query = sprintf("SELECT * FROM NRI_CensusTracts where STATEFIPS  = '%s'", statefips))




  }
  NRI_tracts_sf$RISK_RATNG<- factor(NRI_tracts_sf$RISK_RATNG
                                    , levels = c("Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"))
  # NRI_tracts_sf$RISK_RATNG <- fct_na_level_to_value(NRI_tracts_sf$RISK_RATNG,extra_levels  = "Insufficient Data" )

  print(table(NRI_tracts_sf$RISK_RATNG, useNA = "ifany"))

  # NRI_tracts_sf$RISK_RATNG2 <- forcats::fct_collapse(NRI_tracts_sf$RISK_RATNG
  #                                                    , 'High'=c('Very High','Relatively High')
  #                                                    , 'Moderate'="Relatively Moderate"
  #                                                    , "Low"=c("Relatively Low","Very Low"))

  NRI_tracts_sf$SOVI_RATNG <- factor(NRI_tracts_sf$SOVI_RATNG
                                     , levels = c("Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low" ))
  print(levels(NRI_tracts_sf$SOVI_RATNG))
  # NRI_tracts_sf$SOVI_RATNG <- fct_na_level_to_value(NRI_tracts_sf$SOVI_RATNG,extra_levels  = "Data Unavailable" )

  print(table(NRI_tracts_sf$SOVI_RATNG, useNA = "ifany"))

  # add coastline -----------------------------------------------------------
  coastline_sf <- tigris::coastline(keep_zipped_shapefile =TRUE) %>% st_transform(st_crs(3857)) # %>% subset(NAME!="Arctic")
  coastline_lst <- split(coastline_sf, f = coastline_sf$NAME)

  tmp_df <- lapply(coastline_lst, . %>% st_intersects(x=NRI_tracts_sf,y=.) %>% (function(x) lengths(x)>0)) %>% as.data.frame()
  print(summary(tmp_df))
  (NRI_tracts_sf <- cbind(NRI_tracts_sf, tmp_df))

  # ?sf::st_is_valid.sf
  print(table(polygon_ok <- sf::st_is_valid(NRI_tracts_sf), useNA = "ifany"))
  print(NRI_tracts_sf[!polygon_ok ,  'STCOFIPS'])
  if(!all(polygon_ok)){

    NRI_tracts_sf <- NRI_tracts_sf %>% sf::st_make_valid()
  }


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
    # list.files(.NRI_datadir)
    browseURL(.NRI_datadir)
    NRI_tracts_dt <- fread(file.path(.NRI_datadir, "NRI_Table_CensusTracts.csv")
                           , colClasses = list(character=c('STATEFIPS','COUNTYFIPS','STCOFIPS','TRACT','TRACTFIPS')))
    str(NRI_tracts_dt)
    # NRI_tracts_dt[, STATEFIPS:=sprintf("%02d", STATEFIPS)]
    # str(NRI_tracts_dt$STATEFIPS)
    # print(range(NRI_tracts_dt$STCOFIPS))
    # NRI_tracts_dt[, STCOFIPS:=sprintf("%05d", STCOFIPS)]
    # str(NRI_tracts_dt$STCOFIPS)
    # NRI_tracts_sf <- get_NRI_tracts_sf()
    # NRI_tracts <- data.table(st_drop_geometry(NRI_tracts_sf), stringsAsFactors = TRUE)
    area_cols <- grep("AREA$", names(NRI_tracts_dt ), value=TRUE) # in sq miles
    print(area_cols)
    # NRI_tracts_dt[, AREA:=set_units(AREA, "mile^2")]
    # str(NRI_tracts_dt$AREA)
    NRI_tracts_dt[, (area_cols):=lapply(.SD, set_units,  "mile^2"), .SDcols = area_cols]
    str(NRI_tracts_dt[,   .SD,.SDcols = area_cols])
    # ?fct_relevel

    NRI_tracts_dt[, RISK_RATNG:=factor(RISK_RATNG, levels = c("Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"
                                                              # ,"Insufficient Data"
    ))]
    print(table(NRI_tracts_dt$RISK_RATNG, useNA = "ifany"))

    NRI_tracts_dt[, SOVI_RATNG:=factor(SOVI_RATNG, levels = c( "Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"
                                                               #    ,"Data Unavailable"
    ))]
    print(levels(NRI_tracts_dt$SOVI_RATNG))
    print(table(NRI_tracts_dt$SOVI_RATNG, useNA = "ifany"))
    write_fst(NRI_tracts_dt, path = NRI_tracts_fst);   print(file.info(NRI_tracts_fst))
    saveRDS(NRI_tracts_dt, NRI_tracts_dt_rds);   print(file.info(NRI_tracts_dt_rds))
  }# ; str(NRI_tracts_dt)
  print(summary(NRI_tracts_dt))
  return(NRI_tracts_dt)
}


