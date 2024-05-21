
#' @title get_NRI_HazardInfo
#' Get the main hazrds table from FEMA/NRI
#' @import data.table fst sf
#' @importFrom utils  str
#' @export

get_NRI_HazardInfo <- function(){

  NRI_HazardInfo_fst <- file.path(.NRI_workdir , "NRI_HazardInfo.fst"); print(file.info(NRI_HazardInfo_fst))
  if(file.exists(NRI_HazardInfo_fst)) {
    print(fst.metadata(NRI_HazardInfo_fst))
    NRI_HazardInfo  <- read_fst(NRI_HazardInfo_fst, as.data.table = TRUE)
  } else {

    stopifnot(dir.exists(.NRI_datadir))
    NRI_GDB_states_gdb <- file.path(.NRI_datadir, "NRI_GDB_States.gdb"); stopifnot(dir.exists(NRI_GDB_states_gdb))

    suppressWarnings(NRI_HazardInfo <- sf::st_read(NRI_GDB_states_gdb, layer = "NRI_HazardInfo"))
    fst::write_fst(NRI_HazardInfo, path = NRI_HazardInfo_fst);  print(file.info(NRI_HazardInfo_fst))
  }; utils::str(NRI_HazardInfo )

  return(NRI_HazardInfo)
}



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
    return(readRDS(NRI_ctys_sf_Rds))
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
                                                                       ,"Insufficient Data"))
    NRI_ctys_sf$RISK_RATNG <- fct_na_level_to_value(NRI_ctys_sf$RISK_RATNG,extra_levels  = "Insufficient Data" )

    print(table(NRI_ctys_sf$RISK_RATNG))

    NRI_ctys_sf$SOVI_RATNG <- factor(NRI_ctys_sf$SOVI_RATNG, levels = c( "Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low" ,"Data Unavailable"))
    print(levels(NRI_ctys_sf$SOVI_RATNG))
    NRI_ctys_sf$SOVI_RATNG <- fct_na_level_to_value(NRI_ctys_sf$SOVI_RATNG,extra_levels  = "Data Unavailable" )

    print(table(NRI_ctys_sf$SOVI_RATNG))


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

NRI_states_info <- function() {
  NRI_states_fst <- file.path(.NRI_workdir, "NRI_states.fst"); print(file.info(NRI_states_fst))
  dput(names(fst(NRI_states_fst)))
  print(fst.metadata(NRI_states_fst))

}

#' Get NRI Hazards table by State
#'
#' @param select_cols character
#' @return a data.table
#' @export
#'
get_NRI_states_dt <- function(select_cols=NULL) {
  # browser()
  NRI_states_dt_rds <- file.path(.NRI_workdir, "NRI_states_dt.rds"); print(file.info(NRI_states_dt_rds))
  NRI_states_fst <- file.path(.NRI_workdir, "NRI_states.fst"); print(file.info(NRI_states_fst))
  if(file.exists(NRI_states_dt_rds)) {

    NRI_states_dt <- readRDS(NRI_states_dt_rds)
  } else if(file.exists(NRI_states_fst)) {
    print(fst.metadata(NRI_states_fst))
    NRI_states_dt <- read_fst(NRI_states_fst, as.data.table = TRUE, columns = select_cols)
  } else {
    #  list.files(.NRI_datadir)
    NRI_states_dt <- fread(file.path(.NRI_datadir, "NRI_Table_States.csv"))
    # NRI_states_sf <- get_NRI_states_sf()
    # NRI_states <- data.table(st_drop_geometry(NRI_states_sf), stringsAsFactors = TRUE)
    # area_cols <- grep("AREA$", names(NRI_states_dt ), value=TRUE) # in sq miles
    write_fst(NRI_states_dt, path = NRI_states_fst);   print(file.info(NRI_states_fst))
    saveRDS(NRI_states_dt, NRI_states_dt_rds)
  }; str(NRI_states_dt)

  return(subset(NRI_states_dt, select=select_cols))
}


#' Get NRI Hazards table by County
#'
#' @return a data.table
#' @import forcats
#' @export
#'
get_NRI_ctys_dt <- function() {
  # browser()
  NRI_ctys_rds <- file.path(.NRI_workdir, "NRI_ctys.rds"); print(file.info(NRI_ctys_rds))
  NRI_ctys_fst <- file.path(.NRI_workdir, "NRI_ctys.fst"); print(file.info(NRI_ctys_fst))
  if(file.exists(NRI_ctys_rds)) {

    NRI_ctys_dt <- readRDS(NRI_ctys_rds)
  } else if(file.exists(NRI_ctys_fst)) {
    print(fst.metadata(NRI_ctys_fst))
    NRI_ctys_dt <- read_fst(NRI_ctys_fst, as.data.table = TRUE)
  } else {
    # list.files(.NRI_datadir)
    NRI_ctys_dt <- fread(file.path(.NRI_datadir, "NRI_Table_Counties.csv"))
    NRI_ctys_dt[, STATEFIPS:=sprintf("%02d", STATEFIPS)]
    str(NRI_ctys_dt$STATEFIPS)
    print(range(NRI_ctys_dt$STCOFIPS))
    NRI_ctys_dt[, STCOFIPS:=sprintf("%05d", STCOFIPS)]
    str(NRI_ctys_dt$STCOFIPS)
    # NRI_ctys_sf <- get_NRI_ctys_sf()
    # NRI_ctys <- data.table(st_drop_geometry(NRI_ctys_sf), stringsAsFactors = TRUE)
    area_cols <- grep("AREA$", names(NRI_ctys_dt ), value=TRUE) # in sq miles
    print(area_cols)
    # NRI_ctys_dt[, AREA:=set_units(AREA, "mile^2")]
    # str(NRI_ctys_dt$AREA)
    NRI_ctys_dt[, (area_cols):=lapply(.SD, set_units,  "mile^2"), .SDcols = area_cols]
    str(NRI_ctys_dt[,  .SD,.SDcols = area_cols])
    # ?fct_relevel

    NRI_ctys_dt[, RISK_RATNG:=factor(RISK_RATNG, levels = c("Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"
                                                           # ,"Insufficient Data"
                                                            ))]
    print(table(NRI_ctys_dt$RISK_RATNG, useNA = "ifany"))

    NRI_ctys_dt[, SOVI_RATNG:=factor(SOVI_RATNG, levels = c( "Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"
                                                         #    ,"Data Unavailable"
                                                             ))]
    print(levels(NRI_ctys_dt$SOVI_RATNG))
    print(table(NRI_ctys_dt$SOVI_RATNG, useNA = "ifany"))
    write_fst(NRI_ctys_dt, path = NRI_ctys_fst);   print(file.info(NRI_ctys_fst))
    saveRDS(NRI_ctys_dt, NRI_ctys_rds);   print(file.info(NRI_ctys_rds))
  }# ; str(NRI_ctys_dt)
  print(summary(NRI_ctys_dt))
  return(NRI_ctys_dt)
}

get_hrcn_cat_dt <- function(){

  hrcn_cat_rds <- file.path(.NRI_workdir, "hrcn_cat.rds"); print(file.info(hrcn_cat_rds))
  if(file.exists(  hrcn_cat_rds)) {
    hrcn_cat <- readRDS(hrcn_cat_rds)
  } else {
    hrcn_cat_xlsx <- system.file("extdata", "FEMA", "NRI", "Hurricane_Categorization.xlsx", package="femar", mustWork = TRUE)

    hrcn_cat <- readxl::read_xlsx(hrcn_cat_xlsx) %>% setDT() %>% sanitize()
    print(names(hrcn_cat))
    hrcn_cat[, c("MINIMUM_WIND_SPEED_MPH","MAXIMUM_WIND_SPEED_MPH"):=NULL]



    hrcn_cat[, AVERAGE_RADIUS_OF_HURRICANE_OR_TROPICAL_STORM_FORCE_WINDS_MILES:=set_units(AVERAGE_RADIUS_OF_HURRICANE_OR_TROPICAL_STORM_FORCE_WINDS_MILES , "mile")]
    hrcn_cat[, c("MINIMUM_WIND_SPEED_KTS","MAXIMUM_WIND_SPEED_KTS"):=lapply(.SD, set_units, "kts")
             , .SDcols = c("MINIMUM_WIND_SPEED_KTS","MAXIMUM_WIND_SPEED_KTS")]

    setnames(hrcn_cat, c('STORM_CATEGORY'
                         , 'MINIMUM_WIND_SPEED'
                         ,'MAXIMUM_WIND_SPEED'
                         ,'AVERAGE_RADIUS_OF_HURRICANE_OR_TROPICAL_STORM_FORCE_WINDS'))
    hrcn_cat[, USA_SSHS:=seq.int(-1,5)]
    # hrcn_cat
    saveRDS(hrcn_cat,   hrcn_cat_rds); print(file.info(hrcn_cat_rds))
  }; str(hrcn_cat)
  return(invisible(hrcn_cat))
}

#' NRI Hazard Info by Census Tabulation Block
#'
#' @param state character
#'
#' @return sf simple feature
#' @import sf
#' @export
#'
get_NRI_tracts_sf <- function(state) {
  # browser()
  # browseURL(.NRI_datadir)
  NRI_tracts_sf_Rds <- file.path(.NRI_workdir, "NRI_tracts_sf.Rds"); print(file.info(NRI_tracts_sf_Rds))

  if(file.exists(NRI_tracts_sf_Rds)) {
    return(readRDS(NRI_tracts_sf_Rds))
  } else {
    NRI_GDB_tracts_gdb <- file.path(.NRI_datadir, "NRI_GDB_CensusTracts.gdb"); stopifnot(dir.exists(NRI_GDB_tracts_gdb))
    # print(st_layers(NRI_GDB_tracts_gdb))

    # ?st_read
    # debugonce(st_read)

    if(missing(state)) {
      NRI_tracts_sf <- sf::st_read(dsn = NRI_GDB_tracts_gdb
                                 , layer = "NRI_CensusTracts"
                                 ,quiet=TRUE)
    } else{
      statefips <- get_fips_code(state)
      str(tigris::fips_codes)
      NRI_tracts_sf <- sf::st_read(dsn = NRI_GDB_tracts_gdb
                                 , layer = "NRI_CensusTracts"
                                 , query = sprintf("SELECT * FROM NRI_CensusTracts where STATEFIPS  = '%d'", statefips))

    }

    NRI_tracts_sf$RISK_RATNG<- factor(NRI_tracts_sf$RISK_RATNG, levels = c("Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"
                                                                       ))
    # NRI_tracts_sf$RISK_RATNG <- fct_na_level_to_value(NRI_tracts_sf$RISK_RATNG,extra_levels  = "Insufficient Data" )

    print(table(NRI_tracts_sf$RISK_RATNG, useNA = "ifany"))

    NRI_tracts_sf$SOVI_RATNG <- factor(NRI_tracts_sf$SOVI_RATNG, levels = c( "Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low" ))
    print(levels(NRI_tracts_sf$SOVI_RATNG))
    # NRI_tracts_sf$SOVI_RATNG <- fct_na_level_to_value(NRI_tracts_sf$SOVI_RATNG,extra_levels  = "Data Unavailable" )

    print(table(NRI_tracts_sf$SOVI_RATNG))


    # ?sf::st_is_valid.sf
    print(table(polygon_ok <- sf::st_is_valid(NRI_tracts_sf), useNA = "ifany"))
    print(NRI_tracts_sf[!polygon_ok ,  'STCOFIPS'])
    if(!all(polygon_ok)){

      NRI_tracts_sf <- NRI_tracts_sf %>% sf::st_make_valid()
    }
    saveRDS(NRI_tracts_sf, NRI_tracts_sf_Rds)
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
  NRI_tracts_dt_rds <- file.path(.NRI_workdir, "NRI_tracts_dt.rds"); print(file.info(NRI_tracts_dt_rds))
  NRI_tracts_fst <- file.path(.NRI_workdir, "NRI_tracts.fst"); print(file.info(NRI_tracts_fst))
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

