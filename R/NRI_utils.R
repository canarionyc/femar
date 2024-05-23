
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

