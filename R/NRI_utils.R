
#' @title get_NRI_HazardInfo
#' Get the main hazrds table from FEMA/NRI
#' @import data.table fst sf
#' @importFrom utils  str
#' @export

get_NRI_HazardInfo <- function(){

  NRI_HazardInfo_fst <- file.path(the$NRI_WORKDIR , "NRI_HazardInfo.fst"); print(file.info(NRI_HazardInfo_fst))
  if(file.exists(NRI_HazardInfo_fst)) {
    print(fst.metadata(NRI_HazardInfo_fst))
    NRI_HazardInfo  <- read_fst(NRI_HazardInfo_fst, as.data.table = TRUE)
  } else {

    stopifnot(dir.exists(the$NRI_DATADIR))
    NRI_GDB_states_gdb <- file.path(the$NRI_DATADIR, "NRI_GDB_States.gdb"); stopifnot(dir.exists(NRI_GDB_states_gdb))
# browseURL(NRI_GDB_states_gdb)
    suppressWarnings(NRI_HazardInfo <- sf::st_read(NRI_GDB_states_gdb, layer = "NRI_HazardInfo"))
    fst::write_fst(NRI_HazardInfo, path = NRI_HazardInfo_fst);  print(file.info(NRI_HazardInfo_fst))
  }; utils::str(NRI_HazardInfo )

  return(NRI_HazardInfo)
}

get_hrcn_cat_dt <- function(){

  hrcn_cat_rds <- file.path(the$NRI_WORKDIR, "hrcn_cat.rds"); print(file.info(hrcn_cat_rds))
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

