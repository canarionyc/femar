#' @import tigris
#' @import stringi
validate_state<- function (state, .msg = interactive())
{
  if (is.null(state))
    return(NULL)
  # state <- tolower(stringi::stri_trim(state))
  state <- tolower(trimws(state))
  if (grepl("^[[:digit:]]+$", state)) {
    state <- sprintf("%02d", as.numeric(state))
    if (state %in% tigris::fips_codes$state_code) {
      return(state)
    }
    else {
      state_sub <- substr(state, 1, 2)
      if (state_sub %in% tigris::fips_codes$state_code) {
        message(sprintf("Using first two digits of %s - '%s' (%s) - for FIPS code.",
                        state, state_sub, tigris::fips_codes[tigris::fips_codes$state_code ==
                                                               state_sub, "state_name"]), call. = FALSE)
        return(state_sub)
      }
      else {
        warning(sprintf("'%s' is not a valid FIPS code or state name/abbreviation",
                        state), call. = FALSE)
        return(NULL)
      }
    }
  }
  else if (grepl("^[[:alpha:]]+", state)) {
    if (nchar(state) == 2 & state %in% tigris::fips_codes$state) {
      if (.msg)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        tigris::fips_codes[tigris::fips_codes$state == state,
                                           "state_code"], toupper(state)))
      return(tigris::fips_codes[tigris::fips_codes$state == state,
                                "state_code"])
    }
    else if (nchar(state) > 2 & state %in% tigris::fips_codes$state_name) {
      if (.msg)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        tigris::fips_codes[tigris::fips_codes$state_name == state,
                                           "state_code"], stringi::stri_trans_totitle(state)))
      return(tigris::fips_codes[tigris::fips_codes$state_name ==
                                  state, "state_code"])
    }
    else {
      warning(sprintf("'%s' is not a valid FIPS code or state name/abbreviation",state), call. = FALSE)
      return(NULL)
    }
  }
  else {
    warning(sprintf("'%s' is not a valid FIPS code or state name/abbreviation",state), call. = FALSE)
    return(NULL)
  }
}

#' @import tigris
#' @import utils
get_fips_code <- function (state, county = NULL){
  state <- validate_state(state, .msg = FALSE)
  if (is.null(state))
    stop("Invalid state", call. = FALSE)
  if (!is.null(county)) {
    vals <- tigris::fips_codes[tigris::fips_codes$state_code == state &
                                 grepl(sprintf("^%s",county), tigris::fips_codes$county, ignore.case = TRUE),
    ]
    message(paste0("The code for ", vals$state_name, " is '",
                   vals$state_code, "'", " and the code for ", vals$county,
                   " is '", vals$county_code, "'."))
    return(vals$county_code)
  }
  else {
    vals <- utils::head(tigris::fips_codes[tigris::fips_codes$state_code == state,1:3], 1)
    message(paste0("The code for ", vals$state_name, " is '",
                   vals$state_code, "'."))
    return(vals$state_code)
  }

}

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
  NRI_states_fst <- file.path(.NRI_workdir, "NRI_states.fst"); print(file.info(NRI_states_fst))
  if(file.exists(NRI_states_fst)) {
    print(fst.metadata(NRI_states_fst))
    NRI_states_dt <- read_fst(NRI_states_fst, as.data.table = TRUE, columns = select_cols)
  } else {
    #  list.files(.NRI_datadir)
    NRI_states_dt <- fread(file.path(.NRI_datadir, "NRI_Table_States.csv"))
    # NRI_states_sf <- get_NRI_states_sf()
    # NRI_states <- data.table(st_drop_geometry(NRI_states_sf), stringsAsFactors = TRUE)
    # area_cols <- grep("AREA$", names(NRI_states_dt ), value=TRUE) # in sq miles
    write_fst(NRI_states_dt, path = NRI_states_fst);   print(file.info(NRI_states_fst))
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
  NRI_ctys_fst <- file.path(.NRI_workdir, "NRI_ctys.fst"); print(file.info(NRI_ctys_fst))
  if(file.exists(NRI_ctys_fst)) {
    print(fst.metadata(NRI_ctys_fst))
    NRI_ctys_dt <- read_fst(NRI_ctys_fst, as.data.table = TRUE)
  } else {
    # list.files(.NRI_datadir)
    NRI_ctys_dt <- fread(file.path(.NRI_datadir, "NRI_Table_Counties.csv"))
    NRI_ctys_dt[, STATEFIPS:=sprintf("%02d", STATEFIPS)]

    # NRI_ctys_sf <- get_NRI_ctys_sf()
    # NRI_ctys <- data.table(st_drop_geometry(NRI_ctys_sf), stringsAsFactors = TRUE)
    area_cols <- grep("AREA$", names(NRI_ctys_dt ), value=TRUE) # in sq miles
    print(area_cols)

    ?fct_relevel

    NRI_ctys_dt[, RISK_RATNG:=factor(RISK_RATNG, levels = c("Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low" ,"Insufficient Data"))]
    print(table(NRI_ctys_dt$RISK_RATNG))

    NRI_ctys_dt[, SOVI_RATNG:=factor(SOVI_RATNG, levels = c( "Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low" ,"Data Unavailable"))]
    print(levels(NRI_ctys_dt$SOVI_RATNG))
    print(table(NRI_ctys_dt$SOVI_RATNG))
    write_fst(NRI_ctys_dt, path = NRI_ctys_fst);   print(file.info(NRI_ctys_fst))
  }; str(NRI_ctys_dt)
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
