

#' @import tigris
#' @import stringi
validate_state<- function (state, verbose = interactive())
{
  if (is.null(state))
    return(NULL)
  str(tigris::fips_codes)
  # state <- tolower(stringi::stri_trim(state))
  # browser()
  state <- toupper(trimws(state))
  if (grepl("^[[:digit:]]+$", state)) {
    state <- sprintf("%02d", as.numeric(state))
    if (state %in% unique(tigris::fips_codes$state_code)) {
      return(state)
    }
    else {
      state_sub <- substr(state, 1, 2)
      if (state_sub %in% unique(tigris::fips_codes$state_code)) {
        message(sprintf("Using first two digits of %s - '%s' (%s) - for FIPS code.",
                        state, state_sub, unique(tigris::fips_codes[tigris::fips_codes$state_code ==state_sub, "state_name"]) ), call. = FALSE)
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
    if (nchar(state) == 2 && state %in% unique(tigris::fips_codes$state)) {
      if (verbose)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        unique(tigris::fips_codes[tigris::fips_codes$state == state, "state_code"]), toupper(state)))
      return(unique(tigris::fips_codes[tigris::fips_codes$state == state,"state_code"]))
    }
    else if (nchar(state) > 2 & state %in% tigris::fips_codes$state_name) {
      if (verbose)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        unique(tigris::fips_codes[tigris::fips_codes$state_name == state,
                                           "state_code"]), stringi::stri_trans_totitle(state)))
      return(unique(tigris::fips_codes[tigris::fips_codes$state_name ==
                                  state, "state_code"]))
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
  state <- validate_state(state, verbose = FALSE)
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

#' get states
#'
#'
#' Tigris shapefile for states
#' in WGS 84 / Pseudo-Mercator
#'
#' @param year integer
#'
#' @return simple feature
#' @export
#'

get_states_sf <- function(year=getOption("tigris_year",2020L)){
  # states_dsn <- file.path(CENSUS_WORKDIR, sprintf("states_%d.shp", year)); print(file.info(states_dsn))
  states_gpkg <- file.path(CENSUS_WORKDIR, sprintf("states_%d.gpkg", year)); print(file.info(states_gpkg)['size'])
  states_sf_rds <- file.path(CENSUS_WORKDIR, sprintf("states_%d.rds", year)); print(file.info(states_sf_rds)['size'])
  if(file.exists(states_sf_rds)) {
    states_sf <- readRDS(states_sf_rds)
  } else if(file.exists(states_gpkg)) {
    states_sf <- st_read(states_gpkg)
  } else {
    # print(year)
    # ?states
    (states_sf <- tigris::states(cb=TRUE
                                 , year=year
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

    states_sf <- st_transform(states_sf, st_crs(4269)) #
    states_sf
    # show(states_sf)
    # ?which
    #
    # imax <- which.max(states_sf$INTPTLON==max(states_sf$INTPTLON)); print(states_sf[imax,])
    #
    #
    # imin <- which.max(states_sf$INTPTLON==min(states_sf$INTPTLON)); print(states_sf[imin,])
    # ?st_write.sf
    # debugonce(st_write)
    # st_write(states_sf, dsn=states_dsn, append = FALSE)
    st_write(states_sf, dsn=states_gpkg, append = FALSE)
    saveRDS(states_sf, states_sf_rds);print(file.info(states_sf_rds))

  }# ; str(states_sf)
  return(states_sf)
}

#' @import terra
#' @export
states_vect <- purrr::compose(terra::vect, tigris::states)

NRI_states_info <- function() {
  NRI_states_fst <- file.path(NRI_WORKDIR, "NRI_states.fst"); print(file.info(NRI_states_fst))
  dput(names(fst(NRI_states_fst)))
  print(fst.metadata(NRI_states_fst))
}

#' Get NRI Hazards table by State
#'
#' @return a data.table
#' @export
#'
get_NRI_states_dt <- function() {
  # browser()
  NRI_states_dt_rds <- file.path(NRI_WORKDIR, "NRI_states_dt.rds"); print(file.info(NRI_states_dt_rds))
  NRI_states_fst <- file.path(NRI_WORKDIR, "NRI_states.fst"); print(file.info(NRI_states_fst))
  if(file.exists(NRI_states_dt_rds)) {

    NRI_states_dt <- readRDS(NRI_states_dt_rds)
  } else if(file.exists(NRI_states_fst)) {
    print(fst.metadata(NRI_states_fst))
    NRI_states_dt <- read_fst(NRI_states_fst, as.data.table = TRUE)
  } else {
    #  list.files(.NRI_datadir)
    NRI_states_dt <- fread(file.path(.NRI_datadir, "NRI_Table_States.csv")
                           , na.strings = c("Data Unavailable","Insufficient Data","Not Applicable")
                           , colClasses = list(character='STATEFIPS'
                                               ,factor=c("AVLN_EALR", "CFLD_EALR", "CWAV_EALR", "DRGT_EALR", "ERQK_EALR",
                                                         "HAIL_EALR", "HWAV_EALR", "HRCN_EALR", "ISTM_EALR", "LNDS_EALR",
                                                         "LTNG_EALR", "RFLD_EALR", "SWND_EALR", "TRND_EALR", "TSUN_EALR",
                                                         "VLCN_EALR", "WFIR_EALR", "WNTW_EALR")))
    NRI_states_dt[, OID_:=NULL]
rating_cols <-  grep("EALR$", names(NRI_states_dt ), value=TRUE); print(rating_cols)
NRI_states_dt[, (rating_cols):=lapply(.SD, as.factor), .SDcols = rating_cols]
str(NRI_states_dt[, .SD, .SDcols = rating_cols])

area_cols <- grep("AREA$", names(NRI_states_dt ), value=TRUE) # in sq miles

    write_fst(NRI_states_dt, path = NRI_states_fst);   print(file.info(NRI_states_fst))
    saveRDS(NRI_states_dt, NRI_states_dt_rds)
  }; str(NRI_states_dt)

  return(NRI_states_dt)
}


# NRI_states_sf ------------------------------------------------------------------
get_NRI_states_sf <- function(){
  NRI_states_sf_rds <- file.path(NRI_WORKDIR, "NRI_states_sf.rds"); print(file.info(NRI_states_sf_rds))
  if(file.exists(NRI_states_sf_rds)) {
    NRI_states_sf <- readRDS(NRI_states_sf_rds)
  } else {
    NRI_GDB_states_gdb <- file.path(.NRI_datadir, "NRI_GDB_States.gdb"); stopifnot(dir.exists(NRI_GDB_states_gdb))
    print(st_layers(NRI_GDB_states_gdb))
    ?st_read
    NRI_states_sf <- st_read(NRI_GDB_states_gdb, layer = "NRI_States")
    print(st_crs(NRI_states_sf))

    (coastline_sf <- tigris::coastline(keep_zipped_shapefile =TRUE) %>% st_transform(st_crs(3857)) # %>% subset(NAME!="Arctic")
    )
    print(table(coastline_sf$NAME))

    # (coastline_3857_sf <- st_transform(coastline_sf, st_crs(3857)))

    (NRI_states.coastline_lst <- st_intersects(NRI_states_sf, coastline_sf))

    NRI_states_sf$IS_COASTAL <- lengths(NRI_states.coastline_lst)>0
    print(table(NRI_states_sf$IS_COASTAL, useNA = "ifany"))

    saveRDS(NRI_states_sf, NRI_states_sf_rds)
  }
  NRI_states_sf
}

