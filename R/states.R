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

# us states ----
# ?states
#
# (states_sf <- tigris::states(cb=TRUE))
# st_crs(states_sf)

# options(tigris_year=2022L); getOption("tigris_year")

get_states_sf <- function(){
  # states_dsn <- file.path(.census_workdir, sprintf("states_%d.shp", getOption("tigris_year"))); print(file.info(states_dsn))
  states_gpkg <- file.path(.census_workdir, sprintf("states_%d.gpkg", getOption("tigris_year"))); print(file.info(states_gpkg))
  states_sf_rds <- file.path(.census_workdir, sprintf("states_%d.rds", getOption("tigris_year"))); print(file.info(states_sf_rds))
  if(file.exists(states_sf_rds)) {
    states_sf <- readRDS(states_sf_rds)
  } else if(file.exists(states_gpkg)) {
    states_sf <- st_read(states_gpkg)
  } else {
    # print(getOption("tigris_year"))
    # ?states
    (states_sf <- tigris::states(cb=TRUE
                                 , year=getOption("tigris_year")
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

    # show(states_sf)
    # ?which
    #
    # imax <- which.max(states_sf$INTPTLON==max(states_sf$INTPTLON)); print(states_sf[imax,])
    #
    #
    # imin <- which.max(states_sf$INTPTLON==min(states_sf$INTPTLON)); print(states_sf[imin,])
    # ?st_write.sf
    debugonce(st_write)
    # st_write(states_sf, dsn=states_dsn, append = FALSE)
    st_write(states_sf, dsn=states_gpkg, append = FALSE)
    saveRDS(states_sf, states_sf_rds);print(file.info(states_sf_rds))
  }; states_sf
}

# NRI_states_sf ------------------------------------------------------------------
get_NRI_states_sf <- function(){
  NRI_states_sf_rds <- file.path(.NRI_workdir, "NRI_states_sf.rds"); print(file.info(NRI_states_sf_rds))
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

