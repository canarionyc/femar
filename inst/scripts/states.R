# us states ----
?states

(states_sf <- tigris::states(cb=TRUE))
st_crs(states_sf)

# conus ------------------------------------------------------------------
# options(tigris_year=2022L); getOption("tigris_year")

get_conus_sf <- function(){
  # conus_dsn <- file.path(census_dir, sprintf("conus_%d.shp", getOption("tigris_year"))); print(file.info(conus_dsn))
  conus_gpkg <- file.path(census_dir, sprintf("conus_%d.gpkg", getOption("tigris_year"))); print(file.info(conus_gpkg))
  conus_rds <- file.path(census_dir, sprintf("conus_%d.rds", getOption("tigris_year"))); print(file.info(conus_rds))
  if(file.exists(conus_rds)) {
    conus_sf <- readRDS(conus_sf_rds)
  }
  if(file.exists(conus_gpkg)) {
    conus_sf <- st_read(conus_gpkg)
  } else {
    print(getOption("tigris_year"))
    ?states
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

    conus_sf <- states_sf%>% subset(STATEFP!="02" & STATEFP !="15" & STATEFP<60)

    print(length(conus_sf$STUSPS))



    # show(states_sf)
    # ?which
    #
    # imax <- which.max(conus_sf$INTPTLON==max(conus_sf$INTPTLON)); print(conus_sf[imax,])
    #
    #
    # imin <- which.max(conus_sf$INTPTLON==min(conus_sf$INTPTLON)); print(conus_sf[imin,])
    # ?st_write.sf
    debugonce(st_write)
    # st_write(conus_sf, dsn=conus_dsn, append = FALSE)
    st_write(conus_sf, dsn=conus_gpkg, append = FALSE)
    saveRDS(conus_sf, conus_sf_rds)
  }; conus_sf
}
