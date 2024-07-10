#' get Climate Zones
#'
#' from https://basc.pnnl.gov/guide-determining-climate-zone-county-data-files
#'
#' @import sf
get_ClimateZones_sf <- function(){
  # https://basc.pnnl.gov/sites/default/files/ClimateZoneDataFiles.zip
  # https://www.osti.gov/biblio/1893981/

  ClimateZones_sf_rds <- file.path(.DOE_workdir, "ClimateZones_sf.rds"); print(file.info(ClimateZones_sf_rds))
  if(file.exists(  ClimateZones_sf_rds)) {
    ClimateZones_sf <- readRDS(ClimateZones_sf_rds)
  } else {

    (ClimateZones_sf <- st_read("E:\\Datasets\\DOE\\ClimateZoneDataFiles\\ClimateZones.shp"))
    table(ClimateZones_sf$BA21, useNA = "ifany")
    nrow(ClimateZones_sf)
    names(ClimateZones_sf)[which(names(ClimateZones_sf)=="Country")] <- "County"
    print(names(ClimateZones_sf))
    ClimateZones_sf$BA21<-factor(ClimateZones_sf$BA21, levels = c( 'Subarctic'
      ,'Very Cold','Cold'
                                                                  , 'Marine'
                                                                  ,'Mixed-Humid','Mixed-Dry'
                                                                  ,'Hot-Humid','Hot-Dry'))
    print(levels(ClimateZones_sf$BA21))
    ClimateZones_sf$GEOID <- sub(pattern = "^G",replacement = "", ClimateZones_sf$GEOID , fixed = FALSE,perl = TRUE)
    ClimateZones_sf %>% subset(GEOID %in% c("06037")) %>% print() # Los Angeles County
    attr(ClimateZones_sf, "path") <- ClimateZones_sf_rds
    print(ClimateZones_sf)
    saveRDS(ClimateZones_sf,ClimateZones_sf_rds); print(file.info(ClimateZones_sf_rds))
  }; str(ClimateZones_sf)

  return(ClimateZones_sf)
}

get_ClimateZones_dt <- function(){
  ClimateZones_fst <- file.path(.DOE_workdir, "ClimateZones.fst"); print(file.info(ClimateZones_fst))
  if(file.exists(ClimateZones_fst)) {
    print(fst.metadata(ClimateZones_fst))
    ClimateZones <- read_dt(ClimateZones_fst)
  } else {
    ClimateZones_sf <- get_ClimateZones_sf()
    ClimateZones <- ClimateZones_sf %>% st_drop_geometry() %>% setDT(key='GEOID')
    print(levels(ClimateZones$BA21))
     write_fst(ClimateZones, ClimateZones_fst); print(file.info(ClimateZones_fst))
  }; str(ClimateZones)
  ClimateZones
}

