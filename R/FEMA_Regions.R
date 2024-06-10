get_FEMA_Regions_dt <- function(.msg=interactive()){

  FEMA_Regions_dt_rds <- file.path(.fema_workdir, "FEMA_Regions_dt.rds") ; if(.msg) print(file.info(FEMA_Regions_dt_rds))
  if(file.exists(  FEMA_Regions_dt_rds)) {
    FEMA_Regions_dt <- readRDS(FEMA_Regions_dt_rds)
  } else {
    ?readxl::read_xlsx
    DataSets.xlsx <- file.path(.fema_datadir, "DataSets.xlsx")

    FEMA_Regions_dt <- readxl::read_xlsx(DataSets.xlsx,  range = "Regions!A1:B11", .name_repair=sanitize) %>% setDT()

    FEMA_Regions_dt[, STATES_SERVING:=strsplit(STATES_SERVING, ",\\s+", perl = TRUE)]
    saveRDS(FEMA_Regions_dt,   FEMA_Regions_dt_rds); if(.msg) print(file.info(FEMA_Regions_dt_rds))
  }# ; str(FEMA_Regions_dt)

  # str(FEMA_Regions_dt)
  return(FEMA_Regions_dt)
}

get_FEMA_Regions_sf <- function(){
  FEMA_Regions_sf_rds <- file.path(.fema_workdir, "FEMA_Regions_sf.rds")# ; print(file.info(FEMA_Regions_sf_rds))
  if(file.exists(  FEMA_Regions_sf_rds)) {
    FEMA_Regions_sf <- readRDS(FEMA_Regions_sf_rds)
  } else {

    list.files(file.path(.fema_datadir, "FEMA_Regions"), recursive = TRUE, full.names = TRUE)

    FEMA_Regions_sf <- st_read(file.path(.fema_datadir, "FEMA_Regions")) %>% st_transform( st_crs(3857))
    FEMA_Regions_sf$AREA <-  FEMA_Regions_sf%>% st_area()

    saveRDS(FEMA_Regions_sf,   FEMA_Regions_sf_rds); print(file.info(FEMA_Regions_sf_rds))
  }# ; str(FEMA_Regions_sf)
  FEMA_Regions_sf
}
