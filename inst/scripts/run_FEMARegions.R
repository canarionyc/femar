# setup -------------------------------------------------------------------
# rm(list = ls())
source("~/Spatial/.RProfile")
library(configr)
configr::read.config()
devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)
print(getOption("tigris_year"))
# source("~/lattice_setup.R")


library(rjson)
browseURL(.fema_datadir)





# FEMARegions_json --------------------------------------------------------

FEMARegions_json <- file.path(.fema_datadir, "FEMARegions.json"); print(file.info(FEMARegions_json))
download.file("https://www.fema.gov/api/open/v2/FemaRegions.json", destfile =  FEMARegions_json)

help(package="geojson")
help(package="geojsonio")
library(geojson)

(FEMARegions <- geojsonio::geojson_read(FEMARegions_json))


(states_sf <- get_states_sf())


# FEMA_Regions_dt ---------------------------------------------------------
?`[.data.table`
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE); (FEMA_Regions_dt <- get_FEMA_Regions_dt())

(states.RegionII <- FEMA_Regions_dt[REGION=="Region II",unlist(STATES_SERVING)])


  (states.RegionIV <- FEMA_Regions_dt[REGION=="Region IV", unlist(STATES_SERVING)])

# FEMA_Regions_sf -------------------------------------------------------------


FEMA_Regions_sf %>% st_geometry()

states_sf %>% subset(STATEFP!='02' & STATEFP!='15' & STATEFP<60) %>% st_geometry()  %>% plot(axes=TRUE,border="grey", graticule=TRUE, reset=FALSE)
FEMA_Regions_sf %>% st_geometry()  %>%  plot(add=TRUE,border="black")




