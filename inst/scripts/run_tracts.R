# setup -------------------------------------------------------------------
# rm(list = ls())
source("~/Spatial/.RProfile")
library(configr)
library(tigris)
configr::read.config()
devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)
print(getOption("tigris_year"))
# source("~/lattice_setup.R")

# -------------------------------------------------------------------------

(STATEFP <- unique(tigris::fips_codes$state_code))

(url_tracts_zip <- paste("https://www2.census.gov/geo/tiger/TIGER2023/TRACT/tl_2023",STATEFP, "tract.zip", sep="_"))

?download.file
setwd()
lapply(url_tracts_zip[-1], function(url) tryCatch(
  {
    destfile <- file.path(Sys.getenv('TIGRIS_CACHE_DIR'),basename(url))
    if(file.exists(destfile)) return(destfile)
    download.file(url, destfile = destfile)}
  , error=function(e) {print(e); return(NULL)}
))

(STATEFP <- unique(tigris::fips_codes$state_code))

tracts_sf%>% subset(STATEFP=='48')

tract_sf%>% subset(TRACTCE==substr('481810005011011',6,11))

nchar("48157674100")

names(tract_sf)

table(dupes <- duplicated(tract_sf, by=c( 'TRACTCE')))  # => tract code is unique nationwide


# tracts_vect -------------------------------------------------------------


?tracts


devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE); tracts_vect <- get_tracts_vect(cb=TRUE,state='22',keep_zipped_shapefile =TRUE)

tracts_vect %>% terra::plot(main="Louisiana tracts", border="grey")

# NRI_tracts_sf -----------------------------------------------------------

(NRI_tracts_sf <- get_NRI_tracts_sf())


(tx_NRI_tracts_sf <- NRI_tracts_sf %>% subset(STATEFIPS!='02' & STATEFIPS!='15' & STATEFIPS<60 & STATEFIPS=='48', 'ALR_VALB') %>%
    mutate(ALR_VALB_PER_MN=round(1e6*ALR_VALB),0) ) # %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)

(tx_NRI_tracts_vect <- terra::vect(tx_NRI_tracts_sf))


# tx_NRI_tracts_alr_valb_png ----

tx_NRI_tracts_alr_valb_png <- file.path(the$CENSUS_WORKDIR, format(Sys.time(),"tx_NRI_tracts_alr_valb_%Y%m%d_%H%M.png")); print(file.info(tx_NRI_tracts_alr_valb_png))
library(Cairo)
Cairo::CairoPNG(filename = tx_NRI_tracts_alr_valb_png, width = 10.0, height = 6.0, dpi=300, units="in")
?terra::plot
?terra::legend
terra::plot(tx_tracts_vect, 'ALR_VALB_PER_MN'
            , main="Annualized Loss in Building Value $ Amt per $M"
            , border="grey"
            , alpha=0.5
            , axes=TRUE
            , grid=TRUE)
dev.off()
print(file.info(tx_NRI_tracts_alr_valb_png))
browseURL(dirname(tx_NRI_tracts_alr_valb_png))

library(tmap)
tmap_mode("view")
?tm_layout
library(leaflet)
?leafletOptions
tm_shape(tx_tracts_sf) + tm_fill(fill='ALR_VALB_PER_MN'
                                 , main="Annualized Loss in Building Value $ Amt per $M")



# ?leaflet
# ?addTiles
tx_tracts_sf <- tracts(state="TX", cb = TRUE)
st_crs(tx_tracts_sf)
# doesnt work in Mercator
# 1: sf layer is not long-lat data
# 2: sf layer has inconsistent datum (+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs).
# Need '+proj=longlat +datum=WGS84'
(m <- leaflet(tx_tracts_sf) %>% addTiles() %>% addPolygons() )


# tracts in a FEMA Region ------------------------------------------------------

library(tigris)
?load_tiger
states.RegionIV.tracts_sf <- lapply(states.RegionIV, tigris::tracts, cb=TRUE, keep_zipped_shapefile = TRUE)%>%
  rbind_tigris()

states.RegionIV.tracts_sf

terra::plot(terra::vect(states.RegionIV.tracts_sf))

# NRI_tracts in a FEMA Region ------------------------------------------------------

library(tigris)
?load_tiger
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE); (NRI_tracts_RegIV_sf <-get_NRI_tracts_sf() %>%
                                                                              subset(STATEABBRV %in% states.RegionIV))

levels(NRI_tracts_RegIV_sf$RISK_RATNG)
# NRI_tracts_sf$RISK_RATNG<- factor(NRI_tracts_sf$RISK_RATNG
#                                   , levels = c("Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"))


(NRI_tracts_RegIV_vect <- terra::vect(NRI_tracts_RegIV_sf))
?terra::plot
terra::plot(NRI_tracts_RegIV_vect, 'RISK_RATNG'
            , sort=c("Very High", "Relatively High", "Relatively Moderate", "Relatively Low",                                                          "Very Low")
#            , sort=FALSE
            , border=NULL, alpha=0.5
, main="FEMA Region IV Risk Rating by Census Tract")

# debugonce(get_NRI_tracts_sf); states.RegionIV.tracts_sf <- lapply(states.RegionIV, get_NRI_tracts_sf)%>%
#   rbind_tigris()

states.RegionIV.tracts_sf

terra::plot(terra::vect(states.RegionIV.tracts_sf))


# cleanup -----------------------------------------------------------------

rm(NRI_tracts_sf)
