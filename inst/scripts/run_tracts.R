# setup -------------------------------------------------------------------
# rm(list = ls())
source("~/Spatial/.RProfile")
library(configr)
# configr::read.config()
library(tigris)

devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)
print(getOption("tigris_year"))
# source("~/lattice_setup.R")

# download tracts -------------------------------------------------------------------------

(STATEFP <- unique(tigris::fips_codes$state_code))

(url_tracts_zip <- paste("https://www2.census.gov/geo/tiger/TIGER2023/TRACT/tl_2023",STATEFP, "tract.zip", sep="_"))

?download.file
setwd()
lapply(url_tracts_zip[-1], function(url) tryCatch({
    destfile <- file.path(Sys.getenv('TIGRIS_CACHE_DIR'),basename(url))
    if(file.exists(destfile)) return(destfile)
    download.file(url, destfile = destfile)}
  , error=function(e) {print(e); return(NULL)}))

(STATEFP <- unique(tigris::fips_codes$state_code))

tracts_sf%>% subset(STATEFP=='48')

tract_sf%>% subset(TRACTCE==substr('481810005011011',6,11))

nchar("48157674100")

names(tract_sf)

table(dupes <- duplicated(tract_sf, by=c( 'TRACTCE')))  # => tract code is unique nationwide


# LA tracts -------------------------------------------------------------


?tracts


devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE); tracts <- get_tracts(cb=TRUE,state='22',keep_zipped_shapefile =TRUE)

tracts %>% terra::plot(main="Louisiana tracts", border="grey")

# cty06037_tracts_2020 ------------------------------------------------------

?tracts
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE); cty06037_tracts_2020 <- tracts_vect(cb=TRUE,state='06', county='037', year=2020, keep_zipped_shapefile =TRUE) %>% project('EPSG:3857')
cty06037_tracts_2020 <- cty06037_tracts_2020 %>% subset(! cty06037_tracts_2020$TRACTCE %in% c('599000','599100'))
cty06037_tracts_2020 %>% terra::plot(main="Los Angeles tracts", border="grey")


# DINS_2025_Palisades_tracts ----------------------------------------------


DINS_2025_Palisades_tracts <- terra::intersect(cty06037_tracts_2020, DINS_2025_Palisades)

table(DINS_2025_Palisades_tracts$TRACTCE)

unique(DINS_2025_Palisades_tracts$TRACTCE) %>% dput()



# NRI_tracts -----------------------------------------------------------

devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE); debugonce(get_NRI_tracts_sf); NRI_tracts_sf <- get_NRI_tracts_sf(statefips = SW.states$STATEFP)
str(NRI_tracts_sf)

quantile(NRI_tracts_sf$POPULATION)

NRI_tracts_pop_sf <- NRI_tracts_sf %>% subset(POPULATION>=5e3)

# ALR_VALB_png ----
plot(st_simplify(NRI_tracts_pop_sf['ALR_VALB']))

plot(density(NRI_tracts_sf$ALR_VALB))

library(classInt)
help(package="classInt")
?classIntervals
pal1 <- c("wheat1", "red3")

library(grDevices)
?colorRampPalette

clI <- classIntervals(NRI_tracts_sf$ALR_VALB,n=5L, style = "quantile")
str(clI)

plot(clI,pal=pal1, main="tracts ALR_VALB")

?findColours
cols <- findColours(clI, pal1)
attributes(cols)
attr(cols, "palette")
# NRI_tracts_sf <- NRI_tracts_sf %>% subset(POPULATION>=5e4)

# NRI_tracts_sf <- NRI_tracts_sf %>% subset(STATEFIPS!='02' & STATEFIPS!='15' & STATEFIPS<60 )
?plot.sf


?st_simplify
# NRI_tracts_sf %>% subset(STATEABBRV=="CA") %>% st_simplify() %>% plot()

?plot.sf
plot(st_simplify(NRI_tracts_sf['ALR_VALB'], preserveTopology = TRUE, dTolerance = 1e3)
     , breaks=clI$brks
     , pal=colorRampPalette(pal1)
     , border=NA
     , main="Annualized Loss Rate in Building Value")


ALR_VALB_png <- file.path(the$FHFA_WORKDIR, format(Sys.time(),"ALR_VALB_%Y%m%d_%H%M.png")); print(file.info(ALR_VALB_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = ALR_VALB_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(ALR_VALB_png)['size'])
browseURL(dirname(ALR_VALB_png))

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
