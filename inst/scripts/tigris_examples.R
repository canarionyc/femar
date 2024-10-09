# setup ----
options(rgdal_show_exportToProj4_warnings="none")
library(rgdal)
# library(tmap)
library(raster) # needed for plot(ca)
library(sf); help(package="sf")
library(rgeos)
# library(leaflet)
# library(mapview)

# file.show('~/.Renviron')
readRenviron('~/.Renviron')
cat("CENSUS_KEY:",Sys.getenv("CENSUS_KEY"))
cat("TIGRIS_CACHE_DIR:", Sys.getenv('TIGRIS_CACHE_DIR'))
# if(! dir.exists(Sys.getenv('TIGRIS_CACHE_DIR')))
#   dir.create(Sys.getenv('TIGRIS_CACHE_DIR'))

source("~/Spatial/Tigris/.RProfile")

browseURL(.census_workdir)



# conus_png ----

# conus_sf <- get_conus_sf()
#
# conus_png <- file.path(Sys.getenv("R_WORK_DIR"), format(Sys.time(),"conus_%Y%m%d_%H%M.png")); print(file.info(conus_png))
# library(Cairo)
# Cairo::CairoPNG(filename = conus_png, width = 10.0, height = 6.0, dpi=300, units="in")
# plot(conus.trellis)
# dev.off()
# print(file.info(conus_png))
# browseURL(dirname(conus_png))
#
# library(terra)
# terra::plot(st_geometry(conus_sf))

library(terra)
conus_sf <- get_states_sf() %>% subset(STATEFP!="02" & STATEFP !="15" & STATEFP<60)
str(conus_sf)
conus_v <- terra::vect(conus_sf)
class(conus_v)
?terra::plot
dev.new()
plot(conus_v); terra::text(conus_v, labels= conus_v$NAME, cex=0.65)
# ?terra::text


# divisions ---------------------------------------------------------------
?divisions
options(tigris_year = 2022); getOption("tigris_year")
debugonce(divisions)
divs_sf <- tigris::divisions(keep_zipped_shapefile=TRUE)
divs_sf

debugonce(load_tiger)
debugonce(httr::GET)
list.files(Sys.getenv("TIGRIS_CACHE_DIR"), pattern = "cb_\\d{4}_us_division_", full.names = TRUE)

subset(divs, NAME=="Pacific" ) %>% st_geometry() %>% plot()

divs_conus_no_pacific_sf <- subset(divs, NAME!="Pacific" )
# vs_conus_no_pacific_sf$NAME <- as.factor(divs_conus_no_pacific_sf$NAME)
print(divs_conus_no_pacific_sf)
plot(divs_conus_no_pacific_sf['NAME'])

terra::plot()

divs <- divisions(keep_zipped_shapefile=TRUE)
divs
?plot.sf
plot(st_geometry(divs), extent=conus_sf)

divs_conus_sf <- st_crop(divs,conus_sf)
divs_conus_sf
st_bbox(conus_sf); st_bbox(divs_conus_sf)

# debug(plot_sf)
undebug(plot_sf)
plot_sf(divs_conus_sf['NAME'])


?terra::plot


plot(st_geometry(divs_conus_sf))

divs_conus_v <- vect(divs_conus_sf)
divs_conus_v
# plot(st_geometry(st_centroid(divs_conus_sf)),add=TRUE)
# terra::plot(divs_conus_sf['NAME'])

subset(states_sf, REGION==1, select=c('STUSPS', 'NAME' )) # North East
subset(states_sf, REGION==2, select=c('STUSPS', 'NAME' )) # North West
subset(states_sf, REGION==3, select=c('STUSPS', 'NAME' )) # South East
subset(states_sf, REGION==4, select=c('STUSPS', 'NAME' ))# South West + Hawaii
subset(states_sf, REGION==9, select=c('STUSPS', 'NAME' )) # PR, VI, Guam


# uscon <- subset(states_sf, ! STUSPS  %in%  c("AK", "AS","MP", "HI","PR","VI","GU"))
# plot(uscon)
#
# states_map <- leaflet(states_sf) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(fillColor = "white",
#     color = "black",
#     weight = 0.5) %>%
#   setView(-98.5795, 39.8282, zoom=3)
#
# mapview::mapview(states_sf)





# core_based_statistical_areas  ----------------------------------------------------------------------
?core_based_statistical_areas
browseURL("https://www.census.gov/programs-surveys/metro-micro.html")
debugonce(core_based_statistical_areas)
source("~/Spatial/Tigris/.RProfile");
(cbsa_sf <- core_based_statistical_areas(cb=TRUE,  keep_zipped_shapefile=TRUE))
cbsa_sf
cbsa_sf[grep("Houston", cbsa_sf$NAME),] # 26420
cbsa_sf[grep("Dallas", cbsa_sf$NAME),] # 19100

st_intersects()
st_contains()
?st_contains


# Metropolitan Statistical Areas ------------------------------------------

cbsa_m1_sf <- cbsa_sf %>% subset(LSAD=='M1')
cbsa_m1_sf # 392 MSAs

cbsa_m1_sf[grep("New York", cbsa_m1_sf$NAME),] # 35620
cbsa_m1_sf[grep("Los Angeles", cbsa_m1_sf$NAME),] # 31080
cbsa_m1_sf[grep("Miami", cbsa_m1_sf$NAME),] # 33100
cbsa_m1_sf[grep("Chicago", cbsa_m1_sf$NAME),] # 16980

c(35620,31080,33100,16980)

cbsa_m1_sf %>% st_filter(conus_sf, .predicate = st_intersects) # 380 MSAs in conus

# ?st_join
# conus_cbsa_sf <- st_join(cbsa_sf, conus_sf, join = st_intersects)
# conus_cbsa_sf

cbsa_conus_sf <- st_filter(cbsa_sf, conus_sf, .predicate = st_intersects)
cbsa_conus_sf

# CBSAs in Texas ----------------------------------------------------------

tx_sf <- states_sf[states_sf$STUSPS=='TX',]
plot(st_geometry(tx_sf))
tx_v <- vect(tx_sf)


# cbsa_tx_sf <- st_intersection(cbsa_sf, tx_sf)
# cbsa_tx_sf

tx_cbsa_sgbp <- st_contains_properly(tx_sf, cbsa_sf)

cbsa_sf[unlist(tx_cbsa_sgbp),]

cbsa_tx_m1_sf <- cbsa_tx_sf %>% subset(LSAD=='M1')
cbsa_tx_m1_sf

cbsa_tx_m1_sf[grep("Houston", cbsa_tx_m1_sf$NAME),] # MSA 26420
cbsa_tx_m1_v <- vect(cbsa_tx_m1_sf)

plot(st_geometry(tx_sf), lwd=2)
plot(st_geometry(cbsa_tx_m1_sf), add=TRUE)

?terra::text


?terra::plot
?st_contains_properly

# cbsa_tx_png ----

cbsa_tx_png <- file.path(.census_workdir, format(Sys.time(),"cbsa_tx_%Y%m%d_%H%M.png")); print(file.info(cbsa_tx_png))
library(Cairo)
Cairo::CairoPNG(filename = cbsa_tx_png, width = 10.0, height = 6.0, dpi=300, units="in")
tx_sf %>% vect %>% plot(lwd=2,main="MSAs in Texas")
(cbsa_tx_m1_v <- cbsa_m1_sf %>% st_filter(tx_sf, .predicate = st_covered_by) %>% vect()) %>% plot(add=TRUE)
cbsa_tx_m1_v %>% {text(.,labels=sub(x=.$NAME, pattern = ", TX$",replacement = "", perl = TRUE), cex=0.65, xpd=TRUE)}
dev.off()
print(file.info(cbsa_tx_png));browseURL(dirname(cbsa_tx_png))


#  combined_statistical_areas -----------------------------------------------------------------------

?combined_statistical_areas

"https://www2.census.gov/geo/tiger/GENZ2022/shp/cb_2022_us_csa_500k.zip"



csa_sf <- combined_statistical_areas(cb=TRUE, keep_zipped_shapefile=TRUE, year=2021)
csa_sf
nrow(csa_sf)

csa_tx_sf <- st_intersection(csa_sf, tx_sf)
?st_intersects

csa_tx_mat <- st_intersects(csa_sf, tx_sf, sparse = FALSE)
csa_tx_mat

csa_sf[csa_tx_mat[,1],] %>% print(n=99L)

# all counties ----
getOption("tigris_year")
counties_sf <- counties(cb = TRUE, year = 2021)

state <- "CA"
lookup_code(state)

?counties

conus_counties_sf <- get_counties_conus_sf()
conus_counties_sf

plot(st_geometry(conus_counties_sf))
plot(st_geometry(conus_sf), lwd=2, add=TRUE)


?fips_codes
(state_fips <- unique(fips_codes$state_code))
(state <- unique(fips_codes$state))
# names(state) <- state
# counties_lst <- lapply(state, function(x)counties(cb = FALSE,state = x, year = 2021))
# names(counties_lst)
# names(counties_lst[[1]])
# counties_sf <- do.call("rbind",counties_lst)

counties_sf <- counties(cb = FALSE,year = 2021) # cb=FALSE to obtain MSA

counties_sf %>% subset(CBSAFP == 26420)

# counties within cbsas ---------------------------------------------------

counties_cbsa_sf <- aggregate(counties_sf[, c('ALAND', 'AWATER')], list(CBSAFP=counties_sf$CBSAFP), FUN=sum)

# CA counties -------------------------------------------------------------
state <- '06'

ca_sf <- counties(state = state, keep_zipped_shapefile=TRUE)
ca_sf
class(ca_sf)
plot(st_geometry(ca_sf))

# TX counties -------------------------------------------------------------
source("~/Spatial/Tigris/.RProfile")
getOption("tigris_year")
state <- "TX"
?lookup_code
lookup_code(state)
state_fips <- '48'

tx_counties_sf <- counties(state = state_fips, keep_zipped_shapefile=TRUE)
tx_counties_sf
tx_counties_sf %>% subset(!is.na(CBSAFP))
plot(st_geometry(tx_counties_sf))

# counties in Houston MSA -------------------------------------------------
(tx_sf <- counties(state = '48', year=2021, keep_zipped_shapefile=TRUE))

counties_sf%>% subset(CBSAFP == 26420)

houston_tx_sf <- tx_sf %>% subset(CBSAFP == 26420)
plot(st_geometry(houston_tx_sf), col="red", add=TRUE)

# counties in Dallas MSA -------------------------------------------------

dallas_tx_sf <- tx_sf %>% subset(CBSAFP == 19100)
plot(st_geometry(dallas_tx_sf), col="blue", add=TRUE)

# az_counties_sf <- counties(state = "AZ", class="sf")
# names(az_counties_sf)
# show(az_counties_sf)

# places ----
ny_places <- places('36')
plot(ny_places)

ca_places <- places('CA')
names(ca_places)
show(ca_places)

lapply(state.abb, places)

# plot(ca_places)
head(ca_places@data)


# cbsas and counties within cbsas in TX -------------------------------------------------------------
state_cbsa_gpkg <- file.path(.census_workdir, "tx_cbsa_sf.gpkg"); print(file.info(state_cbsa_gpkg))
if(file.exists(state_cbsa_gpkg)) {
  tx_cbsa_sf <- st_read(state_cbsa_gpkg, layer = 'State_CBSA')
  tx_counties_sf <-  st_read(state_cbsa_gpkg, layer = 'State_Counties')
} else {
  tx_cbsa_sf <- aggregate(tx_counties_sf[, c('ALAND', 'AWATER')], list(CBSAFP=tx_counties_sf$CBSAFP), FUN=sum)
  tx_cbsa_sf

  ?st_write
  # state_cbsa_shp <- file.path(.census_workdir, "tx_cbsa_sf.shp"); print(file.info(state_cbsa_shp))
  # st_write(tx_cbsa_sf, dsn = state_cbsa_shp) # produces warnings
  # tx_cbsa_sf <- st_read(state_cbsa_shp)

  st_write(tx_cbsa_sf, dsn = state_cbsa_gpkg, layer = 'State_CBSA')
  st_write(tx_counties_sf, dsn = state_cbsa_gpkg, layer = 'State_Counties')
  print(st_layers(state_cbsa_gpkg))
};

st_drivers()


?terra::vect
plot(vect(tx_cbsa_sf),'CBSAFP', lwd=2)
plot(vect(tx_counties_sf), add=TRUE)



# urban_areas -------------------------------------------------------------
options(tigris_year = 2021L)
(uas_sf <- urban_areas(cb=TRUE, keep_zipped_shapefile=TRUE))





# roads ----
loving <- roads("TX", "Loving")
plot(loving)

# combine tracts -----
sts <- c("DC", "MD", "VA")
combined <- rbind_tigris(
  lapply(sts, function(x) {
    tracts(x, cb = TRUE)
  })
)
plot(combined)

# txlege ----
df <- read.csv("http://personal.tcu.edu/kylewalker/data/txlege.csv",
               stringsAsFactors = FALSE)
districts <- state_legislative_districts("TX", house = "lower",
                                         cb = TRUE)
txlege <- geo_join(districts, df, "NAME", "District")
txlege$color <- ifelse(txlege$Party == "R", "red", "blue")
plot(txlege, col = txlege$color)
legend("topright", legend = c("Republican", "Democrat"),
       fill = c("red", "blue"))



# censusapi ----
## Slides 20-22 ----
# If you have a Census API key, assign yours to the `key` variable.
# If not, get one at http://api.census.gov/data/key_signup.html

library(censusapi)
library(tidyverse)
library(tmap)
library(tigris)
chi_counties <- c("Cook", "DeKalb", "DuPage", "Grundy", "Lake",
                  "Kane", "Kendall", "McHenry", "Will County")

(chi_tracts <- tracts(state = "IL", county = chi_counties, cb = TRUE))
names(chi_tracts)
head(chi_tracts)

# key <- "7787afde3f23dfbf16910395148c2259f8e9cbc2"

data_from_api <- censusapi::getCensus(name = "acs/acs5", vintage = 2015, # key = key,
                           vars = "B25077_001E",
                           region = "tract:*", regionin = "state:17", show_call = TRUE)
str(data_from_api)

values <- data_from_api %>%
  transmute(GEOID = paste0(state, county, tract),
            value = B25077_001E)

library(dplyr)
(chi_joined <- geo_join(chi_tracts, values, by_sp = 'GEOID', by_df='GEOID'))

library(tmap)
library(tmaptools)
tm_shape(chi_joined, projection = 26916) +
  tm_fill("value", style = "quantile", n = 7, palette = "Greens",
          title = "Median home values \nin the Chicago Area") +
  tm_legend(bg.color = "white", bg.alpha = 0.6) +
  tm_style(style = "grey")

# Slide 23 ----

ttm()
st_crs(26916)

tm_shape(chi_joined, projection = 26916) +
  tm_fill("value", style = "quantile", n = 7, palette = "Greens",
          title = "Median home values \nin the Chicago Area")


# Slides 26-28 ----

library(sf)

city_hall <- c(-87.631969, 41.883835) %>%
  st_point() %>%
  st_sfc(crs = 4269) %>%
  st_transform(26916)

options(tigris_class = "sf")
chi_tracts_sf <- tracts(state = "IL", county = chi_counties,
                        cb = TRUE)
chi_tracts_sf %>%
  st_transform(26916) %>%
  left_join(values, by = "GEOID") %>%
  mutate(dist = as.numeric(
    st_distance(
      st_centroid(.), city_hall
    )
  )) %>%
  ggplot(aes(x = dist, y = value)) +
  geom_smooth(span = 0.3, method = "loess")

# Slide 29

set.seed(1983)

# key <- "YOUR KEY GOES HERE"

data_from_api <- getCensus(name = "acs5", vintage = 2015, key = key,
                           vars = c("B19001_002E", "B19001_003E", "B19001_004E",
                                    "B19001_005E", "B19001_017E"),
                           region = "tract:*", regionin = "state:11")


income_data = data_from_api %>%
  transmute(GEOID = paste0(state, county, tract),
            below25 = as.integer((B19001_002E + B19001_003E + B19001_004E + B19001_005E) / 10),
            above200 = as.integer(B19001_017E / 10))

dc <- tracts("DC", cb = TRUE) %>%
  left_join(income_data, by = "GEOID") %>%
  st_transform(26918)

below25 <- st_sample(dc, dc$below25) %>%
  st_sf() %>%
  setNames(., "geometry") %>%
  mutate(type = "Below $25k")

above200 <- st_sample(dc, dc$above200) %>%
  st_sf() %>%
  setNames(., "geometry") %>%
  mutate(type = "Above $200k")

dots <- rbind(below25, above200)

dots %>%
  ggplot() +
  geom_sf(aes(color = type, fill = type), shape = ".") +
  scale_color_manual(values = c("#fff04f", "#60eaf2")) +
  scale_fill_manual(values = c("#fff04f", "#60eaf2")) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "black"),
        legend.title = element_blank(),
        text = element_text(color = "white"),
        panel.grid.major = element_blank(),
        plot.margin = margin(0.5, 0.2, 0.2, 0.2, "cm"),
        plot.caption = element_text(size = 8)) +
  labs(title = "Income inequality in Washington, DC",
       subtitle = "1 dot = approximately 10 households",
       caption = "Data: 2011-2015 ACS via the tigris and censusapi R packages")

## total population and median household income  ----
# Get total population and median household income for places (cities, towns, villages)
# in one state from the 5-year ACS.
acs_simple <- getCensus(
  name = "acs/acs5",
  vintage = 2020,
  vars = c("NAME", "B01001_001E", "B19013_001E"),
  region = "place:*",
  regionin = "state:01")
head(acs_simple)

# Get all data from the B19013 variable group.
# This returns estimates as well as margins of error and annotation flags.
acs_group <- getCensus(
  name = "acs/acs5",
  vintage = 2020,
  vars = c("B01001_001E", "group(B19013)"),
  region = "place:*",
  regionin = "state:01")
head(acs_group)

## by census tract ----
# Retreive 2010 Decennial Census block-level data within a specific tract,
# using the region in argument to precisely specify the Census tract.
decennial_2010 <- getCensus(
  name = "dec/sf1",
  vintage = 2010,
  vars = c("NAME","P001001"),
  region = "block:*",
  regionin = "state:36+county:027+tract:010000")
head(decennial_2010)

## Small Area Income and Poverty Estimates API ----
# Get poverty rates for children and for people of all ages over time using the
# Small Area Income and Poverty Estimates API
saipe <- getCensus(
  name = "timeseries/poverty/saipe",
  vars = c("NAME", "SAEPOVRT0_17_PT", "SAEPOVRTALL_PT"),
  region = "state:01",
  year = "2000:2019")
head(saipe)

## County Business Patterns  ----
# Get County Business Patterns data for a specific NAICS sector.
cbp_2016 <- getCensus(
  name = "cbp",
  vintage = "2016",
  vars = c("EMP", "ESTAB", "NAICS2012_TTL", "GEO_TTL"),
  region = "state:*",
  naics2012 = "23")
head(cbp_2016)

# fips_codes --------------------------------------------------------------
data("fips_codes")
help("fips_codes", package="tigris")
head(fips_codes)

