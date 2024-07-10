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

browseURL(.census_dir)


# blocks ------------------------------------------------------------------

?blocks

?load_tiger
(tx_blocks_sf <- blocks(state="TX",keep_zipped_shapefile = TRUE))
print(tx_blocks_sf)

tx_blocks.coastline_sf

