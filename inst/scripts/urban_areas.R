# setup ----
library(tigris)
library(sf)
library(rgeos)
# library(leaflet)


# tigris_cache_dir(Sys.getenv('TIGRIS_CACHE_DIR'))
readRenviron('~/.Renviron')
Sys.getenv('TIGRIS_CACHE_DIR')
if(! dir.exists(Sys.getenv('TIGRIS_CACHE_DIR')))
  dir.create(Sys.getenv('TIGRIS_CACHE_DIR'), recursive = TRUE)
options(tigris_use_cache = TRUE)


# us_zip5_map <- zctas(cb = FALSE
#   # , starts_with = c("37", "38", "72")
#   )


#  urbanized area ---------------------------------------------------------



#  Let’s first find the boundary of the Memphis urbanized area. Note: this is different from the
#  Memphis metropolitan area, which is county-based and accessible through the
#  core_based_statistical_areas function in tigris.

uas_sf <- urban_areas(cb = FALSE)
names(uas_sf)
uas_sf
memphis_ua <- uas_sf[grep("Memphis", uas_sf$NAME10), ]


(miami_ua <- uas_sf[grep("Miami, FL", uas_sf$NAME10) , ])

# miami_ua %>% leaflet() %>% addTiles() %>% addPolygons(fillOpacity = 0, color = "red")

plot(st_geometry(miami_ua))

leaflet(memphis_ua) %>% addTiles() %>% addPolygons(fillOpacity = 0, color = "red")

# let’s extract all those ZCTAs that intersect the urbanized area using the gIntersect function
# from rgeos.

mem_zcta1 <- us_zip5_map[as.vector(gIntersects(us_zip5_map, memphis_ua, byid = TRUE)), ]

leaflet() %>%
  addTiles() %>%
  addPolygons(data = mem_zcta1) %>%
  addPolygons(data = memphis_ua, fillOpacity = 0, color = "red")

# The blue polygons represent the ZCTAs that intersect the Memphis urbanized area, and the red
# boundary represents the Memphis urbanized area itself. As we can see, there are several ZCTAs that
# touch the Memphis urbanized area boundary, but are mostly located outside the urbanized area. In
# turn, let’s retrieve only those ZCTAs that have their centroids in the urbanized area.

centroids <- gCentroid(us_zip5_map, byid = TRUE)

mem_zcta2 <- us_zip5_map[as.vector(gContains(memphis_ua, centroids, byid = TRUE)), ]
mem_zcta2@data


leaflet() %>%
  addTiles() %>%
  addPolygons(data = mem_zcta2) %>%
  addPolygons(data = memphis_ua, fillOpacity = 0, color = "red")

# This certainly subsets the data, but also leaves out some areas that are clearly in the Memphis
# urbanized area, as we can see from the OpenStreetMap tileset. These neighborhoods are part of
# larger ZCTAs that extend beyond the urbanized area and in turn have their centroids outside of
# that area.
