# setup -------------------------------------------------------------------
# rm(list = ls())
source("~/Spatial/.RProfile")
library(configr)
configr::read.config()
devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)

options(tigris_year=2020L)
browseURL("https://www2.census.gov/geo/tiger/TIGER2010/ZCTA5")

# zctas area verification -------------------------------------------------------------------


devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE);zctas_sf <- get_zctas_sf()
zctas_sf

(zctas_dt <- st_drop_geometry(zctas_sf) %>% setDT(key = c('ZCTA5CE20')))

(zctas_stats <- zctas_dt[, .(ZCTA5CE20_COUNT=uniqueN(ZCTA5CE20), ALAND20 =sum( ALAND20 ), AWATER20=sum(AWATER20))]) %>% as_tibble()
