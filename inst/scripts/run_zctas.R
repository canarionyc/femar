# setup -------------------------------------------------------------------
# rm(list = ls())
source("~/Spatial/.RProfile")
library(configr)
configr::read.config()
devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)

options(tigris_year=2020L)
browseURL("https://www2.census.gov/geo/tiger/TIGER2010/ZCTA5")
browseURL("https://www2.census.gov/geo/tiger/TIGER2020/ZCTA5")

# zctas area verification -------------------------------------------------------------------

?zctas
zctas_sf <- zctas(cb=FALSE, keep_zipped_shapefile =TRUE)
zctas_sf

# devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE);zctas_sf <- get_zctas_sf()
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE);(zctas_vect <- zctas_vect())
names(zctas_vect)
class(zctas_vect)

zctas_vect$ZCTA3CE20<-substr(zctas_vect$ZCTA5CE20, 1,3)
summary(zctas_vect[, c('ZCTA3CE20','ALAND20', 'AWATER20')])


(zctas_dt <- st_drop_geometry(zctas_sf) %>% setDT(key = c('ZCTA5CE20')))

(zctas_stats <- zctas_dt[, .(ZCTA5CE20_COUNT=uniqueN(ZCTA5CE20), ALAND20 =sum( ALAND20 ), AWATER20=sum(AWATER20))]) %>% as_tibble()


# tab20_zcta520_county20_natl ---------------------------------------------

(tab20_zcta520_county20_natl <- get_tab20_zcta520_county20_natl())
str(tab20_zcta520_county20_natl)
names(tab20_zcta520_county20_natl)

tab20_zcta520_county20_natl[, county_pct:=100*AREALAND_PART/AREALAND_ZCTA5_20]
tab20_zcta520_county20_natl[, GEOID_ZCTA3_20:=substr(GEOID_ZCTA5_20,1,3)]

?aggregate.data.frame
(tab20_zcta320_county20_natl <- aggregate(tab20_zcta520_county20_natl[, c("AREALAND_ZCTA5_20"
                                                                          ,"AREALAND_PART" )]
                                          ,by=list(GEOID_ZCTA3_20=tab20_zcta520_county20_natl$GEOID_ZCTA3_20, GEOID_COUNTY_20=tab20_zcta520_county20_natl$GEOID_COUNTY_20 )
                                          ,FUN=base::sum))
tab20_zcta320_county20_natl

tab20_zcta520_county20_natl[, .(county_count=uniqueN(GEOID_COUNTY_20)
                                ,county_pct=sum(county_pct)), by=c('GEOID_ZCTA5_20')][order(county_count, decreasing = TRUE)]

?substr
(zip3_county_stats <- tab20_zcta520_county20_natl[, .(county_count=uniqueN(GEOID_COUNTY_20))
                                                  , by=substr(GEOID_ZCTA5_20,1,3)])[order(county_count, decreasing = TRUE)]


# zip_code_short ----------------------------------------------------------

devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE); zip_code_short_vect <- get_zip_code_short_vect()
