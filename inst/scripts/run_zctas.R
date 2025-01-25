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


# CA zctas ----------------------------------------------------------------
# ZCTAs are only available by state for 2000 and 2010.
zctas <-zctas_vect(cb=TRUE
                     # , starts_with = '902'
                     #                     , state='CA'
                     , year=2020
                     , keep_zipped_shapefile =TRUE) %>% project("epsg:3857")
zctas

plot(zctas)
# zctas920-------------------------------------------------------------------

?zctas
zctas920 <- zctas_vect(cb=TRUE
                    , starts_with = '902'
#                     , state='CA'
                    , year=2020
                    , keep_zipped_shapefile =TRUE) %>% project("epsg:3857")
zctas920
names(zctas)
class(zctas)

plot(zctas920)
?terra::text
text(zctas920, labels=zctas920$NAME20, cex=0.65)

# zctas area verification -------------------------------------------------------------------

zctas$ZCTA3CE20<-substr(zctas$ZCTA5CE20, 1,3)
# summary(zctas[, c('ZCTA3CE20','ALAND20', 'AWATER20')])


(zctas_dt <- as.data.frame(zctas) %>% setDT(key = c('ZCTA5CE20')))

(zctas_stats <- zctas_dt[, .(ZCTA5CE20_COUNT=uniqueN(ZCTA5CE20)
                             , ALAND20 =sum( ALAND20 )
                             , AWATER20=sum(AWATER20))]) %>% print(big.mark=",")


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

devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE); zip_code_short <- get_zip_code_short_vect( )



(zcta920 <- zip_code_short[zip_code_short$ZCTA3CE20=="920"]) %>% plot()
