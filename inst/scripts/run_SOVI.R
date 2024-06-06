# setup ----
library(EnvStats)

# Social Vulnerability Index ----------------------------------------------
# https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html


NRI_counties_dt[, plot(x=SOVI_SCORE, y=SOVI_SPCTL)]
str(NRI_counties_dt)
attach(NRI_counties_dt)

xyplot(SOVI_SPCTL~SOVI_SCORE, groups=STATEABBRV, data=NRI_counties_dt)
detach(NRI_counties_dt)

(NRI_counties_by_st <- NRI_counties_dt[, .(
  SOVI_SCORE.MIN=min(SOVI_SCORE, na.rm = TRUE)
                ,SOVI_SCORE.MAX=max(SOVI_SCORE, na.rm = TRUE)
                ,SOVI_SCORE.MEDIAN=quantile(SOVI_SCORE, probs = 0.5, na.rm = TRUE)), keyby = .(STATEFIPS)]) %>% as_tibble()

?fct_reorder
NRI_counties_dt[, STATEABBRV:=as.factor(STATEABBRV)]
lattice::bwplot(STATEABBRV~SOVI_SCORE, NRI_counties_dt, main="SOVI_SCORE")

lattice::bwplot(fct_reorder(STATEABBRV,SOVI_SCORE)~SOVI_SCORE, NRI_counties_dt, main="SOVI_SCORE")
lattice::bwplot(STATEABBRV~SOVI_SPCTL, NRI_counties_dt, main="SOVI_SPCTL")


# CDC ---------------------------------------------------------------------



.cdc_datadir <- file.path(Sys.getenv("DATADIR"), "CDC")
browseURL(.cdc_datadir)
list.files(.cdc_datadir, full.names = TRUE)

SVI_2022_US_county <- fread(file.path(.cdc_datadir, "SVI_2022_US_county.csv"), colClasses = list(character=c('ST','STCNTY','FIPS')))
str(SVI_2022_US_county)

SVI_2022_US_county[grep("Fulton County, Georgia", LOCATION ), .(SPL_THEME1, RPL_THEME1
                                                                ,SPL_THEME2, RPL_THEME2
                                                                ,SPL_THEME3, RPL_THEME3
                                                                ,SPL_THEME4, RPL_THEME4
                                                                ,SPL_THEMES, RPL_THEMES)]


dotplot(ST_ABBR~SPL_THEMES, data = SVI_2022_US_county, main="SPL_THEMES 2022")
dotplot(ST_ABBR~RPL_THEMES, data = SVI_2022_US_county, main="RPL_THEMES 2022")
bwplot(ST_ABBR~RPL_THEMES, data = SVI_2022_US_county, main="RPL_THEMES 2022")

#Fulton County in Georgia Verification -----------------------------------

#For example, using the 2018 Georgia SVI database, Fulton County has an overall
#SVI score of 0.2658 with a ranking of 117 out of 159 Georgia counties. However,
#using the 2018 U.S. SVI database, Fulton County has an overall SVI score of
#0.5268, giving Fulton County a ranking of 125 out of the 159 Georgia counties.
#The ranking differences between the two databases are due to differences in
#summed percentile ranks. In short, because a state has fewer census tracts than
#the U.S., relative differences are more pronounced at the state level than at
#the national level. These comparative differences, when summed, can result in a
#different rank order between the state and U.S. databases.

# by census tract ---------------------------------------------------------

list.files(.cdc_datadir, full.names = TRUE)
(SVI_2018_US <- fread(file.path(.cdc_datadir, "SVI_2018_US.csv"), colClasses = list(character=c('ST','STCNTY','FIPS')),
                     na.strings = c("-999","-999.0")))
str(SVI_2018_US)



# by county ---------------------------------------------------------------

# US level ----------------------------------------------------------------

(SVI_2018_US_county <- fread(file.path(.cdc_datadir, "SVI_2018_US_county.csv"), colClasses = list(character=c('ST','FIPS'))
                            , na.strings = c("-999","-999.0")))
str(SVI_2018_US_county)
SVI_2018_US_county[grep("Fulton County, Georgia", LOCATION ), .(SPL_THEME1, RPL_THEME1
                                                                ,SPL_THEME2, RPL_THEME2
                                                                ,SPL_THEME3, RPL_THEME3
                                                                ,SPL_THEME4, RPL_THEME4
                                                                ,SPL_THEMES, RPL_THEMES)]
SVI_2018_US_county[, .(min(RPL_THEMES, na.rm=TRUE), max(RPL_THEMES, na.rm=TRUE))]

# state level -------------------------------------------------------------
#For example, using the 2018 Georgia SVI database, Fulton County has an overall
#SVI score of 0.2658 with a ranking of 117 out of 159 Georgia counties.

(Georgia_county_2018 <- fread(file.path(.cdc_datadir, "Georgia_county_2018.csv"), colClasses = list(character=c('ST','FIPS')),
                              na.strings = c("-999","-999.0")))
Georgia_county_2018[grep("Fulton County, Georgia", LOCATION ), .(SPL_THEME1, RPL_THEME1
                                                                 ,SPL_THEME2, RPL_THEME2
                                                                 ,SPL_THEME3, RPL_THEME3
                                                                 ,SPL_THEME4, RPL_THEME4
                                                                 ,SPL_THEMES, RPL_THEMES)]


# South Carolina University -----------------------------------------------


# https://www.sc.edu/study/colleges_schools/artsandsciences/centers_and_institutes/hvri/index.php/sovi%c2%ae-0
.sc_datadir <- file.path(Sys.getenv("DATADIR"), "SC")
list.files(.sc_datadir, full.names = TRUE)
sovi2019_countyus_xlsx <- file.path(.cdc_datadir, "sovi2019_countyus.xlsx")
browseURL(sovi2019_countyus_xlsx)

sovi2019_countyus <- readxl::read_xlsx(sovi2019_countyus_xlsx) %>% setDT(key = c("GeoID"))

str(sovi2019_countyus)
sovi2019_countyus[,GeoID_A:=NULL]
?iconv

table(nchar(sovi2019_countyus$GeoID))
sovi2019_countyus[, STCOFIPS:=substr(GeoID, 10,14)]
sovi2019_countyus[, STATEFIPS:=substr(GeoID, 10,11)]

sovi2019_countyus[, SoVI2019US_SCORE:=100*(SoVI2019US-min(SoVI2019US))/(max(SoVI2019US)-min(SoVI2019US))]

str(sovi2019_countyus)
dotplot(STATEFIPS~SoVI2019US, sovi2019_countyus, main="SoVI2019US")
lattice::bwplot(SoVI2019US~STATEFIPS, sovi2019_countyus)
dotplot(STATEFIPS~SoVI2019US_SCORE, sovi2019_countyus, main="sovi2019_countyus SOVI SCORE?")

counties_sf
sovi2019_countyus
(sovi2019_countyus_sf <- merge(counties_sf, sovi2019_countyus, by.x=c('AFFGEOID'), by.y=c('GeoID')))
sovi2019_countyus_sf

(sovi2019_NRI_counties_sf <- merge(NRI_counties_sf, sovi2019_countyus, by=c('STATEFIPS',  'STCOFIPS')))
(sovi2019_NRI_counties_dt <- merge(NRI_counties_dt, sovi2019_countyus, by=c('STATEFIPS',  'STCOFIPS')))

sovi2019_NRI_counties_dt[, .(STCOFIPS, SOVI_SCORE,SOVI_RATNG, SOVI_SPCTL,SoVI2019US, SoVI2019US_SCORE)]


dotplot(STATEFIPS~SOVI_SCORE+SoVI2019US_SCORE, sovi2019_NRI_counties_dt)



dotplot(STATEFIPS~SOVI_SPCTL, NRI_counties_dt)

NRI_counties_dt[, .(STCOFIPS, SOVI_SCORE)]
sovi2019_countyus[, .(STCOFIPS, SOVI_SCORE)]
# sovi.tmap ---------------------------------------------------------------

levels(NRI_counties_sf$SOVI_RATNG)
?fct_na_value_to_level
NRI_counties_sf$SOVI_RATNG <- fct_na_level_to_value(NRI_counties_sf$SOVI_RATNG,extra_levels  = "Data Unavailable" )
levels(NRI_counties_sf$SOVI_RATNG)
str(NRI_counties_sf$SOVI_RATNG)

(sovi.tmap <- NRI_counties_sf %>% subset(STATEFIPS  =='06') %>%
    tm_shape() + tm_polygons(fill='SOVI_RATNG',fill_alpha = 0.5))

print(sovi.tmap)
