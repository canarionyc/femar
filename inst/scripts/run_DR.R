# setup -------------------------------------------------------------------

data(state, package="datasets")
help(state, package="datasets")

library(tigris)
library(terra); library(tidyterra)

# fips_codes <- setDT(tigris::fips_codes)
# str(fips_codes)
# ?anyDuplicated.data.table
# anyDuplicated(fips_codes, by="county")
# fips_codes[125]
# fips_codes[county=="Calhoun County"]


# counties_vect ------------------------------------------------------------
?tigris::counties

devtools::load_all("~/Spatial/FEMA/femar/", export_all = TRUE); debugonce(get_counties_vect); counties_vect <- get_counties_vect(state=c('NC'),keep_zipped_shapefile =TRUE)
counties_vect
names(counties_vect)

counties_vect[grep("Ashe", counties_vect$NAME),]

methods(class = "SpatVector")



# states_vect -------------------------------------------------------------
?subset
devtools::load_all("~/Spatial/FEMA/femar/", export_all = TRUE); (states_vect <- get_states_vect(keep_zipped_shapefile =TRUE) %>% subset(states_vect$STUSPS=='NC'))
names(states_vect)

st_bbox(states_vect)

states_vect$ALAND+states_vect$AWATER; sum(counties_vect$ALAND)+sum(counties_vect$AWATER)

# DR ---------------------------------------------------

browseURL(.fema_datadir)
DR_xlsx <- file.path(.fema_datadir, "DR.xlsx"); stopifnot(file.exists(DR_xlsx))
DR_xlsx

?read.xlsx
DR <- readxl::read_xlsx(DR_xlsx) %>% as.data.table()
DR[, county:=gsub("[()]","", county)]
str(DR)

(DR_fips <- merge(DR[state=="NC"], tigris::fips_codes, by = c('county','state'), all.x = TRUE, all.y = FALSE)) %>%
  setcolorder(c('Event','DR', 'state_code','state', 'state_name', 'county_code','county'))
print(DR_fips)

# DR[counties_dt, CBSAFP:=CBSAFP, on= 'NAMELSAD']
#
# DR_CBSA<- merge(DR, counties_dt, by = 'NAMELSAD')
# DR_CBSA


# counties_DR_vect --------------------------------------


?`merge,SpatVector,data.frame-method`
names(counties_vect)

(counties_DR_vect <- merge(counties_vect, DR_fips
                          , by.x=c("STATEFP" , "COUNTYFP" ), by.y=c('state_code','county_code')
                          , all.x=FALSE))
names(counties_DR_vect)

table(counties_DR_vect$LSAD, useNA = "ifany")
table(counties_DR_vect$CLASSFP, useNA = "ifany")



(DR_area <- terra::expanse(counties_DR_vect) %>% sum() ) # 34% of NC is DR
100*DR_area/states_vect$ALAND

sum(counties_DR_vect$ALAND)+sum(counties_DR_vect$AWATER);DR_area

?`plot,SpatVector,missing-method`
crs(counties_DR_vect)

counties_DR_vect[]



cbsa_m1_related_vect

gdal(drivers=TRUE)
Helene2024_msa_vect_gpkg <- file.path(the$FEMA_WORKDIR, "Helene2024_msa_vect.gpkg"); print(file.info(Helene2024_msa_vect_gpkg))
writeVector(cbsa_m1_related_vect, filename = Helene2024_msa_vect_gpkg)

?terra::intersect


(cbsa_m1_contains_dd_county <- is.related(cbsa_m1_vect, counties_DR_vect, "contains"))


# zip_code_short_contains_county_dr_test ------------------------------------------------


?terra::is.related
(zip_code_short_contains_county_dr_test <- is.related(zip_code_short_vect, counties_DR_vect, "intersects")) %>% table()

?terra::relate
zip_code_short_dr_pairs_mat <- terra::relate(zip_code_short_vect,counties_DR_vect,  "intersects", pairs=TRUE)


unique(zip_code_short_dr_pairs_mat[,1]) %>% length() # 14 NC counties have intersetc with 36 DR counties
unique(zip_code_short_dr_pairs_mat[,2]) %>% length()

# dr_intersects_zip_code_short_vect ---------------------------------------

?terra::intersect
(dr_intersects_zip_code_short_vect <- terra::intersect(counties_DR_vect,zip_code_short_vect))
names(dr_intersects_zip_code_short_vect)

unique(dr_intersects_zip_code_short_vect$GEOID) %>% length() # 39 NC counties
unique(dr_intersects_zip_code_short_vect$ZCTA3CE20) %>% length() # 14 NC zip3

dr_intersects_zip_code_short_vect %>% polys()

?terra::expanse
dr_intersects_zip_code_short_vect$area <- terra::expanse(dr_intersects_zip_code_short_vect)
names(dr_intersects_zip_code_short_vect)

# the zip3 intersected area captures all of counties DR area
sum(dr_intersects_zip_code_short_vect$area);sum(counties_DR_vect$ALAND)+sum(counties_DR_vect$AWATER)


# zip_code_short_intersects_dr_vect ---------------------------------------

?`terra::[`
# 14 of the 20 zip3 in NC have intersection with DR
# (zip_code_short_intersects_dr_vect <- zip_code_short_vect[counties_DR_vect]) # %>% polys(col="green", alpha=0.4)

(zip_code_short_intersects_dr_vect <- terra::intersect(zip_code_short_vect,counties_DR_vect))
names(zip_code_short_intersects_dr_vect)

zip_code_short_intersects_dr_vect$area <- terra::expanse(zip_code_short_intersects_dr_vect)

# the zip3 intersected area captures all of counties DR area
sum(zip_code_short_intersects_dr_vect$area);sum(counties_DR_vect$ALAND)+sum(counties_DR_vect$AWATER)

?terra::aggregate
(zip_code_short_intersects_agg_dr_vect <- terra::aggregate(zip_code_short_intersects_dr_vect[c("ZCTA3CE20","area")], by="ZCTA3CE20", fun=base::sum))

?terra::merge
(zip_code_short_intersects_agg_dr_x_vect <- merge(zip_code_short_intersects_agg_dr_vect , zip_code_short_vect, by="ZCTA3CE20", all=FALSE
                                                  , suffixes = c(".county",".ZCTA5CE20")))

zip_code_short_intersects_agg_dr_x_vect$agg_area %>% sum()

?dplyr::mutate
zip_code_short_intersects_agg_dr_x_vect%<>%tidyterra::mutate(agg_area_pct=100*agg_area/(agg_ALAND20+agg_AWATER20 ))


# zip_code_short_vect %>% polys(border="grey")


zip_code_short_vect %>% terra::crop(counties_DR_vect) %>% plot()

zip_code_short_vect %>% subset(zip_code_short_vect$ZCTA3CE20=="240") %>% polys(lwd=2)
dr_intersects_zip_code_short_vect %>% # subset(dr_intersects_zip_code_short_vect$ZCTA3CE20=="240", select=c('ZCTA3CE20','agg_ALAND20', 'STATEFP', 'STUSPS', 'COUNTYFP','NAME',"area" )) %>%
  polys(#border="firebrick",
    col="yellow", alpha=0.5)

?terra::aggregate
zip_code_short_intersects_dr_vect_stats <- terra::aggregate(dr_intersects_zip_code_short_vect[c('ZCTA3CE20','area')], by='ZCTA3CE20', fun=base::sum)
zip_code_short_intersects_dr_vect_stats


zip_code_short_intersects_dr_vect_stats %>% plot()
counties_DR_vect %>% polys( col="yellow", alpha=0.2
                           , border=NA)
counties_dt
counties_dt[DR_fips, DR:=DR,  on= c("COUNTYFP==county_code","STATEFP==state_code")]

counties_dt[, .(county_count=.N
                , state_count=uniqueN(STATEFP)
                , DR_count=uniqueN(DR)
), keyby = 'CBSAFP']

counties_dt[!is.na(DR), .(county_count=.N
                          , state_count=uniqueN(STATEFP)
                          , cbsa_count=uniqueN(CBSAFP)
), keyby = 'DR']


# zip_code_short_vect -----------------------------------------------------
st_bbox(zip_code_short_vect)
devtools::load_all("~/Spatial/FEMA/femar/", export_all = TRUE); (zip_code_short_vect <- get_zip_code_short_vect())
(zip_code_short_vect <- zip_code_short_vect%>% crop(states_vect)) # free up memory



# plot --------------------------------------------------------------------
?polys

colors()
plot(counties_DR_vect
     , col="yellow", alpha=0.5
     , border="firebrick", main="FEMA Disaster Declaration DR-4827 (NC Helene 2024)")
# text(counties_DR_vect,"CBSAFP", cex=0.5 )
states_vect %>% lines(lwd=2, col="black")# ;text(states_vect, "STUSPS", cex=1 )

# names(states_vec)
zip_code_short_intersects_agg_dr_x_vect %>% tidyterra::filter(agg_area_pct>=50) %>% polys(col="lightgreen"
                                            ,border="darkgreen"
                                            ,alpha=0.5)
?tidyterra::select
?as.data.frame
zip_code_short_intersects_agg_dr_x_vect %>% tidyterra::filter(agg_area_pct>=50) %>%
  dplyr::select('ZCTA3CE20') %>%
  as.data.frame() %>% as.vector() %>% dput()

zip_code_short_vect %>%
  #  subset(zip_code_short_dd_county) %>%
  polys(border="darkblue"
        ,col="lightblue"
        ,alpha=0.5)

(Helene_vec %>% as.lines() ) %>% lines(col="blue", lwd=3, lty="dashed")

# DR4827_png ----

DR4827_png <- file.path(the$FEMA_WORKDIR, format(Sys.time(),"DR4827_%Y%m%d_%H%M.png")); print(file.info(DR4827_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = DR4827_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(DR4827_png)['size'])
browseURL(dirname(DR4827_png))

(cbsa_m1_related_vect <- tigris::core_based_statistical_areas(cb=TRUE,keep_zipped_shapefile =TRUE) %>%
    subset(LSAD=="M1") %>%
    terra::vect() %>%
    {subset(.,is.related(.,counties_DR_vect, "contains"))} )%>% polys(col="cyan"
                                                                     ,border="blue"
                                                                     ,alpha=0.5)

