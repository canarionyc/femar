# setup -------------------------------------------------------------------
rm(list = ls())

library(lattice)

source("~/Spatial/.RProfile")
library(configr)
configr::read.config()

data(state, package="datasets")
help(state, package="datasets")

cbind(state.name, state.abb)

devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)
# source("~/Spatial/Tigris/tigris_setup.R")

source("~/Spatial/FEMA/femar/R/string_utils.R")

stop()
list.files(.NRI_datadir, full.names = TRUE, recursive = TRUE)

browseURL(NRI_WORKDIR)

# stormevents -------------------------------------------------------------
browseURL("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/")

browseURL(.ncei_datadir)

# dir_create(.ncei_workdir)
StormEvents_details_fst <- file.path(.ncei_workdir, "StormEvents_details.fst"); print(file.info(StormEvents_details_fst)['size'])
if(file.exists(StormEvents_details_fst)) {
  print(fst.metadata(StormEvents_details_fst))
  StormEvents_details <- read_fst(StormEvents_details_fst, as.data.table=TRUE)
} else {
  StormEvents_details.vec <- list.files(file.path(.ncei_datadir, "StormEvents"), pattern = "^StormEvents_details", full.names = TRUE)
  library(rio)
  ?import_list
  StormEvents_details <- rio::import_list(StormEvents_details.vec, setclass = "data.table",rbind = TRUE)
  str(StormEvents_details)

  StormEvents_details[, DAMAGE_PROPERTY:=currency_to_number(StormEvents_details$DAMAGE_PROPERTY)]
  StormEvents_details[, monthly_reporting_period:=ym(paste(YEAR,MONTH_NAME))]
  write_fst(StormEvents_details,StormEvents_details_fst); print(file.info(StormEvents_details_fst)['size'])
}; str(StormEvents_details)
StormEvents_details[, table(DAMAGE_PROPERTY)]

StormEvents_details[, DAMAGE_PROPERTY:=currency_to_number(StormEvents_details$DAMAGE_PROPERTY)]

StormEvents_details[, str(STATE_FIPS)]
StormEvents_details[STATE=="", c('STATE', 'STATE_FIPS'):=list("GUAM",98)]
StormEvents_details[STATE=="GUAM"]

(StormEvents_details_stats <- groupingsets(StormEvents_details
                                           , .(DAMAGE_PROPERTY=sum(DAMAGE_PROPERTY, na.rm = TRUE))
                                           , by = c('STATE','STATE_FIPS','YEAR','monthly_reporting_period')
                                           ,sets=list(c('STATE','STATE_FIPS','YEAR')
                                                      ,c('STATE','STATE_FIPS','YEAR','monthly_reporting_period')
                                           )
                                           ,id = TRUE
)
)
table(StormEvents_details_stats$grouping)
str(StormEvents_details$monthly_reporting_period)


xyplot(DAMAGE_PROPERTY~monthly_reporting_period, groups=STATE, data = StormEvents_details_stats[grouping==0], type="b"
       , scales = list(y=list(log=10))
)

?glayer


dat <- StormEvents_details_stats[grouping==1 & DAMAGE_PROPERTY>=5e9]
dat
undebug(lattice:::ltext.default);
undebug(glayer)
xyplot(DAMAGE_PROPERTY~YEAR, groups=STATE, data = dat
# , panel=function(x,y,...,groups,subscripts) {
#
#   dots <- list(...)
#   str(dots)
#   browser()
#   panel.xyplot(x,y,...,groups=groups,subscripts=subscripts)
#   panel.text(x,y,groups, cex=0.65)
# }
       , type="n"
       #                       , scales = list(y=list(log=10)
) + glayer(str(list(...))) + glayer(ltext(x,y,group.value, cex=0.65))


+layer(ltext(x,y,labels=groups), groups=groups)

?panel.text

library(latticeExtra)
help(package="latticeExtra")

#StormEvents_details <- rbindlist(StormEvents_details)

# best[, c('SEASON', 'USA_SSHS'):=lapply(.SD, drop_units), .SDcols =  c('SEASON', 'USA_SSHS')]

# best_tracks_dt ----------------------------------------------------------

## add hurricane category  -------------------------------------------------

hrcn_cat <- get_hrcn_cat()
best_tracks[hrcn_cat, AVERAGE_RADIUS_OF_HURRICANE_OR_TROPICAL_STORM_FORCE_WINDS:=set_units(i.AVERAGE_RADIUS_OF_HURRICANE_OR_TROPICAL_STORM_FORCE_WINDS, "nmile"), on = 'USA_SSHS']


# best_tracks_sf GOOD -------------------------------------------------------------

devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE); (best_tracks_sf <- get_best_tracks_sf())
best_tracks_sf
st_crs(best_tracks_sf)
str(best_tracks_sf)


best_tracks_sf <- best_tracks_sf %>% st_set_crs(4269) %>% st_transform(st_crs(lcc))

# (best_tracks_sf_g <- best_tracks_sf %>% subset(SEASON==2005 & WMO_AGENCY=='hurdat_atl' & !is.na(USA_RMW) & USA_WIND>34 & USA_SSHS>0) %>% group_by(SID))

table(best_tracks_sf$NAME)

?st_union
?st_combine

# best_tracks_sf_g_buff GOOD ----------------------------------------------


(best_tracks_sf_buff <- best_tracks_sf %>%
   #   subset(SEASON==2005)  %>%
   {st_buffer(.,.$USA_RMW*1852)} %>% st_union(by_feature=TRUE) )

plot(best_tracks_sf_g_buff['SID'])

# best_tracks_sf_g_buff
#
# debugonce(setkeyv);setDT(best_tracks_sf, key=c('SID','ISO_TIME'))
# str(best_tracks_sf)
# key(best_tracks_sf)
#
# best_tracks_sf[ , geometry:=st_set_crs(geometry, 4269)]
# st_crs(best_tracks_sf$geometry)
# best_tracks_sf <- best_tracks_sf[BASIN=="NA"]
# best_tracks_sf <- best_tracks_sf[!st_is_empty(geometry)]
# best_tracks_sf[ , geometry:=st_transform(geometry, st_crs(lcc))]
# best_tracks_sf[ , geometry:=st_buffer(geometry, fcoalesce(USA_RMW,0L)*1852)]
# ?st_combine
# best_tracks_sf[, st_crs(geometry)]
# ?data.table::shift
# best_tracks_sf[SID=="2017106N36310",geometry] %>% plot()
#
# ?st_union
# (best_tracks_agg_sf <- best_tracks_sf[
# #  NAME=="KATRINA" &
#     SEASON==2017, .(buffer_geometry = st_combine(st_union(geometry))), keyby=c('SID')] )
#
# plot.new()
# best_tracks_agg_sf[1, plot(buffer_geometry)]
# best_tracks_agg_sf[-1, plot(buffer_geometry,add=TRUE)]
#
# best_tracks_sf[!st_is_empty(geometry), st_crs(geometry)]
# best_tracks_sf[NAME=="KATRINA" & SEASON==2005, c('SID', 'ISO_TIME', 'geometry')]

## add hurricane category  -------------------------------------------------


hrcn_cat <- get_hrcn_cat()


Kt_best_tracks_sf <- best_tracks_sf %>% subset(NAME=="KATRINA" & SEASON==2005 & !is.na(WMO_WIND))
Kt_best_tracks_sf['WMO_WIND'] %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)
st_crs(Kt_best_tracks_sf)

(Kt_best_tracks_sf <- Kt_best_tracks_sf %>% left_join(hrcn_cat,by='USA_SSHS') %>%
    mutate(AVERAGE_RADIUS_OF_HURRICANE_OR_TROPICAL_STORM_FORCE_WINDS:=set_units(AVERAGE_RADIUS_OF_HURRICANE_OR_TROPICAL_STORM_FORCE_WINDS, "nmile")) %>%
    dplyr::filter(!is.na(AVERAGE_RADIUS_OF_HURRICANE_OR_TROPICAL_STORM_FORCE_WINDS))
) %>% dplyr::select(USA_RMW,AVERAGE_RADIUS_OF_HURRICANE_OR_TROPICAL_STORM_FORCE_WINDS)

st_crs(lcc)
Kt_best_tracks_sf %>% dplyr::select(USA_RMW)
Kt_best_tracks_lcc_sf <- Kt_best_tracks_sf  %>% st_transform(st_crs(lcc))
Kt_best_tracks_lcc_sf
drop_units(set_units(1, "nmile") %>% set_units("m"))

debugonce(st_buffer)
(Kt_best_tracks_lcc_buff_sf <- Kt_best_tracks_lcc_sf %>% dplyr::filter(!is.na(USA_RMW)) %>%
    {st_buffer(.,dist = .$USA_RMW*1852)} )

(Kt_best_tracks_lcc_buff_g <- st_geometry(Kt_best_tracks_lcc_buff_sf))

setDT(Kt_best_tracks_lcc_buff_g, key ='ISO_TIME')

Kt_best_tracks_lcc_buff_g %>% st_combine() %>% st_convex_hull() %>% plot()

Kt_best_tracks_lcc_buff_sf['WMO_WIND'] %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)

# east coast counties  ----------------------------------------------------

best_tracks_sf_g_buff['SID']  %>% st_area()
best_tracks_sf_g_buff[500,c('SID','NAME', 'USA_RMW')]

View(best_tracks_sf%>% subset(NAME=="TAMMY" & SEASON==2005))

?plot.sf
(counties_east_coast_sf <- counties(state = c('LA','MS', 'AL','GA','FL'), cb=TRUE, keep_zipped_shapefile =TRUE) %>%
    st_transform(st_crs(lcc)))

counties_east_coast_sf %>% st_bbox()


# counties_east_coast_best_tracks_png -----------------------------------------


counties_east_coast_sf%>%
  st_geometry() %>%
  plot(axes=TRUE, graticule=TRUE, reset=FALSE)
plot(best_tracks_sf_buff['NAME'] # %>% subset(NAME=="KATRINA")
     , key.pos=2
     , add=TRUE, pal=sf.colors(n=length(unique(best_tracks_sf_buff$SID)),alpha=0.5, categorical = TRUE))

table(best_tracks_sf_g_buff$SEASON)

#
#' using data.table --------------------------------------------------------
# Kt_best_tracks_lcc_buff_sf[, ISO_TIME:=as.POSIXct(ISO_TIME)]
# debugonce(setDT)
# setDT(Kt_best_tracks_lcc_buff_sf, key = C('SID','ISO_TIME'))
# debugonce(setkeyv)
# ?setkeyv
# setkeyv(Kt_best_tracks_lcc_buff_sf, cols =  C('SID','ISO_TIME'), verbose = TRUE)
# str(Kt_best_tracks_lcc_buff_sf)
#
#
# (colnames <- names(best))
# dput(colnames, file = "global_best.R")
# getwd()
# Hurricane Category ------------------------------------------------------

names(best)
nri[, .(HRCN_AFREQ, HRCN_EVNTS, HRCN_EXP_AREA)]
print(best[, table(USA_SSHS)])
best[, .(USA_SSHS, USA_RMW)]
?units
# best[, AVERAGE_RADIUS_OF_HURRICANE_OR_TROPICAL_STORM_FORCE_WINDS_MILES:=NULL]


best[BASIN=="NA" & USA_SSHS ==5, .(SID,SEASON, NAME, USA_SSHS, USA_RMW,AVERAGE_RADIUS_OF_HURRICANE_OR_TROPICAL_STORM_FORCE_WINDS_MILES)]
best[BASIN=="NA"  & SEASON==2005 & USA_SSHS ==5,  .(SID,SEASON, NAME, USA_SSHS
                                                    , USA_RMW = set_units(USA_RMW, "nmile")
                                                    , AVERAGE_RADIUS_OF_HURRICANE_OR_TROPICAL_STORM_FORCE_WINDS=set_units(AVERAGE_RADIUS_OF_HURRICANE_OR_TROPICAL_STORM_FORCE_WINDS, "nmile"))]
# fips_state_table --------------------------------------------------------

fips_state_table <- tigris:::fips_state_table
save(fips_state_table, file=file.path(system.file("R", package="femar"),"sysdata.Rda"))

# NRIDataDictionary.csv ---------------------------------------------------
NRIDataDictionary_xlsx <- file.path(.NRI_datadir, "NRIDataDictionary.xlsx")
browseURL(NRIDataDictionary_xlsx)

NRIDataDictionary <- fread(file.path(.NRI_datadir, "NRIDataDictionary.csv"))
NRIDataDictionary


# NRI_states_sf ------------------------------------------------------------------
NRI_GDB_states_gdb <- file.path(.NRI_datadir, "NRI_GDB_States.gdb"); stopifnot(dir.exists(NRI_GDB_states_gdb))
st_layers(NRI_GDB_states_gdb)

NRI_states_sf <- st_read(NRI_GDB_states_gdb, layer = "NRI_States")
NRI_states_sf

# NRI_HazardInfo ----------------------------------------------------------
devtools::load_all("~/Spatial/FEMA/femar"); debugonce(get_NRI_HazardInfo); hur_info <- get_NRI_HazardInfo()
str(hur_info)


# NRI_states_conus_sf <- subset(NRI_states_sf, ! STATEABBRV %in% states_noconus)

# NRI_conus_png ----

library(grid)

plot(NRI_states_conus_sf['EAL_SCORE'])

NRI_conus_png <- file.path(NRI_WORKDIR, format(Sys.time(),"NRI_conus_%Y%m%d_%H%M.png")); print(file.info(NRI_conus_png))
library(Cairo)
Cairo::CairoPNG(filename = NRI_conus_png, width = 10.0, height = 6.0, dpi=300, units="in")
plot(NRI_states_conus_sf['EAL_SCORE'])
dev.off()
print(file.info(NRI_conus_png))
browseURL(dirname(NRI_conus_png))

# NRI_counties_sf ------------------------------------------------------------------
devtools::load_all("~/Spatial/FEMA/femar"); debugonce(get_NRI_GDB_counties_sf); NRI_counties_sf <- get_NRI_counties_sf('TX')
str(NRI_counties_sf)

# NRI_counties --------------------------------------------------------

devtools::load_all("~/Spatial/FEMA/femar"); debugonce(get_NRI_counties); NRI_counties <- get_NRI_counties()
NRI_counties[, .(STATEFIPS         ,COUNTYFIPS,COUNTYTYPE    ,BUILDVALUE,AREA,EAL_VALB,ALR_VALB,Shape_Area)]

library(sf)
?merge.sf
?base::merge
merge(counties_sf, NRI_counties, by=)

# NRI_hrcn_counties -------------------------------------------------------

sel <- c(1:which(names(NRI_counties)=="CRF_VALUE"),grep("^HRCN",names(NRI_counties)))

NRI_counties_hrcn <- NRI_counties[, ..sel]

# NRI_counties_conus_png ----
NRI_counties_conus_sf <- subset(NRI_counties_sf, ! STATEABBRV %in% states_noconus)
NRI_counties_conus_png <- file.path(NRI_WORKDIR, format(Sys.time(),"NRI_counties_conus_%Y%m%d_%H%M.png")); print(file.info(NRI_counties_conus_png))
library(Cairo)
Cairo::CairoPNG(filename = NRI_counties_conus_png, width = 10.0, height = 6.0, dpi=300, units="in")
plot(NRI_counties_conus_sf['EAL_SCORE'])
dev.off()
print(file.info(NRI_counties_conus_png))
browseURL(dirname(NRI_counties_conus_png))

# hurricane frequencies ---------------------------------------------------
nrow(NRI_counties_hrcn)
attach(NRI_counties_hrcn)

cbind(COUNTYFIPS, HRCN_AFREQ, HRCN_EVNTS)

sum(HRCN_EVNTS, na.rm = TRUE)
detach(NRI_counties_hrcn)

# NRI_counties_tx_sf <- subset(NRI_counties_sf, STATEABBRV %in% 'TX')
# NRI_counties_tx_sf

# NRI_counties_tx_png ----

NRI_counties_tx_png <- file.path(Sys.getenv("R_WORK_DIR"), format(Sys.time(),"NRI_counties_tx_%Y%m%d_%H%M.png")); print(file.info(NRI_counties_tx_png))
library(Cairo)
Cairo::CairoPNG(filename = NRI_counties_tx_png, width = 10.0, height = 6.0, dpi=300, units="in")
plot(NRI_counties_tx_sf['EAL_SCORE'])
dev.off()
print(file.info(NRI_counties_tx_png))
browseURL(dirname(NRI_counties_tx_png))



# NRI_counties_ok_png ----
NRI_counties_ok_sf <- subset(NRI_counties_sf, STATEABBRV %in% 'OK')
NRI_counties_ok_sf


NRI_counties_ok_png <- file.path(Sys.getenv("R_WORK_DIR"), format(Sys.time(),"NRI_counties_ok_%Y%m%d_%H%M.png")); print(file.info(NRI_counties_ok_png))
library(Cairo)
Cairo::CairoPNG(filename = NRI_counties_ok_png, width = 10.0, height = 6.0, dpi=300, units="in")
plot(NRI_counties_ok_sf['EAL_SCORE'])
dev.off()
print(file.info(NRI_counties_ok_png))
browseURL(dirname(NRI_counties_ok_png))

# Hurricanes --------------------------------------------------------------
(Prefix <- NRI_HazardInfo%>% subset(Hazard=="Hurricane", select="Prefix") %>% as.character())

NRI_counties_haz1_sf <- get_NRI_counties_haz1(Prefix)
NRI_counties_haz1_sf

?natural_breaks
library(rgeoda)
undebug(rgeoda::natural_breaks)
inherits(NRI_counties_haz1_sf, "data.frame")
# col.name <- 'HRCN_EALB'; col.descr <- "Expected Annualized Loss in Building value (EALB) due to Hurricane (in $USD)"
col.name <- 'HRCN_ALRB'; col.descr <- "Annualized Loss Rate on Building value (ALRB) due to Hurricane"
(breaks <- rgeoda::natural_breaks(k = 5
                                  ,df = na.omit(NRI_counties_haz1_sf[, col.name])
)
)
print(breaks, digits = 4L)

?mtext
?terra::plot


# hrcn_alrb_png ----

hrcn_alrb_png <- file.path(Sys.getenv("R_WORK_DIR"), format(Sys.time(),"hrcn_alrb_%Y%m%d_%H%M.png")); print(file.info(hrcn_alrb_png))
library(Cairo)
Cairo::CairoPNG(filename = hrcn_alrb_png, width = 10.0, height = 6.0, dpi=300, units="in")


lo <- min(NRI_counties_haz1_sf[[col.name]], na.rm = TRUE)
hi <- max(NRI_counties_haz1_sf[[col.name]], na.rm = TRUE)
plot(vect(NRI_counties_haz1_sf), col.name, breaks=c(lo,breaks,hi)
     , main=paste("Texas", col.descr)
     , plg=list(title=col.name)
)
mtext(side = 1, text = "Source: FEMA National Risk Index", adj=0.25, xpd=FALSE)

dev.off()
print(file.info(hrcn_alrb_png))
browseURL(dirname(hrcn_alrb_png))

# tornadoes ---------------------------------------------------------------
(Prefix <- NRI_HazardInfo%>% subset(Hazard=="Tornado", select="Prefix") %>% as.character())

get_NRI_counties_haz1 <- function(Prefix) {
  select <- c(1:grep("CRF_VALUE", names(NRI_counties_sf))
              ,grep(paste0("^",Prefix), names(NRI_counties_sf), value = FALSE)
              ,grep("Shape", names(NRI_counties_sf))
  )

  NRI_counties_haz1_sf <- subset(NRI_counties_sf , select=select )
  print(names(NRI_counties_haz1_sf))
  NRI_counties_haz1_sf
}

NRI_counties_ok_tornadoes_sf <- subset(NRI_counties_sf, subset=STATEABBRV %in% 'OK'
                                       , select=c(1:grep("CRF_VALUE", names(NRI_counties_sf)), grep("TRND", names(NRI_counties_sf)), grep("Shape", names(NRI_counties_sf)))
)

NRI_counties_ok_tornadoes_sf

NRI_counties_ok_tornadoes_dt <- st_drop_geometry(NRI_counties_ok_tornadoes_sf) %>% setDT() %>% subset(select=county_building_cols)

(county_building_cols <- grep("^(COUNTY|EAL_VALB|TRND)", names(NRI_counties_ok_tornadoes_sf), value = TRUE) %>%
    grep(pattern="(EXPP|EXPPE|EXPA|EXPT|HLRP|HLRA|EALP|EALPE|EALA|EALT|ALRP|ALRA)", invert = TRUE, value = TRUE))

NRI_counties_ok_tornadoes_dt[,  TRND_EVNTS_PER_EXP_AREA:=TRND_EVNTS/TRND_EXP_AREA]

NRI_counties_ok_tornadoes_dt[, .(TRND_AFREQ,12/TRND_EVNTS)]
