# setup -------------------------------------------------------------------
# rm(list = ls())
source("~/Spatial/.RProfile")
library(configr)
configr::read.config()
devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)
# source("~/Spatial/Tigris/tigris_setup.R")

?units_options
?valid_udunits
if(interactive())
  View(valid_udunits())
(val_ud.df <- valid_udunits()) %>% clipr::write_clip(object_type = "table")
print(valid_udunits(), n=276L)
units::valid_udunits_prefixes()
my_area <- set_units(1, "mile^2")
set_units(my_area, "km^2")
stop()
browseURL(.NRI_datadir)
list.files(.NRI_datadir, full.names = TRUE)

browseURL(.NRI_workdir)

# NRIDataDictionary.csv ---------------------------------------------------
NRIDataDictionary_xlsx <- system.file("extdata","FEMA","NRI",  "NRIDataDictionary.xlsx", package="femar",mustWork = TRUE)
browseURL(NRIDataDictionary_xlsx)

NRIDataDictionary <- fread(file.path(.NRI_datadir, "NRIDataDictionary.csv"))
NRIDataDictionary


# NRI_counties --------------------------------------------------------

devtools::load_all("~/Spatial/FEMA/femar"); debugonce(get_NRI_counties_dt); NRI_counties <- get_NRI_counties_dt()
NRI_counties[, .(STATEFIPS         ,COUNTYFIPS,COUNTYTYPE    ,BUILDVALUE,AREA,EAL_VALB,ALR_VALB,Shape_Area
             ,HRCN_EVNTS
             ,HRCN_AFREQ
             ,HRCN_EXPB

             ,HRCN_EXP_AREA
             ,HRCN_HLRB

             ,HRCN_EALB


             ,HRCN_ALRB

)]



library(sf)
?merge.sf
?base::merge
merge(counties_sf, NRI_counties, by=)

# hrcn_cat ----------------------------------------------------------------



hrcn_cat <- get_hrcn_cat()




# east coast counties  ----------------------------------------------------

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


nri[, .(HRCN_AFREQ, HRCN_EVNTS, HRCN_EXP_AREA)]
  print(best[, table(USA_SSHS)])
best[, .(USA_SSHS, USA_RMW)]
?units

fips_state_table <- tigris:::fips_state_table
save(fips_state_table, file=file.path(system.file("R", package="femar"),"sysdata.Rda"))



# NRI_HazardInfo ----------------------------------------------------------
devtools::load_all("~/Spatial/FEMA/femar"); debugonce(get_NRI_HazardInfo); hur_info <- get_NRI_HazardInfo()
str(hur_info)


# NRI_states_conus_sf <- subset(NRI_states_sf, ! STATEABBRV %in% states_noconus)

## NRI_conus_png ----

library(grid)

plot(NRI_states_conus_sf['EAL_SCORE'])

NRI_conus_png <- file.path(.NRI_workdir, format(Sys.time(),"NRI_conus_%Y%m%d_%H%M.png")); print(file.info(NRI_conus_png))
library(Cairo)
Cairo::CairoPNG(filename = NRI_conus_png, width = 10.0, height = 6.0, dpi=300, units="in")
plot(NRI_states_conus_sf['EAL_SCORE'])
dev.off()
print(file.info(NRI_conus_png))
browseURL(dirname(NRI_conus_png))

# NRI_counties_sf ------------------------------------------------------------------
devtools::load_all("~/Spatial/FEMA/femar"); debugonce(get_NRI_GDB_counties_sf); NRI_counties_sf <- get_NRI_counties_sf('TX')
str(NRI_counties_sf)


## NRI_hrcn_counties -------------------------------------------------------

sel <- c(1:which(names(NRI_counties)=="CRF_VALUE"),grep("^HRCN",names(NRI_counties)))

NRI_counties_hrcn <- NRI_counties[, ..sel]

## NRI_counties_conus_png ----
NRI_counties_conus_sf <- subset(NRI_counties_sf, ! STATEABBRV %in% states_noconus)
NRI_counties_conus_png <- file.path(.NRI_workdir, format(Sys.time(),"NRI_counties_conus_%Y%m%d_%H%M.png")); print(file.info(NRI_counties_conus_png))
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


# NRI tracts --------------------------------------------------------------

library(tmap)
?tm_fill
NRI_tracts_sf %>% subset(STATEFIPS  =='06') %>% tm_shape()+ tm_fill(fill='SOVI_SCORE', col=NA)
