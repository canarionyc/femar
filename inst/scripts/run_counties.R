
# setup -------------------------------------------------------------------
# rm(list = ls())
source("~/Spatial/.RProfile")
library(configr)
configr::read.config()
devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)
print(getOption("tigris_year"))
# source("~/lattice_setup.R")


# counties_sf -------------------------------------------------------------



counties_sf <- get_counties_sf()
counties_sf

counties_dt <- st_drop_geometry(counties_sf) %>% setDT(key='GEOID')

# verify area -------------------------------------------------------------

(counties_stats <- counties_dt[, .(GEOID_COUNT=uniqueN(GEOID), ALAND20 =sum(  ALAND ), AWATER=sum(AWATER))]) %>% as_tibble()

?load_tiger
(counties_sf <- tigris::counties(cb=FALSE,
                                 keep_zipped_shapefile = TRUE ))

fips_codes

library(tmap)
?tmap_mode
tmap_mode("view")
?tm_view
debugonce(tmap_mode)
counties_sf %>% subset(STATEFP=='06') %>% qtm()
# qtm(counties_sf)



(counties_lcc_sf <- counties_sf %>% st_transform(st_crs(lcc)))

?st_buffer
(counties_lcc_buff_sf <- st_buffer(counties_lcc_sf, dist=dist))

# coastal counties --------------------------------------------------------

plot(counties_lcc_buff_sf['NAME'])

(counties.coastline_lst <- st_intersects(counties_sf, coastline_sf))

(coastal_counties_sf <- counties_sf[lengths(counties.coastline_lst)>0,])

# coastal counties buffered -----------------------------------------------


## Lambers Conical Confocal ------------------------------------------------


(counties.coastline_buff_lst <- st_intersects(counties_lcc_buff_sf, coastline_lcc_sf))

(coastal_counties_lcc_sf  <- counties_lcc_sf %>% subset(lengths(counties.coastline_buff_lst)>0)) %>%
  subset(subset=STATEFP!="02" & STATEFP !="15" & STATEFP<60) %>%
  st_geometry() %>% plot()

library(EnvStats)
?dtri
NRI_counties_dt[, 1:30]

# Community Resilience Factor ---------------------------------------------
NRI_counties_dt[, .(STCOFIPS, SOVI_SPCTL,CRF_VALUE,  SOVI_SCORE, RESL_VALUE, CRF_VALUE_CALC=SOVI_SCORE/RESL_VALUE
                , EnvStats::dtri(SOVI_SCORE/RESL_VALUE, min = 0.5, max = 2, mode = 1))]

NRI_counties_dt[, plot(stats::density(CRF_VALUE, na.rm=TRUE))]
curve(dtri(x,min=0.5,max=2,mode = 1), add=TRUE, lty=2)

NRI_counties_dt[, plot(CRF_VALUE~SOVI_SCORE)]

NRI_counties_dt[, plot(x=SOVI_SCORE,y=CRF_VALUE)]

?order
xyplot(CRF_VALUE~SOVI_SCORE|STATEFIPS, type="r", data = NRI_counties_dt[order(STATEFIPS,SOVI_SCORE)])

xyplot(CRF_VALUE~SOVI_SCORE/RESL_VALUE|STATEFIPS, type="r", data = NRI_counties_dt[order(STATEFIPS,SOVI_SCORE)])

## RISK_SCORE.cty.shingle --------------------------------------------------

(risk_intervals.cty <- NRI_counties_dt[, .(RISK_RATNG_COUNT=.N,MIN_RISK_SCORE=min(RISK_SCORE, na.rm = TRUE), MAX_RISK_SCORE=max(RISK_SCORE, na.rm = TRUE)), keyby=.(RISK_RATNG)])
(risk_intervals.cty.mat <- as.matrix(na.omit(risk_intervals.cty[, .(MIN_RISK_SCORE, MAX_RISK_SCORE)])))
library(lattice)
?shingle
RISK_SCORE.cty.shingle <- shingle(NRI_counties_dt$RISK_SCORE, intervals = risk_intervals.cty.mat)
plot(RISK_SCORE.cty.shingle, main="NRI RISK rating vs score at County level", xlab = 'RISK_SCORE', ylab='RISK_RATNG')



# Annualized Loss Rate ----------------------------------------------------

# bwplot(EAL_VALT~STCOFIPS, horizontal = TRUE)

NRI_counties_dt[, .(STCOFIPS, ALR_VALB, EAL_VALB/BUILDVALUE)]# good
NRI_counties_dt[, .(STCOFIPS, ALR_VALP, EAL_VALP/POPULATION)]# good
NRI_counties_dt[, .(STCOFIPS, ALR_VALA, EAL_VALA/AGRIVALUE)]# good
NRI_counties_dt[, .(STCOFIPS, EAL_VALT, EAL_VALB+EAL_VALPE+EAL_VALA)] # good

NRI_counties_dt[, .(STCOFIPS,RISK_VALUE, EAL_VALT, RISK_VALUE, RISK_VALUE_CALC=EAL_VALT* CRF_VALUE)] # good
NRI_counties_dt[, .(STCOFIPS,CRF_VALUE, SOVI_SCORE, RESL_VALUE, qtri(1, min=0.5,max=2,mode = 1))]
NRI_counties_dt[, .(min(CRF_VALUE, na.rm = TRUE),   max(CRF_VALUE, na.rm = TRUE))]

?stats::density
NRI_counties_dt[, plot(stats::density(CRF_VALUE, na.rm=TRUE))]
curve(dtri(x,min=0.5,max=2,mode = 1), add=TRUE, lty=2)


NRI_counties_dt[, .(min(SOVI_SCORE, na.rm = TRUE),   max(SOVI_SCORE, na.rm = TRUE))]
NRI_counties_dt[, plot(stats::density(SOVI_SCORE, na.rm=TRUE, cut=0))]


# Resilience --------------------------------------------------------------

NRI_counties_dt[, plot(CRF_VALUE~RESL_VALUE)]
NRI_counties_dt[, plot(stats::density(RESL_VALUE, na.rm=TRUE, cut=0))]



NRI_counties_dt[, EAL_VALT_rank:=rank(EAL_VALT)]


NRI_counties_dt[, .(STCOFIPS,EAL_VALT, EAL_VALT_rank,EAL_SCORE_CALC=100*(EAL_VALT_rank-min(EAL_VALT_rank))/(max(EAL_VALT_rank)-min(EAL_VALT_rank)), EAL_SCORE)][order(EAL_SCORE)]
# approximately good
NRI_counties_dt[ ,.(STCOFIPS,HRCN_EALB,  HRCN_AFREQ, AREA,HRCN_EXP_AREA, HRCN_HLRB, BUILDVALUE*HRCN_AFREQ* HRCN_HLRB )]


NRI_counties_dt[, .(STCOFIPS, RISK_VALUE, RISK_SCORE, RISK_RATNG)]


# correlations ------------------------------------------------------------

(value_cols <- grep("VALUE$", names(NRI_counties_dt), value = TRUE))
NRI_counties_dt[, .SD,.SDcols = c('STATEFIPS', 'EAL_VALT',value_cols)]

NRI_counties_dt[, .(SOVI_SCORE,  RISK_VALUE,EAL_VALT*CRF_VALUE/RESL_VALUE )]

(counties_num_cols <- which(sapply(NRI_counties_dt, is.numeric)==TRUE))


# NRI_counties_sf -------------------------------------------------------------

(NRI_counties_sf <- get_NRI_counties_sf())


(tx_sf <- NRI_counties_sf %>% subset(STATEFIPS!='02' & STATEFIPS!='15' & STATEFIPS<60 & STATEFIPS=='48', 'ALR_VALB') %>%
  mutate(ALR_VALB=1e6*ALR_VALB) ) %>%
  plot(axes=TRUE, graticule=TRUE, reset=FALSE)

(tx_vect <- terra::vect(tx_sf))

dev.off()




# tx_alr_valb_png ----

tx_alr_valb_png <- file.path(.census_workdir, format(Sys.time(),"tx_alr_valb_%Y%m%d_%H%M.png")); print(file.info(tx_alr_valb_png))
library(Cairo)
Cairo::CairoPNG(filename = tx_alr_valb_png, width = 10.0, height = 6.0, dpi=300, units="in")
terra::plot(tx_vect, 'ALR_VALB', main="Annualized Loss in Building Value $ Amt per $1M", alpha=0.5)
dev.off()
print(file.info(tx_alr_valb_png))
browseURL(dirname(tx_alr_valb_png))

# NRI_counties in a FEMA Region ------------------------------------------------------

library(tigris)
?load_tiger
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE); (NRI_counties_RegIV_sf <-get_NRI_counties_sf() %>%
                                                                              subset(STATEABBRV %in% states.RegionIV))

levels(NRI_counties_RegIV_sf$RISK_RATNG)
# NRI_counties_sf$RISK_RATNG<- factor(NRI_counties_sf$RISK_RATNG
#                                   , levels = c("Very High","Relatively High" ,"Relatively Moderate", "Relatively Low"  ,"Very Low"))


(NRI_counties_RegIV_vect <- terra::vect(NRI_counties_RegIV_sf))
?terra::plot
terra::plot(NRI_counties_RegIV_vect, 'RISK_RATNG'
            , sort=c("Very High", "Relatively High", "Relatively Moderate", "Relatively Low",                                                          "Very Low")
            #            , sort=FALSE
            , border=NULL, alpha=0.5
            , main="FEMA Region IV Risk Rating by Census Tract")


(counties_sf <- tigris::counties(cb=TRUE))

(counties_lcc_sf <- counties_sf %>% st_transform(st_crs(lcc)))

?st_buffer
(counties_lcc_buff_sf <- st_buffer(counties_lcc_sf, dist=dist))

# coastal counties --------------------------------------------------------


plot(counties_lcc_buff_sf['NAME'])

(counties.coastline_lst <- st_intersects(counties_sf, coastline_sf))

(coastal_counties_sf <- counties_sf[lengths(counties.coastline_lst)>0,])

# coastal counties buffered -----------------------------------------------

(counties.coastline_buff_lst <- st_intersects(counties_lcc_buff_sf, coastline_lcc_sf))

(coastal_counties_lcc_sf  <- counties_lcc_sf %>% subset(lengths(counties.coastline_buff_lst)>0)) %>%
  subset(subset=STATEFP!="02" & STATEFP !="15" & STATEFP<60) %>%
  st_geometry() %>% plot()
