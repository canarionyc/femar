
# setup -------------------------------------------------------------------
# rm(list = ls())
#source("~/Spatial/.RProfile")
#library(configr)
# print(configr::read.config())
library(sf); sf_use_s2(FALSE)
devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)
options(tigris_year=2020L)
print(getOption("tigris_year"))
# source("~/lattice_setup.R")

state.fips

library(maps)

# library(purrr)
# ?purrr::compose
# purrr::compose(plot,st_geometry)
# counties_sf -------------------------------------------------------------

args(get_counties_sf)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE);counties_sf <- get_counties_sf()
counties_sf
st_crs(counties_sf)

table(counties_sf$STUSPS)



dev.off()
counties_sf %>%
  # subset(STCOFIPS=="06037") %>%
  st_geometry() %>% plot(axes=TRUE, graticule=TRUE, reset=TRUE)




# plot(counties_sf[10,], add=TRUE)

# counties_dt -------------------------------------------------------------

counties_dt <- st_drop_geometry(counties_sf) %>% setDT(key='GEOID')

# LA.counties_vect ------------------------------------------------------------

(LA.counties_vect <- counties_vect(state = '22', cb = TRUE))

LA.counties_vect %>% plot()

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

# cleanup tigris ----------------------------------------------------------

rm(counties_sf)

# NRI_counties_sf -------------------------------------------------------------
args(get_NRI_counties_sf)
SW.states$STATEFP

devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE); debugonce(get_NRI_counties_sf); (NRI_counties_sf <- get_NRI_counties_sf(SW.states$STATEFP) %>% subset(STATEFIPS!='02' & STATEFIPS!='15' & STATEFIPS<60 ))
print(NRI_counties_sf)
st_crs(NRI_counties_sf)

NRI_counties_sf <- st_simplify(NRI_counties_sf)

NRI_counties_sf$perimeter_length <- NRI_counties_sf %>% st_geometry() %>% st_perimeter()

devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE); debugonce(add_coastline); NRI_counties_sf <- add_coastline(NRI_counties_sf)
print(NRI_counties_sf[order(NRI_counties_sf$distance_to_coastline),])


# ALR_VALB_png ----
plot(NRI_counties_sf['ALR_VALB'])


plot(density(NRI_counties_sf$ALR_VALB))

library(classInt)
help(package="classInt")
?classIntervals
pal1 <- c("wheat1", "red3")
library(grDevices)
?colorRampPalette
colorRampPalette(pal1)
clI <- classIntervals(NRI_counties_sf$ALR_VALB,n=5L, style = "headtails")
str(clI)

plot(clI,pal=pal1, main="ALR_VALB")

?findColours
cols <- findColours(clI, pal1)
attributes(cols)
attr(cols, "palette")
# NRI_counties_sf <- NRI_counties_sf %>% subset(POPULATION>=5e4)

# NRI_counties_sf <- NRI_counties_sf %>% subset(STATEFIPS!='02' & STATEFIPS!='15' & STATEFIPS<60 )
?plot.sf


?st_simplify
# NRI_counties_sf %>% subset(STATEABBRV=="CA") %>% st_simplify() %>% plot()

?plot.sf
plot(st_simplify(NRI_counties_sf['ALR_VALB'], preserveTopology = TRUE, dTolerance = 1e3), breaks=clI$brks, pal=colorRampPalette(pal1), main="Annualized Loss Rate in Building Value")



ALR_VALB_png <- file.path(the$FHFA_WORKDIR, format(Sys.time(),"ALR_VALB_%Y%m%d_%H%M.png")); print(file.info(ALR_VALB_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = ALR_VALB_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(ALR_VALB_png)['size'])
browseURL(dirname(ALR_VALB_png))

# NRI counties for CA -----------------------------------------------------
maps::state.fips
# NRI_counties_sf <- NRI_counties_sf %>% subset(STATEFIPS=='06' )

# NRI_counties_sf <- NRI_counties_sf %>% subset(STCOFIPS %in% c("25007", "25019", "36085", "53055"))
# plot(st_geometry(NRI_counties_sf))


library(spdep)

rn <- NRI_counties_sf$STCOFIPS

NRI_counties.coords <- st_centroid(st_geometry(NRI_counties_sf), of_largest_polygon = TRUE)
NRI_counties.nb <- spdep::poly2nb(st_geometry(NRI_counties_sf),row.names = rn)
print(NRI_counties.nb)

rn[which(card(NRI_counties.nb)==0)]

plot(st_geometry(NRI_counties_sf[which(card(NRI_counties.nb)==0),c( "STATE"  ,  "COUNTY",'STCOFIPS')]))

NRI_counties.W <- nb2listw(NRI_counties.nb, zero.policy = TRUE)
attr(NRI_counties.W, "zero.policy")

#plot(st_simplify(st_geometry(NRI_counties_sf)), reset=FALSE)
?plot.nb
plot(NRI_counties.nb, coords=NRI_counties.coords, col=2, add=FALSE)

# Global spatial association ----------------------------------------------

moran.test(NRI_counties_sf$ALR_VALB, listw=NRI_counties.W)
moran.plot(NRI_counties_sf$ALR_VALB, listw=NRI_counties.W)

# Local spatial association ----------------------------------------------

?localmoran
resI <- localmoran(NRI_counties_sf$ALR_VALB, listw=NRI_counties.W)
summary(resI)

?hotspot
Ih <- hotspot(resI, Prname = "Pr(z != E(Ii))")
Ih

# tx_sf -------------------------------------------------------------------



(tx_sf <- NRI_counties_sf %>% subset(STATEFIPS!='02' & STATEFIPS!='15' & STATEFIPS<60 & STATEFIPS=='48', 'ALR_VALB') %>%
  mutate(ALR_VALB=1e6*ALR_VALB) ) %>%
  plot(axes=TRUE, graticule=TRUE, reset=FALSE)

(tx_vect <- terra::vect(tx_sf))

dev.off()




# tx_alr_valb_png ----

tx_alr_valb_png <- file.path(the$CENSUS_WORKDIR, format(Sys.time(),"tx_alr_valb_%Y%m%d_%H%M.png")); print(file.info(tx_alr_valb_png))
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

