
# setup -------------------------------------------------------------------
# rm(list = ls())
source("~/Spatial/.RProfile")
library(configr)
configr::read.config()
devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)
print(getOption("tigris_year"))
# source("~/lattice_setup.R")

list.files(.DOE_datadir, full.names = TRUE, recursive = TRUE)



#+ ClimateZones_sf -----------------------------------------------------------------
?get_ClimateZones_sf
(ClimateZones_sf <- get_ClimateZones_sf())
st_crs(ClimateZones_sf)

ClimateZones_sf

# https://basc.pnnl.gov/guide-determining-climate-zone-county-data-files

# file.path(.EIA_datadir, "Climate_Zones_-_DOE_Building_America_Program","Climate_Zones_-_DOE_Building_America_Program.shp")

# (ClimateZones_sf <- st_read("E:\\Datasets\\DOE\\ClimateZoneDataFiles\\ClimateZones.shp"))

unique(ClimateZones_sf$State)


# (ClimateZones_vect <- terra::vect(ClimateZones_sf %>% subset(!is.na(State) & ! State %in% c("Alaska", "Hawaii" ))))
# terra::plot(ClimateZones_vect)
# unique(ClimateZones_vect$State)

# ClimateZones_png ----

# ?plot.sf
?colour_ramp
alpha.f <- 0.75
rgb(0,0,1,alpha.f) # blue

ramp <-colorRamp(c( "blue","red"))
rgb(ramp(seq(0, 1, length = 12)), maxColorValue = 255)

library(scales)
?colour_ramp
ramp2 <- colour_ramp(c("blue", "green", "red"))
ramp2(seq(0, 1, length = 12)) %>% adjustcolor(alpha.f = 0.3)

ClimateZones_png <- file.path(the$FEMA_WORKDIR, format(Sys.time(),"ClimateZones_%Y%m%d_%H%M.png")); print(file.info(ClimateZones_png))
library(Cairo)
Cairo::CairoPNG(filename = ClimateZones_png, width = 10.0, height = 6.0, dpi=300, units="in")
opar <- par(mar=c(5.1,4.1,4.1,2.1))
ClimateZones_sf %>% subset(!is.na(State) & ! State %in% c("Alaska", "Hawaii" ) , select='BA21') %>%
  plot(axes=TRUE, graticule=TRUE, reset=FALSE
#       , pal=rgb( colorRamp(c( "blue","red"))(seq(0, 1, length.out = nlevels(ClimateZones_sf$BA21))), maxColorValue = 255)  %>% adjustcolor(alpha.f = 0.5)  #rev(heat.colors(nlevels(ClimateZones_sf$BA21)))
       , pal=ramp2(seq(0, 1, length.out = nlevels(ClimateZones_sf$BA21)))  %>% adjustcolor(alpha.f = alpha.f)  #rev(heat.colors(nlevels(ClimateZones_sf$BA21)))
              , main="IECC 2021 Climate Zones")
dev.off()
print(file.info(ClimateZones_png))
browseURL(dirname(ClimateZones_png))


# ClimateZones ------------------------------------------------------------

ClimateZones <- get_ClimateZones_dt()

recs5_rda <- file.path(.EIA_workdir, "recs5.rda"); print(file.info(recs5_rda))
if(file.exists(  recs5_rda)) {
  load(recs5_rda, verbose=TRUE)
} else {
  class(RECS5)
  methods(class =  "svyrep.design")
  str(RECS5$variables$BA_CLIMATE)
  # RECS5 <- transform(RECS5, BA_CLIMATE7=fct_collapse(BA_CLIMATE, "Subarctic/Very Cold"=c("Subarctic","Very Cold")))

  ?svyby
  (recs5_total_by_ba_climate <- svyby(~HOUSEHOLD +SQFTEST +TOTHSQFT+TOTCSQFT+ TOTSQFT_EN+TOTALBTU+SOURCE_TOTALBTU
                                      , by=~BA_CLIMATE, FUN = svytotal
                                      , design = RECS5
                                      , keep.var = TRUE
                                      , keep.names = FALSE,
                                      , verbose = TRUE))
  # recs5_total_by_ba_climate_dt <- recs5_total_by_ba_climate%>%
  #     as.data.table()


  print(recs5_total_by_ba_climate)
  # devtools::load_all("~/fstutils/", export_all = TRUE)
  recs5_eui_by_ba_climate <- svyby(~TOTALBTU+SOURCE_TOTALBTU
                                    , by=~BA_CLIMATE
                                    , FUN = svyratio, denominator=~TOTSQFT_EN
                                    , design = RECS5
                                    , keep.var = TRUE
                                    , keep.names = FALSE,
                                    , verbose = TRUE)
  print(recs5_eui_by_ba_climate)
  # recs5_eui_by_ba_climate_dt <-
  #     as.data.table(recs5_eui_by_ba_climate)

  recs5_sqft_by_ba_climate <- svyby(~TOTSQFT_EN+TOTALBTU+SOURCE_TOTALBTU
                                    , by=~BA_CLIMATE
                                    , FUN = svyratio, denominator=~HOUSEHOLD
                                     , design = RECS5
                                     , keep.var = TRUE
                                     , keep.names = FALSE,
                                     , verbose = TRUE)
  print(recs5_sqft_by_ba_climate)

  devtools::load_all("~/fstutils/", export_all = TRUE)
  recs5_by_ba_climate_dt <-recs5_by_ba_climate_dt <- purrr::reduce(list(recs5_total_by_ba_climate, recs5_sqft_by_ba_climate,recs5_eui_by_ba_climate), merge) %>%  setDT(key = 'BA_CLIMATE') %>% sanitize()
  print(recs5_by_ba_climate_dt)

  recs5_by_ba_climate_dt[BA_CLIMATE=="Subarctic"]
  str(recs5_by_ba_climate_dt)
  save(recs5_total_by_ba_climate
       ,recs5_sqft_by_ba_climate
       ,recs5_eui_by_ba_climate
       ,recs5_by_ba_climate_dt
       , file =  recs5_rda); print(file.info(recs5_rda))
}; str(recs5_total_by_ba_climate)

?round

?hist
?lattice::histogram
recs5_total_by_ba_climate

levels(recs5_eui_by_ba_climate_dt$BA_CLIMATE)
levels(droplevels(recs5_eui_by_ba_climate_dt$BA_CLIMATE))
recs5[, .(sum(HOUSEHOLD))]
detach(RECS2020)

# library(grid)
# help(package="grid")
# library(gtable)
# help(package="gtable")
# library(gridtext)
# help(package="gridtext")

library(gridExtra)
?gridExtra::grid.table
?gridExtra::tableGrob
?format
?gtable
?as.array
?format

setnames(recs5_eui_by_ba_climate_dt, function(.x) sub(pattern = "TOTALBTU/TOTSQFT_EN", replacement = "EUI",x=.x))
names(recs5_eui_by_ba_climate)
recs5_eui_by_ba_climate.grob <- recs5_eui_by_ba_climate_dt %>% format(big.mark=",", justify="right")%>% tableGrob()
class(recs5_eui_by_ba_climate.grob)

grid.newpage()
grid.draw(recs5_eui_by_ba_climate.grob)

library(survey)
?hist
class(recs5_eui_by_ba_climate)
methods(class="svyby")
?barplot.svyby
recs5_eui_by_ba_climate$`SOURCE_TOTALBTU/TOTSQFT_EN`
plot.new()
?barplot
graphics::barplot( `SOURCE_TOTALBTU/TOTSQFT_EN`~BA_CLIMATE,droplevels(recs5_eui_by_ba_climate_dt))
?lattice::barchart.formula
lattice::barchart( `SOURCE_TOTALBTU/TOTSQFT_EN`~BA_CLIMATE, data=droplevels(recs5_eui_by_ba_climate_dt)
                   ,scales=list(x=list(rot=45)))

