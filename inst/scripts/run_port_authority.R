
# setup -------------------------------------------------------------------
devtools::load_all("~/fstutils/")

library(xts);
library(xtsExtra)
# library(lattice)

list.files(path="E:\\Datasets\\ORG\\IMF\\",   full.names=TRUE )
# port_valencia_import_export ---------------------------------------------


(port_valencia_import_vol <- fread("E:\\Datasets\\ORG\\IMF\\port-valencia-import-vol.csv") %>% fstutils::sanitize() %>% setkeyv('DATETIME'))
str(port_valencia_import_vol)

port_valencia_import_vol[, DAILY_IMPORT_VOLUME_INC:=diff]

port_valencia_import_vol
port_valencia_export_vol <- fread("E:\\Datasets\\ORG\\IMF\\port-valencia-export-vol.csv")%>% fstutils::sanitize() %>% setkeyv('DATETIME')
str(port_valencia_export_vol)

?xts::merge.xts
port_valencia_import_export <- merge(port_valencia_import_vol,port_valencia_export_vol
#                                     , suffixes = c("_IMPORT","_EXPORT")

                                     )
port_valencia_import_export

port_valencia_import_export.ts <- as.xts(port_valencia_import_export)

diff(port_valencia_import_vol.ts)

apply.monthly(port_valencia_import_export.ts[, c('DAILY_IMPORT_VOLUME')], colMeans) %>% plot()
apply.monthly(port_valencia_import_export.ts[, c('DAILY_EXPORT_VOLUME')], colMeans) %>% plot()

apply.monthly(port_valencia_import_export.ts[, c('DAILY_IMPORT_VOLUME','DAILY_EXPORT_VOLUME')], colMeans) %>% plot(main="Puerto de Valencia, monthly average trade volume"
                                                                                                                   ,ylab="metric tons"
#                                                                                                                   ,legend.loc = "topright"
                                                                                                                   )
addLegend(legend.loc = "topright",
          legend.names = c("imports","exports"), col=1:2, lty=1, lwd=2)


# valencia_pa_png ----

valencia_pa_png <- file.path(eval(str2expression(FEMA_WORKDIR)), format(Sys.time(),"valencia_pa_%Y%m%d_%H%M.png")); print(file.info(valencia_pa_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = valencia_pa_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(valencia_pa_png)['size'])
browseURL(dirname(valencia_pa_png))

?diff.xts
library(xts)
?plot.xts
plot(as.xts(port_valencia_import_export)[,c('DAILY_IMPORT_VOLUME','DAILY_EXPORT_VOLUME')], type="p", log=FALSE)
plot(diff(as.xts(port_valencia_import_export))[,c('DAILY_IMPORT_VOLUME','DAILY_EXPORT_VOLUME')], type="p", log=FALSE, main="Puerto de Valencia")
plot(as.xts(port_valencia_import_export)[,c('X7_DAY_MOVING_AVERAGE.x','X7_DAY_MOVING_AVERAGE.y')], type="p", log=FALSE)


xyplot(DAILY_IMPORT_VOLUME +DAILY_EXPORT_VOLUME ~DATETIME ,port_valencia_import_export)
xyplot(X7_DAY_MOVING_AVERAGE.x +X7_DAY_MOVING_AVERAGE.y ~DATETIME ,port_valencia_import_export
       , type="smooth", auto.key = TRUE)


# panama-canal ------------------------------------------------------------

(panama_canal_daily_trans <- fread("E:\\Datasets\\ORG\\IMF\\panama-canal-daily-trans.csv"  )%>% fstutils::sanitize() %>% setkeyv('DATETIME') )
panama_canal_daily_trans.ts <- as.xts(panama_canal_daily_trans)

xts::plot.xts(panama_canal_daily_trans.ts[, c('X7_DAY_MOVING_AVERAGE', 'PRIOR_YEAR_7_DAY_MOVING_AVERAGE')], multi.panel=FALSE)

(panama_canal_transit_tra.ts <- fread("E:\\Datasets\\ORG\\IMF\\panama-canal-transit-tra.csv"  )%>%
    fstutils::sanitize() %>%
    setkeyv('DATETIME') %>% as.xts())

xts::plot.xts(panama_canal_transit_tra.ts[, c('X7_DAY_MOVING_AVERAGE', 'PRIOR_YEAR_7_DAY_MOVING_AVERAGE')], multi.panel=FALSE)

# Gatun-water-level -------------------------------------------------------

(Gatun_water_level.ts <- fread("E:\\Datasets\\ORG\\IMF\\Download_Gatun_Lake_Water_Level_History.csv") %>% sanitize() %>% as.xts())
?plot.xts
Gatun_water_level.ts %>% xts::plot.xts(subset="2019-01-01/2024-11-28", col="blue")

(Gatun_water_level_monthly_ave.ts <- apply.monthly(Gatun_water_level.ts, colMeans))

?merge.xts
pana_gatun.ts <- merge(panama_canal_daily_trans.ts[,c('NUMBER_OF_CARGO_SHIPS', 'NUMBER_OF_TANKER_SHIPS')],Gatun_water_level.ts, all = FALSE)
pana_gatun.ts

?apply.monthly
pana_gatun_monthly_ave.ts <- apply.monthly(pana_gatun.ts, colMeans)

?period.apply
# to.monthly(pana_gatun.ts, indexAt="yearmon")
pana_gatun_monthly_ave.ts

?yearmon
index(pana_gatun_monthly_ave.ts) <-  as.yearmon(index(pana_gatun_monthly_ave.ts))
plot(pana_gatun_monthly_ave.ts)

?merge.xts
index(SOI.ts)<- as.yearmon(index(SOI.ts))
combined.ts <- merge(pana_gatun_monthly_ave.ts, SOI.ts, all=FALSE)
combined.ts
index(combined.ts)

?subset.xts
combined.ts['Nov 2024']
subset(combined.ts, '2000-01/02')
combined.ts

combined.ts[, c('NUMBER_OF_CARGO_SHIPS', 'NUMBER_OF_TANKER_SHIPS')] %>% xts::plot.xts(multi.panel=FALSE
                                                                                      , main = "Panama Canal average monthly transit"
                                                                                      ,ylab="vessels/day"
                                                                                      ,legend.loc = "left")
lines(combined.ts[, 'GATUN_LAKE_LEVEL.FEET.'], col="blue", lwd=2,on=NA, main='GATUN_LAKE_LEVEL (FEET)')
lines(combined.ts[, 'SOI'], col="green", lwd=2,on=NA, main="SOI")


# pana_png ----

pana_png <- file.path(eval(str2expression(FEMA_WORKDIR)), format(Sys.time(),"pana_%Y%m%d_%H%M.png")); print(file.info(pana_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = pana_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(pana_png)['size'])
browseURL(dirname(pana_png))

# -------------------------------------------------------------------------


help(package="xtsExtra")
?plot.xts
xts::plot.xts(pana_gatun.ts[, c('X7_DAY_MOVING_AVERAGE', 'PRIOR_YEAR_7_DAY_MOVING_AVERAGE')]
              , observation.based = TRUE, lty=1, lwd=c(2,1)
              , main="Panama Canal daily transit"
              , format.labels = "%b-%Y"
              ,legend.loc="bottom")

# xts::plot.xts(pana_gatun.ts[, c('NUMBER_OF_CARGO_SHIPS', 'NUMBER_OF_TANKER_SHIPS')], type = "p")
lines(pana_gatun.ts[,  'GATUN_LAKE_LEVEL.FEET.'], on=NA, col="blue", lwd=2)
