
# setup -------------------------------------------------------------------
devtools::load_all("~/fstutils/")

library(lattice)


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
