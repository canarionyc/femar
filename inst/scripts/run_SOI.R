
# setup -------------------------------------------------------------------

devtools::load_all("~/Spatial/FEMA/femar/")

url <- "https://www.cpc.ncep.noaa.gov/data/indices/soi"
SOI_txt <- file.path(the$NOAA_WORKDIR, "SOI.txt")
download.file(url = url, destfile = SOI_txt)


SOI_csv <- file.path(the$NOAA_DATADIR, "NCEP", "SOI.csv"); print(file.info(SOI_csv))
?fread
SOI_wide <- fread(SOI_csv, na.strings="-999.9")
head(SOI_wide)
tail(SOI_wide)


?melt.data.table
SOI <- melt(SOI_wide,id.vars='YEAR',variable.name = "MONTH",value.name = "SOI")
SOI
SOI[, DATE:=my(paste(MONTH, YEAR))]
SOI

setcolorder(SOI, c("DATE"))
SOI.ts <- as.xts(SOI[,c('DATE','SOI')])
plot(SOI.ts)
