
download.file()
library(rjson)
browseURL(.fema_datadir)

FEMARegions_json <- file.path(.fema_datadir, "FEMARegions.json")
download.file("https://www.fema.gov/api/open/v2/FemaRegions.json", destfile =  FEMARegions_json)

help(package="geojson")
help(package="geojsonio")
library(geojson)

FEMARegions <- geojsonio::geojson_read(FEMARegions_json)
