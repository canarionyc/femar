(STATEFP <- unique(tigris::fips_codes$state_code))

(url_tracts_zip <- paste("https://www2.census.gov/geo/tiger/TIGER2023/TRACT/tl_2023",STATEFP, "tract.zip", sep="_"))

?download.file
setwd()
lapply(url_tracts_zip[-1], function(url) tryCatch(
  {
    destfile <- file.path(Sys.getenv('TIGRIS_CACHE_DIR'),basename(url))
    if(file.exists(destfile)) return(destfile)
    download.file(url, destfile = destfile)}
  , error=function(e) {print(e); return(NULL)}
))

(STATEFP <- unique(tigris::fips_codes$state_code))


tracts_sf%>% subset(STATEFP=='48')

tract_sf%>% subset(TRACTCE==substr('481810005011011',6,11))

nchar("48157674100")

names(tract_sf)

table(dupes <- duplicated(tract_sf, by=c( 'TRACTCE')))  # => tract code is unique nationwide
