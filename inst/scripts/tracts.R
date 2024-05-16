(STATEFP <- unique(tigris::fips_codes$state_code))

(url_tract_zip <- paste("https://www2.census.gov/geo/tiger/TIGER2023/TRACT/tl_2023",STATEFP, "tract.zip", sep="_"))

?download.file
setwd()
lapply(url_tract_zip[-1], function(url) tryCatch(
  {
    destfile <- file.path(Sys.getenv('TIGRIS_CACHE_DIR'),basename(url))
    if(file.exists(destfile)) return(destfile)
    download.file(url, destfile = destfile)}
  , error=function(e) {print(e); return(NULL)}
))

(STATEFP <- unique(tigris::fips_codes$state_code))

(tract_zip <- list.files(path = Sys.getenv('TIGRIS_CACHE_DIR')
                         ,pattern = "tl_2023_.*", full.names = TRUE))
tract_zip[1]
?unzip
?read_sf
out <- unzip(tract_zip[1]
             , exdir =
)



(tract_01_sf <-  read_sf(dsn=file.path(Sys.getenv('TIGRIS_CACHE_DIR'), basename(tract_zip[1])) %>% fs::path_ext_remove()))

(tract_01_sf <-  read_sf(dsn=file.path(Sys.getenv('TIGRIS_CACHE_DIR'), basename(tract_zip[1])) %>% fs::path_ext_remove()))
