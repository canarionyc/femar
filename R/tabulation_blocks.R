# # setup ----
# options(rgdal_show_exportToProj4_warnings="none")
# library(rgdal)
# # library(tmap)
# library(raster) # needed for plot(ca)
# library(sf); help(package="sf")
# library(rgeos)
# # library(leaflet)
# # library(mapview)
#
# # file.show('~/.Renviron')
# readRenviron('~/.Renviron')
# cat("CENSUS_KEY:",Sys.getenv("CENSUS_KEY"))
# cat("TIGRIS_CACHE_DIR:", Sys.getenv('TIGRIS_CACHE_DIR'))
# # if(! dir.exists(Sys.getenv('TIGRIS_CACHE_DIR')))
# #   dir.create(Sys.getenv('TIGRIS_CACHE_DIR'))
#
# source("~/Spatial/Tigris/.RProfile")
# stop()
# browseURL(.census_workdir)
# browseURL(Sys.getenv('TIGRIS_CACHE_DIR'))
# tx_blocks ------------------------------------------------------------------


#' @title get map from census block to county
#' @return data.table
#' @importFrom RCurl url.exists
#' @import utils
#' @export
get_tabblock <- function(){
  # configr::read.config(); ls(all.names = TRUE)

  tabblock_fst <- file.path(.census_workdir, "tl_2023_ZZ_tabblock20.fst"); print(file.info(tabblock_fst))
  if(file.exists(tabblock_fst)) {
    print(fst.metadata(tabblock_fst))
    tabblock <- read_fst(tabblock_fst,columns = c("STATEFP20", "COUNTYFP20", "TRACTCE20", "BLOCKCE20", "GEOID20",
                                                  #                                                  "GEOIDFQ20",
                                                  "NAME20", "MTFCC20", "UR20", "UACE20", "FUNCSTAT20",
                                                  "ALAND20", "AWATER20", "INTPTLAT20", "INTPTLON20", "HOUSING20",
                                                  "POP20"), as.data.table = TRUE)
  } else {
    (STATEFP <- unique(tigris::fips_codes$state_code))

    (url_tabblock_zip <- paste("https://www2.census.gov/geo/tiger/TIGER2023/TABBLOCK20/tl_2023",STATEFP, "tabblock20.zip", sep="_"))
    length(url_tabblock_zip)
    url_tabblock_zip <- url_tabblock_zip[url.exists(url_tabblock_zip)]
    length(url_tabblock_zip)


    lapply(url_tabblock_zip[1], function(url) tryCatch( {
      destfile <- file.path(Sys.getenv('TIGRIS_CACHE_DIR'),basename(url))
      if(file.exists(destfile)) return(destfile)
      utils::download.file(url, destfile = destfile)
      utils::unzip(destfile, exdir = fs::path_ext_remove(destfile))
    }
    , error=function(e) {print(e); return(NULL)}
    ))

    (tabblock.dir <- list.files(Sys.getenv('TIGRIS_CACHE_DIR'), pattern = "tl_2023_\\d{2}_tabblock20$", full.names = TRUE))

    dsn <- tabblock.dir[1]; print(file.info(dsn))

    tabblock <- lapply(tabblock.dir, function(dsn) {
      tabblock_sf <- read_sf(dsn, as_tibble = FALSE)
      print(tabblock_sf)
      tabblock_dt <- st_drop_geometry(tabblock_sf) %>% setDT(key='GEOID20')
      rm(tabblock_sf)
      tabblock_fst <- file.path(.census_workdir, basename(dsn)) %>% fs::path_ext_set("fst")
      write_fst(tabblock_dt, tabblock_fst); print(file.info(tabblock_fst))
      return(tabblock_dt)
    }) %>% rbindlist(fill=TRUE)

    (tabblock_fst.vec <- list.files(.census_workdir, pattern = "tl_2023_\\d{2}_tabblock20\\.fst", full.names = TRUE))

tabblock[, UR20:=as.factor(UR20)]
print(tabblock[, table(UR20, useNA = "ifany")])
tabblock_dt <- lapply(tabblock_fst.vec, read_fst) %>% rbindlist(fill=TRUE)

    stopifnot(anyDuplicated(tabblock$GEOID20)==0)
    setkeyv(tabblock, c('GEOID20'))
    tabblock[, c('INTPTLAT20','INTPTLON20'):=list(as.numeric(INTPTLAT20), as.numeric(INTPTLON20))]

    write_fst(tabblock, path = tabblock_fst);   print(file.info(tabblock_fst))
  }; str(tabblock)
  tabblock
}


