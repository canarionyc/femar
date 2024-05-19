#' @title gt census tracts
#
#' Because blocks nest in tracts, and tract IDs even form the first part of block
#' IDs, it's relatively straightforward to generate tract crosswalks from block
#' crosswalks.
#' @return simple feature
#' @export
get_tracts_sf <- function(){
  tracts_sf_rds <- file.path(.census_workdir, "tracts_sf.rds"); print(file.info(tracts_sf_rds))
  if(file.exists(  tracts_sf_rds)) {
    tracts_sf <- readRDS(tracts_sf_rds)
  } else {
    (tracts_dsn <- list.files(path = Sys.getenv('TIGRIS_CACHE_DIR')
                              ,pattern = "tl_2023_\\d{2}_tract$*", full.names = TRUE))


    tracts_sf.lst <- lapply(tracts_dsn, st_read)

    tracts_sf <- do.call("rbind", tracts_sf.lst)

    saveRDS(tracts_sf,   tracts_sf_rds); print(file.info(tracts_sf_rds))
  }; str(tracts_sf)
  tracts_sf
}
