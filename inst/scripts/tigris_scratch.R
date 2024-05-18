# ?blocks
#
# ?load_tiger
#' @title Get census blocks
#'
#' @param state
#'
#' @import tigris
get_census_blocks <- function(state){
  tx_blocks_rds <- file.path(.geo_dir, "tx_blocks.rds"); print(file.info(tx_blocks_rds))
  if(file.exists(tx_blocks_rds)) {
    tx_blocks_sf <- readRDS(tx_blocks_rds)
  } else {
    (tx_blocks_sf <- tigris::blocks(state="TX",keep_zipped_shapefile = TRUE))
    print(tx_blocks_sf)
    saveRDS(tx_blocks_sf,   tx_blocks_rds); print(file.info(tx_blocks_rds))
  }; str(tx_blocks_sf)
  return(tx_blocks_sf)
}
