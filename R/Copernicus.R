#' @export
get_svc <- function(EMSR773_AOI03_BLP.dir){
  # browseURL(the$FEMA_WORKDIR)
  svc_rds <- file.path(the$FEMA_WORKDIR, "svc.rds"); print(file.info(svc_rds)['size'])
  if(file.exists(  svc_rds)) {
    # sv.wrapped.list <- readRDS(svc_rds)
    # str(sv.wrapped.list, max.level=1L)
    # svc <- terra::svc(lapply(sv.wrapped.list, terra::unwrap))
    sv.list <- readRDS(svc_rds)
  } else {
    # if(is.null(names(vectors))){
    #   (what <- stringi::stri_extract_all_regex(vectors, pattern =  "(?<=EMSR773_AOI03_BLP_PRODUCT_)\\w+(?=_v1)", simplify = FALSE) %>%
    #      as.character())
    #
    #   names(vectors) <- what
    # }
    # print( names(vectors))
    st_layers(EMSR773_AOI03_BLP.dir)
    ?st_read
    layers <- st_layers(EMSR773_AOI03_BLP.dir) %>% as.data.frame()
    str(layers)
print(layers$name)
    sv.list <- lapply( layers$name, sf::st_read, dsn=EMSR773_AOI03_BLP.dir)

    sv.list <- sv.list %>% lapply(FUN=sf::st_transform, crs=st_crs("EPSG:3035"))
    # ?terra::vect
    # sv.list <- purrr::map(vectors, ~terra::vect(EMSR773_AOI03_BLP.dir, .x) %>%  terra::project( "EPSG:3035"))
    #
    #
    print(sv.list[[1]])

    # svc <- terra::svc(sv.list)


    # print(svc)
    # sv.list <- as.list(svc)
    # str(sv.list, max.level=1L)

    ?utils::str
    # attr(svc, "path") <- svc_rds

    # saveRDS(lapply(sv.list, terra::wrap), file=svc_rds, compress = FALSE); print(file.info(svc_rds)['size'])
    # svc <- terra::svc(sv.list)
    saveRDS(sv.list, svc_rds)
  };  str(sv.list, max.level=1L)
  sv.list
}
