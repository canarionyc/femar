#' @export
NRI_WORKDIR <- NULL
#' @export
NOAA_WORKDIR <- NULL
#' @export
CENSUS_WORKDIR <- NULL

cfg <- NULL
#' @import configr
#' @rawNamespace import(stats, except = filter)
options(yaml.eval.expr=TRUE)

.onLoad  <- function(libname, pkgname) {
  # print(stringr::str_glue("About to load {libname}/{pkgname}"))


  # options(scipen=999L)
  suppressWarnings({
    (cfg <<- configr::read.config(eval.expr=TRUE)); stopifnot(is.list(cfg))
    cfg.env <- list2env(cfg$default)
    # attach(cfg$default)
    # str(cfg)
# browser()
 #   NRI_WORKDIR <- cfg$default$NRI_WORKDIR
    NRI_WORKDIR <<- configr::eval.config("NRI_WORKDIR",eval.expr=TRUE)
    fs::dir_create(NRI_WORKDIR, recurse = TRUE)

    NOAA_WORKDIR <<- configr::eval.config("NOAA_WORKDIR",eval.expr=TRUE)
    fs::dir_create(NOAA_WORKDIR, recurse = TRUE)

    CENSUS_WORKDIR <<- configr::eval.config("CENSUS_WORKDIR",eval.expr=TRUE)
    })


}

# onUnLoad ----------------------------------------------------------------

.onUnload <- function(libpath) {
  # detach(cfg$default)
}
