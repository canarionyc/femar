#' @import configr
#' @rawNamespace import(stats, except = filter)
options(yaml.eval.expr=TRUE)

.onLoad  <- function(libname, pkgname) {
  # print(stringr::str_glue("About to load {libname}/{pkgname}"))


  # options(scipen=999L)
  suppressWarnings({
    cfg <- configr::read.config(eval.expr=TRUE); stopifnot(is.list(cfg))
    # str(cfg)
# browser()
 #   .NRI_workdir <- cfg$default$NRI_WORKDIR
    .NRI_workdir <- configr::eval.config("NRI_WORKDIR",eval.expr=TRUE)
    fs::dir_create(.NRI_workdir, recurse = TRUE)

    .noaa_workdir <- configr::eval.config("NOAA_WORKDIR",eval.expr=TRUE)
    fs::dir_create(.noaa_workdir, recurse = TRUE)

    .census_workdir <- configr::eval.config("CENSUS_WORKDIR",eval.expr=TRUE)
    })


}

# onUnLoad ----------------------------------------------------------------

.onUnload <- function(libpath) {

}
