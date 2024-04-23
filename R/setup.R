#' @import configr

.onLoad  <- function(libname, pkgname) {
  # print(stringr::str_glue("About to load {libname}/{pkgname}"))
  .datatable.aware <- TRUE
  options(yaml.eval.expr=TRUE)
  # options(scipen=999L)
  suppressWarnings({
    cfg <- configr::read.config(eval.expr=TRUE); stopifnot(is.list(cfg))
    # str(cfg)
# browser()
 #   .nri_workdir <- cfg$default$NRI_WORKDIR
    .nri_workdir <- configr::eval.config("NRI_WORKDIR",eval.expr=TRUE)
    fs::dir_create(.nri_workdir, recurse = TRUE)

    .noaa_workdir <- configr::eval.config("NOAA_WORKDIR",eval.expr=TRUE)
    fs::dir_create(.noaa_workdir, recurse = TRUE)
    })
}

# onUnLoad ----------------------------------------------------------------

.onUnload <- function(libpath) {

}
