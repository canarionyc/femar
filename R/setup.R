#' #' @export
#' the$NRI_WORKDIR <- NULL
#' #' @export
#' the$NOAA_WORKDIR <- NULL
#' #' @export
#' the$CENSUS_WORKDIR <- NULL


#' @rawNamespace import(stats, except = filter)
# options(yaml.eval.expr=TRUE)

# help("attach")

.onLoad  <- function(libname, pkgname) {
  # browser()
  # print(stringr::str_glue("About to load {libname}/{pkgname}"))

  # str(.cfg$default)
  # ?list2env
  # localpaths.env <- list2env(.cfg$default
  #                            # , envir = new.env()
  #                            )
  #print(ls(localpaths.env ))
  #str(as.list( localpaths.env ))
  #print(ls.str(localpaths.env))
  list2env(.cfg$default, envir = the)
  # print(ls(the))

  # options(scipen=999L)
  suppressWarnings({

#     # attach(cfg$default)
#     # str(cfg)
# # browser()
#  #   the$NRI_WORKDIR <- cfg$default$the$NRI_WORKDIR
#     the$NRI_WORKDIR <<- configr::eval.config("the$NRI_WORKDIR",eval.expr=TRUE)
#     fs::dir_create(the$NRI_WORKDIR, recurse = TRUE)
#
#     the$NOAA_WORKDIR <<- configr::eval.config("the$NOAA_WORKDIR",eval.expr=TRUE)
#     fs::dir_create(the$NOAA_WORKDIR, recurse = TRUE)
#
#     the$CENSUS_WORKDIR <<- configr::eval.config("the$CENSUS_WORKDIR",eval.expr=TRUE)
    })


}


.onUnload <- function(libpath) {

}
