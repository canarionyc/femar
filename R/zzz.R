
options("yaml.eval.expr"=TRUE)
.onAttach <- function(libname, pkgname) {
# browser()



  msgs <- c( sprintf("Running .onAttach(%s)", pkgname)
             ,
             paste(".NRI_workdir",  .NRI_workdir, sep=": " )
             ,
    sprintf("TMPDIR=%s", Sys.getenv("TMPDIR", unset = NA))
    ,
    sprintf("TIGRIS_CACHE_DIR=%s", Sys.getenv("TIGRIS_CACHE_DIR", unset = NA))
    )

  options(tigris_use_cache = TRUE)
  packageStartupMessage(paste(msgs, collapse  = "\n")
  )
}


.onDetach <- function(libpath) {
  message(paste("Detaching", libpath))
}
