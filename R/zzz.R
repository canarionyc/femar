
options("yaml.eval.expr"=TRUE)
.onAttach <- function(libname, pkgname) {
# browser()

  options(tigris_use_cache = TRUE)
  options(tigris_use_cache = TRUE)

  options(tigris_year=2021L);
  # ?tigris_cache_dir
  # debugonce(tigris_cache_dir)
  # tigris_cache_dir(Sys.getenv('TIGRIS_CACHE_DIR'))
  # readRenviron('~/.Renviron')

  options(tigris_class = "sf")

  msgs <- c( sprintf("Running .onAttach(%s)", pkgname)
             ,
             paste("NRI_WORKDIR",  NRI_WORKDIR, sep=": " )
             ,
    sprintf("TMPDIR=%s", Sys.getenv("TMPDIR", unset = NA))
    ,
    sprintf("TIGRIS_CACHE_DIR=%s", Sys.getenv("TIGRIS_CACHE_DIR", unset = NA))
    ,sprintf("tigris_year=%s", getOption("tigris_year"))
    ,sprintf("tigris_class=%s", getOption("tigris_class"))
    )




  packageStartupMessage(paste(msgs, collapse  = "\n")
  )
}


.onDetach <- function(libpath) {
  message(paste("Detaching", libpath))
}
