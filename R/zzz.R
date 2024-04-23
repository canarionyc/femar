
options("yaml.eval.expr"=TRUE)
.onAttach <- function(libname, pkgname) {
# browser()
  msgs <- c( sprintf("Running .onAttach(%s)", pkgname)
             ,
             paste(".nri_workdir",  .nri_workdir, sep=": " )
             ,
    sprintf("TMPDIR=%s", Sys.getenv("TMPDIR", unset = NA))
    )


  packageStartupMessage(paste(msgs, collapse  = "\n")
  )
}


.onDetach <- function(libpath) {
  message(paste("Detaching", libpath))
}
