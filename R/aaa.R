#' @import yaml

.cfg <- yaml::yaml.load_file(Sys.getenv("R_CONFIGFILE_ACTIVE"))

the <- new.env(parent = emptyenv())
print(the)



