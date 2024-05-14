
# setup -------------------------------------------------------------------

configr::read.config()
devtools::load_all("~/fstutils/", export_all = TRUE)
list.files(.emdat_datadir, full.names = TRUE)
dir.create(.emdat_workdir)

(usdat_xlsx <- list.files(.emdat_datadir, pattern = "2024-05-08", full.names = TRUE))


usdat_fst <- file.path(.emdat_workdir, "usdat.fst"); print(file.info(usdat_fst))
if(file.exists(usdat_fst)) {
  print(fst.metadata(usdat_fst))
  usdat <- read_fst(usdat_fst, as.data.table = TRUE)
} else {



usdat <- rio::import(usdat_xlsx,setclass = "data.table")%>% sanitize()
str(usdat)
write_fst(usdat, path = usdat_fst);   print(file.info(usdat_fst))
}; str(usdat)
usdat
