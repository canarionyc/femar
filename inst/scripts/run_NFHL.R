# setup -------------------------------------------------------------------


# library(rgdal)

# library(terra)
# help(package="rgdal")

configr::read.config()

library(sf)
library(terra)
devtools::load_all("~/Spatial/FEMA/femar/");
# NFHL_kmz <- file.path(Sys.getenv("DATADIR"), "ArcGIS", "al092024_best_track.kmz")
# unzip(NFHL_kmz, exdir = file.path(Sys.getenv("DATADIR"), "ArcGIS"))

dir.exists(.fema_datadir)


library(xml2)
help(package="xml2")

doc <- read_xml( file.path(.fema_datadir, "MSC","NFHL_12_20241010_metadata.xml"))
?xml_find_all
eainfo <- xml2::xml_find_all(doc, ".//eainfo")
str(eainfo)


(layers_nodeset <- xml_children(eainfo))

(eainfo_df <- data.frame(enttypl=xml_find_all(layers_nodeset, ".//enttypl") %>% xml_text()
,enttypd=xml_find_all(layers_nodeset, ".//enttypd") %>% xml_text()
,enttypds=xml_find_all(layers_nodeset, ".//enttypds") %>% xml_text()
)) %>% clipr::write_clip("table")
?xml_text
xml_text(layers_nodeset)

# NFHL --------------------------------------------------------------------

library(jsonlite)
NFHL_12_20241010_gdb <- file.path(.fema_datadir, "MSC","NFHL_12_20241010.gdb")

?terra::vect

(S_Fld_Haz_Ar <- terra::vect(NFHL_12_20241010_gdb, "S_FLD_HAZ_AR", proxy=TRUE))
names(S_Fld_Haz_Ar)

(S_Fld_Haz_Ln  <- terra::vect(NFHL_12_20241010_gdb, "S_FLD_HAZ_LN", proxy=TRUE))
names(S_Fld_Haz_Ln )

# NFHL_vec --------------------------------------------------------------



(NFHL_sf <- read_sf(NFHL_12_20241010_gdb) # %>% terra::vect()
# %>% set.crs("EPSG:4269")
 ) # NAD83
