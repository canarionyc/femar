# setup -------------------------------------------------------------------
readRenviron("~/.Renviron")
?isNamespaceLoaded
loadedNamespaces()

if(!isNamespaceLoaded("terra")) devtools::load_all("~/Spatial/terra-master/")

file.choose()

lau <- terra::vect( "E:\\Datasets\\EU\\EC\\ref-lau-2021-01m.gdb.zip", "LAU_RG_01M_2021_3035")
(lau_es <- lau %>% terra::subset(lau$CNTR_CODE=="ES" ))

terra::plot(lau_es,"POP_DENS_2021", ext=terra::ext(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1))

terra::polys(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1)
