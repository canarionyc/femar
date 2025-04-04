
# setup -------------------------------------------------------------------
readRenviron("~/.Renviron")
?isNamespaceLoaded
loadedNamespaces()
isNamespaceLoaded("tigris")

if(!isNamespaceLoaded("terra")) devtools::load_all("~/Spatial/terra-master/")

file.choose()


NUTS_RG_01M_2024 <- terra::vect("E:\\Datasets\\EU\\EC\\NUTS_RG_01M_2024_3035.shp.zip")
table(NUTS_RG_01M_2024$CNTR_CODE)

?subset
NUTS_RG_01M_2024 %>% subset(CNTR_CODE=="ES" & NUTS_NAME=="Valencia/València", drop=FALSE, NSE=TRUE) %>% table()
NUTS_RG_01M_2024 %>% subset(CNTR_CODE=="ES" & NUTS_NAME=="Valencia/València", drop=FALSE, NSE=TRUE) %>% terra::plot()
NUTS_RG_01M_2024 %>% subset(NUTS_ID=="ES523", drop=FALSE, NSE=TRUE) %>% terra::plot()
