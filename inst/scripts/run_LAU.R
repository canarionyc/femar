# setup -------------------------------------------------------------------
readRenviron("~/.Renviron")
?isNamespaceLoaded
loadedNamespaces()

if(!isNamespaceLoaded("terra")) devtools::load_all("~/Spatial/terra-master/")

file.choose()


# lau ---------------------------------------------------------------------

lau_es_gpkg <- file.path(the$FEMA_WORKDIR, "lau_es.gpkg"); print(file.info(lau_es_gpkg))
if(file.exists(lau_es_gpkg)) {
  lau_es <- terra::vect(lau_es_gpkg)

} else {
  lau <- terra::vect( "E:\\Datasets\\EU\\EC\\ref-lau-2021-01m.gdb.zip", "LAU_RG_01M_2021_3035")

  (lau_es <- lau %>% terra::subset(lau$CNTR_CODE=="ES" ))

  # verify the area ---------------------------------------------------------


  lau_es$area <- terra::expanse(lau_es)
  range(lau_es$area/lau_es$Shape_Area)

  lau_es[order(lau_es$POP_2021, decreasing = TRUE)]
  lau_es %>% terra::subset( grepl("Valencia",lau_es$LAU_NAME))
  lau_es$LAU_NAME%>% sort(decreasing = FALSE) %>% tail(100L)

  lau_es[grep("València",lau_es$LAU_NAME, value = FALSE),]
  lau_es %>% terra::subset(lau_es$GISCO_ID=="ES_46250")

  lau_es$developed_area <- 100*terra::zonal(CLMS_CLCplus_stk, lau_es, fun=base::sum, na.rm=TRUE)
  lau_es$developed_area_pct_of_area <- round(100*lau_es$developed_area/lau_es$area,2L)
  print(range(lau_es$developed_area_pct_of_area, na.rm = TRUE))

  (pobmun23 <- readxl::read_xlsx("E:\\Datasets\\ES\\INE\\pobmun23.xlsx", skip = 1L) %>% as.data.table())
  pobmun23[, LAU_ID:=paste0(CPRO,CMUN)]
  pobmun23[LAU_ID=="38038"]
  lau_es[lau_es$LAU_NAME =="Santa Cruz de Tenerife"]

  (lau_es_pobmun23 <- terra::merge(lau_es, pobmun23, by="LAU_ID"))

  lau_es_pobmun23$POP_DENS_2023 <- round(lau_es_pobmun23$POB23/lau_es_pobmun23$area*1e6,2)
  lau_es_pobmun23 %>% as.data.frame() %>% clipr::write_clip()

  lau_es<- lau_es_pobmun23

  terra::writeVector(lau_es, lau_es_gpkg, overwrite=TRUE)
  lau_es
}; lau_es

terra::plot(lau_es)
crs(lau_es)
# valencia_pop_dens_2021 ----

?terra::plot
lau_es %>%
  # terra::subset(lau_es$GISCO_ID=="ES_46250") %>%
  terra::plot("POP_DENS_2023"

              , ext=terra::ext(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1)
              , breakby= "cases"# "eqint"
              , col=rev(heat.colors(5))#, border=NA
              # ,ext=my_ext
              , plg=list(title="pop23 per km2")
              , main="Population 2023 quintiles by municipio"
  )

terra::polys(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1)

# valencia_pop_dens_2021_png ----

valencia_pop_dens_2021_png <- file.path(the$FEMA_WORKDIR, format(Sys.time(),"valencia_pop_dens_2023_%Y%m%d_%H%M.png")); print(file.info(valencia_pop_dens_2021_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = valencia_pop_dens_2021_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(valencia_pop_dens_2021_png)['size'])
browseURL(dirname(valencia_pop_dens_2021_png))

# developed_area_pct_of_area ----------------------------------------------------

lau_es %>%
  # terra::subset(lau_es$GISCO_ID=="ES_46250") %>%
  terra::plot("developed_area_pct_of_area"

              , ext=terra::ext(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1)
              , breakby= "cases"# "eqint"
              , col=rev(heat.colors(5))#, border=NA
              # ,ext=my_ext
              , plg=list(title="Developed area % of municipal area")
              , main="Developed Area quantiles by municipio"
  )

# lau_es_crp_gpkg ---------------------------------------------------------

lau_es_crp_gpkg <- file.path(the$FEMA_WORKDIR, "lau_es_crp.gpkg"); print(file.info(lau_es_crp_gpkg))
if(file.exists(lau_es_crp_gpkg)) {
  lau_es_crp <- terra::vect(lau_es_crp_gpkg)
} else {
  ?terra::crop
  lau_es_crp<- terra::crop(lau_es, EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1)
  plot(lau_es_crp)
  ?terra::expanse
  unique(lau_crp$GISCO_ID)
  lau_es_crp$area_crp <- terra::expanse(lau_es_crp)
  lau_es_crp$area_crp_pct_of_area <- 100*lau_es_crp$area_crp/lau_es_crp$area
  range(lau_es_crp$area_crp_pct_of_area)

  lau_es_crp[order(lau_es_crp$area_crp_pct_of_area, decreasing = TRUE)]

  lau_es_crp$developed_area_crp <- 100*terra::zonal(CLMS_CLCplus_stk_msk, lau_es_crp, fun=base::sum, na.rm=TRUE)
  range(100*lau_es_crp$developed_area_crp/lau_es_crp$developed_area)
  terra::writeVector(lau_es_crp, lau_es_crp_gpkg,overwrite=TRUE)
}
?terra::mask
# lau_es_crp_msk <- terra::mask(CLMS_CLCplus_stk_msk, lau_es_crp)
?terra::zonal
dana_out <- terra::zonal(CLMS_CLCplus_stk_msk, EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg, fun=base::sum, na.rm=TRUE)
dana_out

?terra::intersect
names(lau_es_crp)




# cleanup -----------------------------------------------------------------


