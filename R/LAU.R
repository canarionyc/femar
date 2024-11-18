#' @export
get_lau_es <- function(){
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
  };
  return(lau_es)
}
