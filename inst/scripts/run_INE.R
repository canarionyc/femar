
# setup -------------------------------------------------------------------

# file.choose()

(pobmun23 <- readxl::read_xlsx("E:\\Datasets\\ES\\INE\\pobmun23.xlsx", skip = 1L) %>% as.data.table())
pobmun23[, LAU_ID:=paste0(CPRO,CMUN)]
pobmun23[LAU_ID=="38038"]
lau_es[lau_es$LAU_NAME =="Santa Cruz de Tenerife"]

(lau_es_pobmun23 <- terra::merge(lau_es, pobmun23, by="LAU_ID"))
lau_es_pobmun23 %>% as.data.frame() %>% clipr::write_clip()
