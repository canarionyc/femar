library(sf)
devtools::load_all("~/Spatial/FEMA/femar/")

# cbsa_sf --------------------------------------------------------------------

cbsa_sf <- get_cbsa_sf()
cbsa_sf

cbsa_sf%>%subset(CBSAFP == 26420)

(cbsa_lcc_sf <- cbsa_sf %>% st_transform(st_crs(lcc)))

cbsa_sf <- cbsa_sf %>% subset( stri_extract_last_regex(NAME, pattern = "[A-Z]{2}(-[A-Z]{2})*") %in% c('LA','MS'))
cbsa_sf

?plot.sf
cbsa_sf %>% subset(select='NAME') %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)
str(cbsa_sf)


# cbsa_vec ----------------------------------------------------------------

library(terra)
cbsa_vec <- get_cbsa_vec()
# cbsa_vec <- terra::vect(cbsa_sf )

table(cbsa_vec$LSAD, useNA = "ifany")

labels <-paste(cbsa_vec$NAME, LA_cbsa_vec$CBSAFP, sep = " ")
?`plot,SpatVector,character-method`
options(terra.pal=terrain.colors(10))
plot(cbsa_vec, y='ALAND'
#     , col=map.pal("viridis", 100)
     ); terra::text(cbsa_vec, labels= labels, cex=0.65)



# LA_cbsa_sf --------------------------------------------------------------

LA_cbsa_sf <- cbsa_sf %>% subset( stri_extract_last_regex(NAME, pattern = "[A-Z]{2}(-[A-Z]{2})*")=='LA')
LA_cbsa_sf

?plot.sf
LA_cbsa_sf %>% subset(select='NAME') %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)
str(LA_cbsa_sf)
library(terra)
LA_cbsa_vec <- terra::vect(LA_cbsa_sf )


dev.off()
labels <-paste(LA_cbsa_vec$NAME, LA_cbsa_vec$CBSAFP, sep = " ")
plot(LA_cbsa_vec); terra::text(LA_cbsa_vec, labels= labels, cex=0.65)

# and metropolitan_statistical_area_or_metropolitan_division not in (35380)
# and metropolitan_statistical_area_or_metropolitan_division not in (12940
#                                                                    ,25220
#                                                                    ,26380
#                                                                    ,29340)
?dplyr::case_when
LA_cbsa_sf$group <- with(LA_cbsa_sf,dplyr::case_when(CBSAFP %in% c("35380") ~ 1
                                     ,CBSAFP %in% c("12940"
                                                   ,"25220"
                                                   ,"26380"
                                                   ,"29340") ~2
                                     ,TRUE ~ 3)
)
LA_cbsa_sf

Ka_cbsa_vec <- terra::vect(LA_cbsa_sf)
labels <-paste(Ka_cbsa_vec$NAME, Ka_cbsa_vec$CBSAFP, sep = " ")
?`plot,SpatVector,character-method`
plot(Ka_cbsa_vec,'group'); terra::text(Ka_cbsa_vec, labels= labels, cex=0.65)

# cbsa_names --------------------------------------------------------------


cbsa_names <- get_cbsa_names()
str(cbsa_names)
stri_ex
stri_extract_last_regex(cbsa_names$NAME, pattern = "[A-Z]{2}(-[A-Z]{2})*")

cbsa_names[STUSPS=='LA']
