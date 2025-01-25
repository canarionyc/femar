

# setup -------------------------------------------------------------------
library(terra)
terraOptions()
getOption("tigris_year");options(tigris_year=2022L);getOption("tigris_year")
devtools::load_all("~/Spatial/FEMA/femar")

# hazard management system -------------------------------------------------

library(sf)
shape.file <- "E:\\Datasets\\NOAA\\OSPO\\hms_fire20250107.shp"
shape.file
hms_fire.sf <- read_sf(shape.file)
hms_fire.sf['FRP']
plot(st_geometry(hms_fire.sf))

# CA_tracts----------------------------------------------------------------------
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_year=2022)
# CA_tracts_sf <- tracts(cb=TRUE, state='06', keep_zipped_shapefile = TRUE)%>% st_transform( st_crs(3857))

?tigris::tracts
CA_tracts <- tigris::tracts(cb=TRUE, state='06',  keep_zipped_shapefile = TRUE) %>% terra::vect() %>% project("epsg:3857")
?crs
cat(crs(CA_tracts))
CA_tracts
plot(CA_tracts)


# LosAngeles_tracts -------------------------------------------------------
# Using FIPS code '037' for 'Los Angeles County'
LosAngeles_tracts <-  tigris::tracts(cb=TRUE, state='06', county="Los Angeles County", keep_zipped_shapefile = TRUE) %>% terra::vect() %>% project("epsg:3857")
plot(LosAngeles_tracts, main="Los Angeles County")

# CA_Wfir_tracts ---------------------------------------------------------------

names(NRI_tracts)
grep("WFIR",names(NRI_tracts), value = TRUE)
table(NRI_tracts$STATEABBRV)

?`subset,SpatVector-method`
match("CRF_VALUE", names(NRI_tracts))
CA_Wfir_tracts <- NRI_tracts %>% subset(subset=NRI_tracts$STATEABBRV=="CA" & NRI_tracts$C, select=c(1:match("CRF_VALUE", names(NRI_tracts)), wfir_cols), NSE=FALSE)
table(CA_Wfir_tracts$COUNTY, useNA = "ifany")

LosAngeles_Wfir_tracts <- CA_Wfir_tracts[CA_Wfir_tracts$COUNTY=="Los Angeles"]

summary(LosAngeles_Wfir_tracts[, ])

table(CA_Wfir_tracts$WFIR_HLRR)

(wfir_cols <- grep("WFIR",names(NRI_tracts), value = FALSE))

plot(CA_Wfir_tracts, 'WFIR_HLRR')

# DINS_2025_Palisades_Public_View -----------------------------------------

DINS_2025_Palisades.shp <-"E:\\Datasets\\GOV\\CA\\CNRA\\DINS_2025_Palisades_Public_View.shp"; stopifnot(file.exists(DINS_2025_Palisades.shp))
DINS_2025_Palisades <- terra::vect(DINS_2025_Palisades.shp)
print(DINS_2025_Palisades)
summary(DINS_2025_Palisades)

table(DINS_2025_Palisades$STRUCTURET)
table(DINS_2025_Palisades$DAMAGE)

cat(crs(DINS_2025_Palisades))

tracts

DINS_2025_Palisades_tracts

?terra::plot
plot(CA_Wfir_tracts, 'WFIR_HLRR', ext=DINS_2025_Palisades)

plot(DINS_2025_Palisades, 'DAMAGE'
     #     ,  col=rev(map.pal("reds",5L))
     , col= colorRampPalette(c("red", "grey"))(5L)
     , type="classes", sort=c('Destroyed (>50%)','Major (26-50%)','Minor (10-25%)','Affected (1-9%)','No Damage')
     , pch=20, cex=0.5
     , main="FNMA MF properties and Palisades 2025 Wildfire"
     ,add=FALSE)

plot(zctas920, add=TRUE)
text(zctas920, labels=zctas920$ZCTA5CE20, cex=0.65, xpd=TRUE)

?terra::text
mtext("Data source: CA Natural Resources Agency GIS, Fannie Mae", side=1, line = 3)


debugonce(print)
?print
print(pali_properties[]
      #       , max = 9999L
)

?terra
?terraOptions
?terra::plot


plot(DINS_2025_Palisades, 'STRUCTURET', pch="."
     , main="Palisades 2025 Wildfire")
plot(CA_tracts, add=TRUE)
