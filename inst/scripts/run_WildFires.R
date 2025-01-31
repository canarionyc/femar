

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
LosAngeles_tracts

terra::plot(DINS_2025_Palisades, pch=".", add=FALSE, col="red")
terra::plot(LosAngeles_tracts, main="Los Angeles County", ext= ext(DINS_2025_Palisades), add=TRUE, border="grey")
terra::text(LosAngeles_tracts, labels=LosAngeles_tracts$NAME,  cex=0.65, xpd=FALSE, col="black")


# aggregate points in polygons --------------------------------------------

?terra::relate
nrow(LosAngeles_tracts)
nrow(DINS_2025_Palisades)
r <- terra::relate(LosAngeles_tracts, DINS_2025_Palisades, relation= "intersects")
dim(r)

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

# DINS_2025_Palisades_SF -----------------------------------------

tracts

DINS_2025_Palisades_tracts

?terra::plot
# plot(CA_Wfir_tracts, 'WFIR_HLRR', ext=DINS_2025_Palisades)

DINS_2025_Palisades_SF <- DINS_2025_Palisades %>% subset(grepl(pattern = "^Single Family", x=.$STRUCTURET))
nrow(DINS_2025_Palisades_SF)

dev.off()
opar <- par(mar=c( 5.1, 4.1, 4.1, 2.1))
DINS_2025_Palisades_SF %>% plot( 'DAMAGE'
                                 #     ,  col=rev(map.pal("reds",5L))
                                 , col= colorRampPalette(c("red", "black"))(5L)
                                 , type="classes", sort=c('Destroyed (>50%)','Major (26-50%)','Minor (10-25%)','Affected (1-9%)','No Damage')
                                 , pch=".", cex=1.5
                                 , alpha=0.5
                                 , main="Palisades 2025 Wildfire SF Properties"
                                 ,plg=list(cex=0.65,pt.cex =1, title="Damage")
                                 ,add=FALSE)

# plot(zctas920, add=TRUE)
# text(zctas920, labels=zctas920$ZCTA5CE20, cex=0.65, xpd=TRUE)
plot(LosAngeles_tracts, add=TRUE, border="grey")
?terra::text
text(LosAngeles_tracts,LosAngeles_tracts$NAME, cex=0.65, col="darkgrey")
mtext("Data source: CA Natural Resources Agency GIS", side=1, line = 3)


# DINS_2025_Palisades_SF_png ----

DINS_2025_Palisades_SF_png <- file.path(the$CENSUS_WORKDIR, format(Sys.time(),"DINS_2025_Palisades_SF_%Y%m%d_%H%M.png")); print(file.info(DINS_2025_Palisades_SF_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = DINS_2025_Palisades_SF_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(DINS_2025_Palisades_SF_png)['size'])
browseURL(dirname(DINS_2025_Palisades_SF_png))

# -------------------------------------------------------------------------


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


# DINS_2025_Palisades_tracts ----------------------------------------------

DINS_2025_Palisades_tracts <- terra::intersect(LosAngeles_tracts, DINS_2025_Palisades) %>% as.data.frame()
DINS_2025_Palisades_tracts
?terra::aggregate
names(DINS_2025_Palisades_tracts)

?xtabs
debugonce(xtabs)
(Pali_SF <- DINS_2025_Palisades_tracts[grepl(pattern = "^Single Family", x=DINS_2025_Palisades_tracts$STRUCTURET)
                                       ,] ) %>% as.data.frame()


?aggregate.data.frame
debugonce(aggregate.data.frame)
DINS_2025_Palisades_tracts_stats <- Pali_SF[, "OBJECTID"] %>% aggregate(by=list( Pali_SF$GEOID,Pali_SF$DAMAGE#,    "STRUCTURET"
), FUN="length", na.rm=TRUE)

print(as.data.frame(DINS_2025_Palisades_tracts_stats))

# extract -----------------------------------------------------------------
nrow(LosAngeles_tracts)
dim(LosAngeles_tracts)
nrow(DINS_2025_Palisades)
?terra::extract
e <- extract(LosAngeles_tracts, DINS_2025_Palisades)
dim(e)
str(e)
e[e$id.y==100L,]



# Pali_stats --------------------------------------------------------------


## zonal -------------------------------------------------------------------
?terra::zonal
debugonce(terra::zonal)
methods(zonal)
methods(zonal)
?getMethod
getMethod("zonal",c('SpatVector','SpatVector'))

### Pali_stats_rds ----
Pali_stats_rds <- file.path(the$FEMA_WORKDIR, "Pali_stats.rds"); print(file.info(Pali_stats_rds)['size'])
if(file.exists(  Pali_stats_rds)) {
  Pali_stats <- readRDS(Pali_stats_rds)
} else {
  attr(Pali_stats, "path") <- Pali_stats_rds

  str(DINS_2025_Palisades$DAMAGE)
  is.numeric(DINS_2025_Palisades$DAMAGE)
  DINS_2025_Palisades$DAMAGE.int <- as.integer(DINS_2025_Palisades$DAMAGE)
  major_or_destroyed <- function(x) {#  browser();
    ret_val <- c('Major_or_Destroyed'=sum(x<=2, na.rm = TRUE)
                 ,"Property_Count"=length(x)
                 ,'Major_or_Destroyed_Pct'=round(100*mean(x<=2, na.rm = TRUE),0L)
    ); return(ret_val)}
  zonal_stats <- terra::zonal(DINS_2025_Palisades[grepl(pattern = "^Single Family", x=DINS_2025_Palisades_tracts$STRUCTURET), 'DAMAGE.int'],LosAngeles_tracts
                              , fun= major_or_destroyed
                              ,as.polygons=FALSE)
  str(zonal_stats)

  options(scipen = 999L)
  names(zonal_stats)

  Pali_stats <- cbind(LosAngeles_tracts[zonal_stats$zone], zonal_stats$DAMAGE.int)
  str(values(Pali_stats))
  saveRDS(Pali_stats, Pali_stats_rds); print(file.info(Pali_stats_rds)['size'])
}; str(Pali_stats)



LosAngeles_tracts[zonal_stats$zone]
str(values(Pali_stats))

dev.off()
?terra::plot
plot(Pali_stats, 'Major_or_Destroyed_Pct'
     ,type="interval"#, breaks=seq.int(10,90,10)
     , col= colorRampPalette(c("white","red" ))(10L))
text(Pali_stats, labels='NAME', cex=0.65)


## aggregate.sf ------------------------------------------------------------


library(sf)
help(package="sf")
?aggregate.sf

nrow(LosAngeles_tracts)
zonal_stats_sf <- aggregate(st_as_sf(DINS_2025_Palisades[grepl(pattern = "^Single Family", x=DINS_2025_Palisades_tracts$STRUCTURET), 'DAMAGE.int'],), st_as_sf(LosAngeles_tracts)
                            , FUN = major_or_destroyed
)

zonal_stats_sf$GEOID <- LosAngeles_tracts$GEOID
zonal_stats_sf <- zonal_stats_sf%>% subset(!is.na(Major_or_Destroyed))

# zonal_stats_sf$Major_or_Destroyed_Pct <- round(100*zonal_stats_sf$Major_or_Destroyed/zonal_stats_sf$Property_Count,2L)

?plot.sf
dev.off()

zonal_stats_sf%>% subset( select='Major_or_Destroyed_Pct') %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE
                                                                    ,main="Palisades WildFire 2025 % of SF Properties w/ Major Damage (26-50%) or Destroyed (>50%) "
                                                                    # ,las=2
                                                                    # , breaks = "jenks"
                                                                    ,pal=colorRampPalette(c("white","red" ))
                                                                    ,cex=0.65

                                                                    # ,setParUsrBB=TRUE,expandBB=c(0.1,0,0.1,0)
)
zonal_stats_sf %>% {text(., labels=substr(.$GEOID,6,11), cex=0.65)}
mtext(text = "Data source: DINS", side = 1L)

unique(nchar(zonal_stats_sf$GEOID))
# zonal_stats_sf_png ----

zonal_stats_sf_png <- file.path(the$FEMA_WORKDIR, format(Sys.time(),"zonal_stats_sf_%Y%m%d_%H%M.png")); print(file.info(zonal_stats_sf_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = zonal_stats_sf_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(zonal_stats_sf_png)['size'])
browseURL(dirname(zonal_stats_sf_png))

