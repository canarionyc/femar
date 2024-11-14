
# setup -------------------------------------------------------------------
# rm(list = ls())
options(scipen=999L)
source("~/.RProfile", echo=TRUE)
?removeGeneric
if(isGeneric("not.na")) removeGeneric("not.na")
# help(package="rprojroot")
#
# (root <- rprojroot::is_r_package)
# str(root)
# root_file <- root$make_fix_file()
# root_file <- root$make_fix_file(dirname(whereami::thisfile()))
?devtools::load_all
?devtools::loaded_packages
devtools::loaded_packages()
"terra" %in% search()
if(!isNamespaceLoaded("terra")) devtools::load_all("~/Spatial/terra-master/")
print(getLoadedDLLs())

ns <- getNamespace("terra")
ls(ns)

# CLMS_CLCplus_RASTER_2021 ------------------------------------------------
# setwd(COPERNICUS_DATADIR)
?terra::rast
?terra::window
# https://epsg.io/3035 ETRS89-extended / LAEA Europe
CLMS_CLCplus <- terra::rast(file.path(COPERNICUS_DATADIR, "CLMS_CLCplus_RASTER_2021","CLMS_CLCplus_RASTER_2021_010m_eu_03035_V1_1/CLMS_CLCplus_RASTER_2021_010m_eu_03035_V1_1.tif")
                            # ,win=ext(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1)
                            )
ext(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1) %>% round(0L)
ext(CLMS_CLCplus)
nlyr(CLMS_CLCplus)
res(CLMS_CLCplus)



?terra::cats
(CLMS_CLCplus_cats.df <- cats(CLMS_CLCplus))
names(CLMS_CLCplus)
has.colors(CLMS_CLCplus)

is.factor(CLMS_CLCplus)
table(values(CLMS_CLCplus$Class_name))
# 1        2        3        4        5        6        7        9       10      253      254
# 6253774 33784487  1773101 11984575 66720910 24164168 15057607  9546512   838951   433787 15690771

levels(CLMS_CLCplus)

dput(levels(CLMS_CLCplus))
sealed <- CLMS_CLCplus=="Sealed"
sealed
plot(sealed)
plot(EMSR773_AOI01_DEL_PRODUCT_observedEventA_v1)

?terra::polys
polys(EMSR773_AOI01_DEL_PRODUCT_observedEventA_v1,col="blue", alpha=0.5)

?terra::mask
(sealed_flood_flash <- terra::mask(sealed,EMSR773_AOI01_DEL_PRODUCT_observedEventA_v1))

plot(sealed_flood_flash)
lau_es_crp %>% polys(border="grey")
# ?terra::text
lau_es_crp[muni_is_related,] %>% terra::text("LAU_NAME", cex=0.5, col="black")


plot(CLMS_CLCplus %in% c("Water"), main="Sweet water")
plot(CLMS_CLCplus %in% c("Coastal seawater buffer"), main="Coastal seawater buffer")

# terra::summary(CLMS_CLCplus)

# reclassify --------------------------------------------------------------


table(values(CLMS_CLCplus))
old_codes <- c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
              9L, 10L, 11L, 253L, 254L, 255L)
new_codes <- c(1L
              , 2L, 2L, 2L
              , 5L, 5L, 5L
              , 8L, 8L
              , 10L, 10L
              , 253L, 254L, 255L)
(lookup <- data.frame(is=old_codes
                      ,becomes=new_codes))

rownames(lookup)

new_names <-  c("Developed"
               , rep("Woody",3)
               , rep("Grass/Shrubs",3)
               , rep("Barren/Lichens and mosses",2)
               , rep("Water/Snow and ice",2)
               , "Coastal seawater buffer", "Outside area", "No data"
)

cbind(lookup,new_names)
plot(CLMS_CLCplus)


?classify
CLMS_CLCplus_rc <- CLMS_CLCplus %>% classify(rcl=lookup) %>% as.factor()

dput(unique(data.frame(new_codes,new_names)))

levels(CLMS_CLCplus_rc) <- unique(data.frame(new_codes,new_names))
levels(CLMS_CLCplus_rc)

has.colors(CLMS_CLCplus_rc)
?coltab

coltab(CLMS_CLCplus_rc) <- data.frame(ID=c(1L, 2L, 5L,  8L, 10L, 253L,
                                        254L, 255L)
                                      , color=c("darkgrey"
                                                ,"darkgreen"
                                                , "lightgreen"
                                                ,"orange", "blue","lightblue",NA,NA))


# plot --------------------------------------------------------------------

?terra::coltab
pie(rep(1,14)
#    , col=map.pal("viridis", 14L)
,col=map.pal("viridis", 14)
    )
pie(rep(1,10), col= rev(colorRamps::blue2green(10)))

coltab(CLMS_CLCplus) <-data.frame(id=c(1L
                                       , 2L, 3L, 4L, 5L, 6L, 7L,  9L
                                       , 10L,  253L, 254L), col=c("black"
                                                                  , rep("white",7)
                                                                                                 , "lightblue", "blue", "grey")  )

?terra::plot
debugonce(terra::`coltab<-`)
?colorRamps::blue2green

# lc_png ----

terra::plot(CLMS_CLCplus

#            ,col=data.frame(id=c(1L, 2L, 3L, 4L, 5L, 6L, 7L,  9L, 10L,  253L, 254L), col=c("black", rep("white",10))  )
            , main="Valencia Land Cover")
lau_es %>% terra::polys()
mtext("Data source: https://land.copernicus.eu/", side = 1, line=2, outer = FALSE)
# gadm3_3035 %>% polys(border="grey")
# gadm3_3035 %>% {terra::text(., .$NAME_3, cex=0.5, col="grey")}

lc_png <- file.path(FEMA_WORKDIR, format(Sys.time(),"lc_%Y%m%d_%H%M.png")); print(file.info(lc_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = lc_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(lc_png)['size'])
browseURL(dirname(lc_png))


# developed ---------------------------------------------------------------

?terra::segregate
?terra::subset
coltab(CLMS_CLCplus)
(CLMS_CLCplus_stk <- terra::segregate(CLMS_CLCplus, classes=c(1L
#                                                               , 10L,  253L, 254L
                                                              )
, filename=file.path(FEMA_WORKDIR, "CLMS_CLCplus_stk.tif")))


coltab(CLMS_CLCplus_stk) <- NULL

ext(CLMS_CLCplus_stk)
names(CLMS_CLCplus_stk) <- "Developed"

?terra::summary
summary(CLMS_CLCplus_stk)

?terra::global
CLMS_CLCplus_stk %>% global(fun=base::sum) # number of developed cells in the extent
# sum
# Developed 6253774
?terra::plot
terra::plot(CLMS_CLCplus_stk, y=1L
     ,col=data.frame(value=c(0L,1L), color=c(NA, "black"))
     ,legend=FALSE
)
polys(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1)



# CLMS_CLCplus_stk_crp <- terra::crop(CLMS_CLCplus_stk, EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1 )
# ext(CLMS_CLCplus_stk_crp)

(CLMS_CLCplus_stk_msk <- terra::mask(CLMS_CLCplus_stk, EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1))

(ncells_msk <- CLMS_CLCplus_stk_msk %>% global(fun="notNA", na.rm=TRUE)) # number of cells in the mask
(ncells_msk <- as.numeric(ncells_msk))
# notNA
# Developed 114131688
CLMS_CLCplus_stk_msk %>% global(fun=base::sum, na.rm=TRUE) # number of developed cells in the mask
# sum
# Developed 4793769

# lau_es_AOI1 -------------------------------------------------------------

plot(CLMS_CLCplus_stk_msk, main="Developed cells inside AOI1")
lau_es %>% polys()

?terra::intersect
(lau_es_AOI1 <- terra::intersect(lau_es, EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1))
lau_es_AOI1 %>% plot("POP_DENS_2021")

lau_es_EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg <- terra::intersect(lau_es, EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg)
lau_es_EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg

(out <- terra::zonal(CLMS_CLCplus_stk_msk, lau_es_EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg, fun=base::sum))
out*100

lau_es_EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg$n_developed_cells <- out
lau_es_EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg$developed_area <- out*100
names(lau_es_EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg)

lau_es_EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg[lau_es_EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg$GISCO_ID=="ES_46250",
                                                    c('GISCO_ID',"LAU_NAME" ,"POP_2021" ,         "POP_DENS_2021", "Shape_Area" , "value" ,"n_developed_cells",'developed_area')]

plot(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg, "value")plot(EMSR773_AoutOI01_DEL_PRODUCT_floodDepthA_v1_agg, "value")
polys(lau_es)

?terra::expanse
lau_es_AOI1$expanse <- terra::expanse(lau_es_AOI1)
lau_es_AOI1$expanse_pct <- 100*lau_es_AOI1$expanse/lau_es_AOI1$Shape_Area

names(lau_es_AOI1)
lau_es_AOI1[lau_es_AOI1$GISCO_ID=="ES_46250", c("LAU_NAME",'POP_2021','Shape_Area','expanse','expanse_pct')]
lau_es_AOI1[order(lau_es_AOI1$expanse_pct, decreasing = TRUE), c("LAU_NAME",'POP_2021','Shape_Area','expanse','expanse_pct')] %>% terra::subset(lau_es_AOI1$expanse_pct>=100) %>% plot()

lau_es_AOI1$pop_aoi <- lau_es_AOI1$POP_DENS_2021*lau_es_AOI1$expanse
plot(lau_es_AOI1,"pop_aoi")



?terra::relate

# how many developed cells in the AOI1?
lau_es_AOI1
res(CLMS_CLCplus_stk_msk)
CLMS_CLCplus_stk_msk_lau_es_AOI1 <- terra::zonal(CLMS_CLCplus_stk_msk,lau_es_AOI1, fun=base::sum)
CLMS_CLCplus_stk_msk_lau_es_AOI1
lau_es_AOI1$ndeveloped_cells <- CLMS_CLCplus_stk_msk_lau_es_AOI1
lau_es_AOI1$developed_cells_area <- CLMS_CLCplus_stk_msk_lau_es_AOI1*prod(res(CLMS_CLCplus_stk_msk))
lau_es_AOI1$developed_cells_area_pct <- 100*lau_es_AOI1$developed_cells_area/lau_es_AOI1$Shape_Area
range(lau_es_AOI1$developed_cells_area_pct)

names(lau_es_AOI1)
lau_es_AOI1[lau_es_AOI1$GISCO_ID=="ES_46250",c('GISCO_ID', "LAU_NAME" ,"POP_2021" ,  "Shape_Area" , "expanse_pct" ,"pop_aoi" , "ndeveloped_cells","developed_cells_area")]

# CLMS_CLCplus_stk_msk_stats ----------------------------------------------

(CLMS_CLCplus_stk_msk_stats <- terra::freq(CLMS_CLCplus_stk_msk) %>% as.data.table())
colSums(CLMS_CLCplus_stk_msk_stats); ncells_msk

(nDevelopedCells_msk <- CLMS_CLCplus_stk_msk_stats[value==1, count]) # number of developed cells in the AOI1
# 4793769

# CLMS_CLCplus_stk_msk_stats[, .(sum(count))]*10^2/expanse(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1) # should be one
# terra::expanse(CLMS_CLCplus_stk_msk) # slow

prop.table(CLMS_CLCplus_stk_msk_stats[layer==1, count]) # 4.2% of the area of interest is developed

?terra::crosstab


is.factor(CLMS_CLCplus)
levels(CLMS_CLCplus)


# gadm3_3035 %>% polys(border="grey")


# flood levels -------------------------------------------------------------------

# (EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1 <- terra::project(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1, crs(CLMS_CLCplus)))
EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1
summary(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1)
unique(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value)

?terra::aggregate
# floodDepth <- EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1[, c("value")]

EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value <- as.factor(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value)


?fct_relevel
EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value <- fct_relevel(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value, "Below 0.50" )
levels(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value)

dim(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1)[1] # number of polygons with some level of flood in the AOI!
table(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1$value, useNA = "ifany")

EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1

names(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1)

is.factor(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1)


?terra::zonal
?terra::extract
?terra::classify
?terra::segregate
# (lc_by_flood_depth_detail <- terra::extract(CLMS_CLCplus_stk_msk,EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1, fun=base::sum) )

(lc_by_flood_depth_sum <- terra::zonal(CLMS_CLCplus_stk_msk,EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg, fun=base::sum) )
row.names(lc_by_flood_depth_sum)
(nFloodedDevelopedCells <- colSums(lc_by_flood_depth_sum)) # number of flooded developed cells
# Developed
# 12195

str(lc_by_flood_depth_sum)
lc_by_flood_depth_sum

(lc_by_flood_depth_sum_stats <- lc_by_flood_depth_sum[, .(nFloodedCells=.N, nFloodedDeveloped=sum(Developed))]) # total number of flooded developed cells in the AOI1

CLMS_CLCplus_stk_msk
(lc_by_flood_depth_mean <- terra::zonal(CLMS_CLCplus_stk_msk,EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg, fun=base::mean) %>% magrittr::multiply_by(100) %>%
    as.data.table())

lc_by_flood_depth_mean
?terra::extract

out <- terra::extract(CLMS_CLCplus_stk_msk,EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg)
str(out)
summary(out)
(out.tab <- table(out$ID, out$Developed))
out.tab %>% addmargins()

(out2 <- terra::extract(CLMS_CLCplus_stk_msk,EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg, fun="table"))
str(out2)
print(out2)
