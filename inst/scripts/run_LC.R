
# setup -------------------------------------------------------------------
# rm(list = ls())
source("~/.RProfile", echo=TRUE)
?removeGeneric
if(isGeneric("not.na")) removeGeneric("not.na")

if(!isNamespaceLoaded("terra")) devtools::load_all("~/Spatial/terra-master/")
print(getLoadedDLLs())

ns <- getNamespace("terra")
ls(ns)

# CLMS_CLCplus_RASTER_2021 ------------------------------------------------
setwd(COPERNICUS_DATADIR)
?terra::rast
?terra::window
# https://epsg.io/3035 ETRS89-extended / LAEA Europe
CLMS_CLCplus <- terra::rast(file.path(COPERNICUS_DATADIR, "CLMS_CLCplus_RASTER_2021","CLMS_CLCplus_RASTER_2021_010m_eu_03035_V1_1/CLMS_CLCplus_RASTER_2021_010m_eu_03035_V1_1.tif")
                            ,win=ext(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1))
ext(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1) %>% round(0L)
ext(CLMS_CLCplus)

names(CLMS_CLCplus)
has.colors(CLMS_CLCplus)

is.factor(CLMS_CLCplus)
levels(CLMS_CLCplus)

dput(levels(CLMS_CLCplus))


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


plot(CLMS_CLCplus)
gadm3_3035 %>% polys(border="grey")
gadm3_3035 %>% {terra::text(., .$NAME_3, cex=0.5, col="grey")}


# developed ---------------------------------------------------------------
summary(CLMS_CLCplus)
?segregate
(CLMS_CLCplus_stk <- segregate(CLMS_CLCplus, classes=1L))
names(CLMS_CLCplus_stk) <- "Developed"

?terra::summary
summary(CLMS_CLCplus_stk)

?terra::global
CLMS_CLCplus_stk %>% global(fun=base::sum) # number of developed cells in the extent
# sum
# Developed 6253774
plot(CLMS_CLCplus_stk,col=data.frame(value=c(0L,1L), color=c(NA, "grey"))
     ,legend=FALSE, main="Developed")
polys(EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1)

ext(CLMS_CLCplus_stk)

# CLMS_CLCplus_stk_crp <- terra::crop(CLMS_CLCplus_stk, EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1 )
# ext(CLMS_CLCplus_stk_crp)

(CLMS_CLCplus_stk_msk <- terra::mask(CLMS_CLCplus_stk, EMSR773_AOI01_DEL_PRODUCT_areaOfInterestA_v1))
CLMS_CLCplus_stk_msk %>% global(fun=base::sum, na.rm=TRUE) # number of developed cells in the mask
# sum
# Developed 4793769

(ncells_msk <- CLMS_CLCplus_stk_msk %>% global(fun="notNA", na.rm=TRUE)) # number of cells in the mask
(ncells_msk <- as.numeric(ncells_msk))
# notNA
# Developed 114131688

plot(CLMS_CLCplus_stk_msk, main="Developed cells inside AOI1")

# how many developed cells in the AOI1?

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


gadm3_3035 %>% polys(border="grey")


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

# aggredate flood levels ---------------------------------------------------
EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1
?terra::aggregate


(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg <- aggregate(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1[,"value"], by="value", fun=base::length))
EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg
EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg%>% as.data.frame()

EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg %>% plot("value")
EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1_agg$agg_n %>% sum() ; dim(EMSR773_AOI01_DEL_PRODUCT_floodDepthA_v1)[1] # number of polygons with some level of flood in the AOI!

?terra::zonal
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
