
# setup -------------------------------------------------------------------
# library(purrr)
devtools::load_all("~/Spatial/FEMA/femar/", export_all = TRUE)
ls.str(the)
# library(sf)
# library(stars)

# devtools::load_all("~/Spatial/terra-master/")

xtabs_obj_type <- purrr::compose(~xtabs(~ obj_type , .x), terra::as.data.frame)
xtabs_obj_type_info <- purrr::compose(~xtabs(~ obj_type + info , .x), terra::as.data.frame)

# findMethods("thinGeom")
#
# ?showMethods
# showMethods(terra:::thinGeom, includeDefs = TRUE)
# library(terra)

# help("vect",package="terra")
# help(package="tidyterra")

EMSR773_AOI03_BLP.dir <- file.path(the$COPERNICUS_DATADIR, "EMSR773_AOI03_BLP"  ); stopifnot(dir.exists(EMSR773_AOI03_BLP.dir))
# browseURL(EMSR773_AOI03_BLP.dir)

# summary tables ----------------------------------------------------------
list.files(the$COPERNICUS_DATADIR)
(summaryTable.list <- list.files(the$COPERNICUS_DATADIR, pattern = ".*AOI03.*\\.xlsx", recursive = TRUE,full.names = TRUE))

browseURL(summaryTable.list[1])
browseURL(summaryTable.list[2])

EMSR773_AOI03_DEL_PRODUCT_summaryTable_v1_xlsx <- file.path(the$COPERNICUS_DATADIR,"EMSR773_AOI03_DEL_PRODUCT_summaryTable_v1.xlsx")
browseURL(EMSR773_AOI03_DEL_PRODUCT_summaryTable_v1_xlsx)

# vectors ------------------------------------------------------------------

vectors <- c( "EMSR773_AOI03_BLP_PRODUCT_areaOfInterestA_v1"
              ,'EMSR773_AOI03_BLP_PRODUCT_builtUpA_v1', 'EMSR773_AOI03_BLP_PRODUCT_builtUpP_v1'
              , 'EMSR773_AOI03_BLP_PRODUCT_facilitiesA_v1', 'EMSR773_AOI03_BLP_PRODUCT_facilitiesL_v1'
              , 'EMSR773_AOI03_BLP_PRODUCT_hydrographyA_v1', 'EMSR773_AOI03_BLP_PRODUCT_hydrographyL_v1', 'EMSR773_AOI03_BLP_PRODUCT_naturalLandUseA_v1', 'EMSR773_AOI03_BLP_PRODUCT_physiographyL_v1'
              , 'EMSR773_AOI03_BLP_PRODUCT_transportationA_v1', 'EMSR773_AOI03_BLP_PRODUCT_transportationL_v1' )
print(vectors)

# ?SpatVectorCollection
# debugonce(terra::svc)
# ?svc
# ?findMethods
# svc.listOfMethods <- findMethods("svc")
# findMethodSignatures(methods = svc.listOfMethods)
# svc.env <- getMethodsForDispatch("svc")
# # EMSR773_AOI03_BLP.svc <- terra::svc(x = file.path(EMSR773_AOI03_BLP.dir), layer=vectors, extent=NULL)
#
# ?terra::vect
# ?purrr::map
# ?terra::saveRDS
# help("wrap",package="terra")
# sv.wrapped.list <- lapply(sv.list, terra::wrap)
#
# svc3 <- purrr::map(vectors, ~terra::wrap(terra::vect(EMSR773_AOI03_BLP.dir, .x))) %>%
#   terra::svc()
# class(svc3)
# methods(class="SpatVectorCollection")
# names(svc3)

# svc3_rds ----
devtools::load_all("~/Spatial/FEMA/femar/", export_all = FALSE); svc3 <- get_svc(EMSR773_AOI03_BLP.dir,vectors)

# terra::plot(svc3, pch=".")
lapply(svc3, names)
fsummary_vect <- purrr::compose(fstutils::fsummary, terra::as.data.frame)

svc3_summary.list <- lapply(as.list(svc3), terra::as.data.frame)
svc3_summary.list
str(svc3_summary.list)
svc3_summary.list[[1]]

?vctrs::vec_as_names
tibble::as_tibble(svc3_summary.list[[2]], .name_repair = "universal" )
?broom::tidy
svc3_summary <- broom::tidy(svc3_summary.list[[2]])
as.data.frame(svc3_summary.list[[2]])
svc3_summary_xlsx <- file.path(the$FEMA_WORKDIR, "svc3_summary.xlsx")

writexl::write_xlsx(svc3_summary.list, path = svc3_summary_xlsx)
# ?purrr::lmap
#
# lmap(out.list, function(x) terra::plot(x[[1]], main=names(x)[1]))


# area of interest --------------------------------------------------------

areaOfInterestA <- svc3[["areaOfInterestA"]]
writeLines(terra::crs(areaOfInterestA))
# ?terra::project
# areaOfInterestA %>%  terra::project( "EPSG:3035")
# terra::project(areaOfInterestA, "EPSG:3035")

# built-up area  -----------------------------------------------------------
names(svc3)
(builtUpA <- svc3[['builtUpA']])
fstutils::fsummary(as.data.frame(builtUpA))

builtUpA %>% subset(builtUpA$obj_type=="11-Residential Buildings")

builtUpA_msk <- terra::mask(builtUpA %>% subset(builtUpA$obj_type=="11-Residential Buildings"), areaOfInterestA)
?terra::plot
terra::plot( builtUpA_msk
             , border="grey"
             #            , legend=TRUE
             , alpha=0.5
             , main="EMSR773_AOI03 built-up area Residential Buildings"
)

# built-up points -----------------------------------------------------------

(builtUpP <- svc3[['builtUpP']])
fstutils::fsummary(as.data.frame(builtUpP))
names(builtUpP)
builtUpP$simplified <- factor(builtUpP$simplified)
str(builtUpP$simplified)

builtUpP %>% subset(builtUpP$obj_type=="11-Residential Buildings") %>% xtabs_obj_type_info()

builtUpP_msk <- terra::mask(builtUpP %>% subset(builtUpP$obj_type=="11-Residential Buildings"), areaOfInterestA)



# ?fct_collapse
# # dput(levels(builtUpP$simplified))
# builtUpP$simplified2 <- fct_collapse(builtUpP$simplified
#                                      , Commercial="Non residential"
#                                      , Services=c("Fire station", "Historic or protected monuments", "Hospital or institutional care buildings",  "School, university and research buildings")
#                                      , Other=c("Not Applicable", "Unclassified", "Reservoirs, silos and warehouses")) %>%
#   fct_relevel("Residential","Commercial","Services","Other")
# table(builtUpP$simplified2)

terra::plot(builtUpP_msk, pch=20, cex=0.5)

# EMSR773_AOI03_BLP_PRODUCT_png ----


opar <- par(mfrow=c(1,1))


?terra::plot
# ?terra::points

# undebug(terra::points)
# debugonce(graphics::plot.xy)
# ?switch
# ?dplyr::case_match
# builtUpP$simplified2
# ?graphics::legend
# debugonce(graphics::legend)

# col <- dplyr::case_match(builtUpP$simplified2
#                   , "Residential"~"green"
#                   ,"Commercial"~"magenta"
#                   ,"Services"~"blue"
#                   , .default = "grey"
#                   )
# table(builtUpP$simplified2)
# table(col)

terra::plot(builtUpP_msk
            #
            #            , legend=TRUE
            , plg=list(cex=1,  pt.cex = 1, trace=TRUE)
            #            , sort=c("Residential","Commercial","Services","Other")
            , type="classes"
            #            , col=c("green","magenta","blue","grey")
            # ,add=TRUE
            #, pch='.'
            , cex=0.1
            #            , alpha=0.5
            ,add=TRUE
)
lau_es <- as(lau_es, "SpatVector")
terra::polys(lau_es, border="grey")
lau_es_msk <- terra::mask(lau_es, areaOfInterestA)
terra::text(lau_es_msk, label=lau_es$LAU_NAME, cex=0.5)
# ?terra::add_legend
# ?graphics::legend
# terra::add_legend("topleft", legend=levels(builtUpP$simplified)
#                   ,col= 1:nlevels(builtUpP$simplified)
#                   ,pch=1
#                   , cex=0.5
#                   ,bty="n")
warnings()
terra::polys(areaOfInterestA <- svc3[["areaOfInterestA"]])

?terra::add_mtext

par(opar)

# physiographyL -----------------------------------------------------------

(physiographyL <- svc3[["physiographyL"]])

physiographyPoly <- terra::as.polygons(physiographyL)
physiographyL$elev <- as.integer(physiographyL$elev)

physiographyL_msk <- terra::mask(physiographyL, areaOfInterestA)


?terra::plot
terra::plot(physiographyL_msk, "elev"
            #            , type= "continuous"
            ,plg=list(title="Elevation (m)")
            #            , add=TRUE
)

?terra::cont

# hidro_png ----

hidro_png <- file.path(the$FEMA_WORKDIR, format(Sys.time(),"hidro_%Y%m%d_%H%M.png")); print(file.info(hidro_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = hidro_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(hidro_png)['size'])
browseURL(dirname(hidro_png))

# transportationL ---------------------------------------------------------

(transportationL <- svc3[["transportationL"]])

transportationL_df <- terra::as.data.frame(transportationL)
# xtabs(~ obj_type  +info , transportationL_df)
transportationL %>% xtabs_obj_type_info()



svc3_xtabs.list <- lapply(as.list(svc3)[-1], xtabs_obj_type)
print(svc3_xtabs.list)

# library(tidyterra)
# help(package="tidyterra")
#
# ?terra::`Summary,SpatVector-method`
# terra::Summary(transportationL)
# showMethods(Summary)

EMSR773_AOI03_BLP_PRODUCT_png <- file.path(the$FEMA_WORKDIR, format(Sys.time(),"EMSR773_AOI03_BLP_PRODUCT_%Y%m%d_%H%M.png")); print(file.info(EMSR773_AOI03_BLP_PRODUCT_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = EMSR773_AOI03_BLP_PRODUCT_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(EMSR773_AOI03_BLP_PRODUCT_png)['size'])
browseURL(dirname(EMSR773_AOI03_BLP_PRODUCT_png))

# EMSR773_AOI01_DEL_MONIT03_v1 --------------------------------------------

EMSR773_AOI01_DEL_MONIT03_v1.dir <- file.path(COPERNICUS_DATADIR, "EMSR773_AOI01_DEL_MONIT03_v1"); stopifnot(dir.exists(EMSR773_AOI01_DEL_MONIT03_v1.dir))

