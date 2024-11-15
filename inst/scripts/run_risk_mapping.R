
# setup -------------------------------------------------------------------
# library(purrr)


# library(sf)
# library(stars)
devtools::load_all("~/Spatial/terra-master/")

findMethods("thinGeom")

?showMethods
showMethods(terra:::thinGeom, includeDefs = TRUE)
# library(terra)

# help("vect",package="terra")
# help(package="tidyterra")

EMSR773_AOI03_BLP.dir <- file.path(COPERNICUS_DATADIR, "EMSR773_AOI03_BLP"  ); stopifnot(dir.exists(EMSR773_AOI03_BLP.dir))
# browseURL(EMSR773_AOI03_BLP.dir)

# summary tables ----------------------------------------------------------
list.files(COPERNICUS_DATADIR)
(summaryTable.list <- list.files(COPERNICUS_DATADIR, pattern = ".*AOI03.*\\.xlsx", recursive = TRUE,full.names = TRUE))

browseURL(summaryTable.list[1])
browseURL(summaryTable.list[2])

EMSR773_AOI03_DEL_PRODUCT_summaryTable_v1_xlsx <- file.path(COPERNICUS_DATADIR,"EMSR773_AOI03_DEL_PRODUCT_summaryTable_v1.xlsx")
browseURL(EMSR773_AOI03_DEL_PRODUCT_summaryTable_v1_xlsx)

# vectors ------------------------------------------------------------------

vectors <- c( "EMSR773_AOI03_BLP_PRODUCT_areaOfInterestA_v1"
              ,'EMSR773_AOI03_BLP_PRODUCT_builtUpA_v1', 'EMSR773_AOI03_BLP_PRODUCT_builtUpP_v1'
              , 'EMSR773_AOI03_BLP_PRODUCT_facilitiesA_v1', 'EMSR773_AOI03_BLP_PRODUCT_facilitiesL_v1'
              , 'EMSR773_AOI03_BLP_PRODUCT_hydrographyA_v1', 'EMSR773_AOI03_BLP_PRODUCT_hydrographyL_v1', 'EMSR773_AOI03_BLP_PRODUCT_naturalLandUseA_v1', 'EMSR773_AOI03_BLP_PRODUCT_physiographyL_v1', 'EMSR773_AOI03_BLP_PRODUCT_transportationA_v1', 'EMSR773_AOI03_BLP_PRODUCT_transportationL_v1' )
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

terra::plot(svc3, pch=".")

fsummary_vect <- purrr::compose(fstutils::fsummary, terra::as.data.frame)

svc3_summary.list <- lapply(as.list(svc3), fsummary_vect)
svc3_summary.list

writex
# ?purrr::lmap
#
# lmap(out.list, function(x) terra::plot(x[[1]], main=names(x)[1]))



# built-up area and points -----------------------------------------------------------
names(svc3)
builtUpA <- as.list(svc3)[['builtUpA']]
fstutils::fsummary(as.data.frame(builtUpA))

(builtUpP <- as.list(svc3)[['builtUpP']])
fstutils::fsummary(as.data.frame(builtUpP))
names(builtUpP)

builtUpP$simplified <- factor(builtUpP$simplified)
str(builtUpP$simplified)
?fct_collapse
# dput(levels(builtUpP$simplified))
builtUpP$simplified2 <- fct_collapse(builtUpP$simplified
                                     , Commercial="Non residential"
                                     , Services=c("Fire station", "Historic or protected monuments", "Hospital or institutional care buildings",  "School, university and research buildings")
                                     , Other=c("Not Applicable", "Unclassified", "Reservoirs, silos and warehouses")) %>%
  fct_relevel("Residential","Commercial","Services","Other")
table(builtUpP$simplified2)

str(builtUpP$simplified)
# EMSR773_AOI03_BLP_PRODUCT_png ----


opar <- par(mfrow=c(1,1))

terra::plot( builtUpA
             , border="grey"
             #            , legend=TRUE
             , alpha=0.5
             , main="EMSR773_AOI03 built-up"
)
?terra::plot
# ?terra::points

# undebug(terra::points)
# debugonce(graphics::plot.xy)
?switch
?dplyr::case_match
builtUpP$simplified2
?graphics::legend
debugonce(graphics::legend)

col <- dplyr::case_match(builtUpP$simplified2
                  , "Residential"~"green"
                  ,"Commercial"~"magenta"
                  ,"Services"~"blue"
                  , .default = "grey"
                  )
table(builtUpP$simplified2)
table(col)

terra::plot(builtUpP, "simplified2"
            #
            #            , legend=TRUE
            , plg=list(cex=1,  pt.cex = 1, trace=TRUE)
            , sort=c("Residential","Commercial","Services","Other")
            , type="classes"
            , col=c("green","magenta","blue","grey")
            # ,add=TRUE
            #, pch='.'
            , cex=0.1
            , alpha=0.5)
?terra::add_legend
?graphics::legend
# terra::add_legend("topleft", legend=levels(builtUpP$simplified)
#                   ,col= 1:nlevels(builtUpP$simplified)
#                   ,pch=1
#                   , cex=0.5
#                   ,bty="n")
warnings()
terra::polys(areaOfInterestA <- out.list$EMSR773_AOI03_BLP_PRODUCT_areaOfInterestA_v1)

?terra::add_mtext

par(opar)


EMSR773_AOI03_BLP_PRODUCT_png <- file.path(the$FEMA_WORKDIR, format(Sys.time(),"EMSR773_AOI03_BLP_PRODUCT_%Y%m%d_%H%M.png")); print(file.info(EMSR773_AOI03_BLP_PRODUCT_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = EMSR773_AOI03_BLP_PRODUCT_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(EMSR773_AOI03_BLP_PRODUCT_png)['size'])
browseURL(dirname(EMSR773_AOI03_BLP_PRODUCT_png))

# EMSR773_AOI01_DEL_MONIT03_v1 --------------------------------------------

EMSR773_AOI01_DEL_MONIT03_v1.dir <- file.path(COPERNICUS_DATADIR, "EMSR773_AOI01_DEL_MONIT03_v1"); stopifnot(dir.exists(EMSR773_AOI01_DEL_MONIT03_v1.dir))

