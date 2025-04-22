
# setup -------------------------------------------------------------------

devtools::load_all("~/Spatial/FEMA/femar/")

# states_sf ---------------------------------------------------------------

devtools::load_all("~/Spatial/FEMA/femar/"); (states_sf <- get_states_sf())

states_sf %>% st_geometry() %>% plot(axes=TRUE, graticule=TRUE, reset=TRUE, las=2)
st_bbox(states_sf)
library(tmap)
tmap_mode("view")
states_sf %>%
#  subset(STATEFP=='06') %>%
  qtm()

# us states ----
?states

# (states_sf <- tigris::states(cb=TRUE))
st_crs(states_sf)
#
# # states_vect --------------------------------------------------------------
#
# devtools::load_all("~/Spatial/FEMA/femar/", export_all = TRUE); (states_sf <- states_vect(cb = TRUE))
#
# ?filter
# ?subset
# conus_vect <- states_vect %>% subset(! states_vect$STATEFP %in% c('02','15') & as.integer(states_vect$STATEFP)<60)
# plot(conus_vect)
#
# ?terra::rasterize
# as.lines(conus_vect)
# (r <- rast(conus_vect, res=1))
# (conus.SpatRast <- terra::rasterizeGeom(as.lines(conus_vect),  r, fun="length", "m"))
# plot(conus.SpatRast)
#
# terra::flip(conus.SpatRast, direction="vertical") %>% plot()
# terra::flip(conus.SpatRast, direction="horizontal") %>% plot()

# verify area -------------------------------------------------------------

(states_dt <- states_sf%>% st_drop_geometry() %>% setDT(key='STATEFP'))

(states_stats <- states_dt[, .(STATEFP_COUNT=uniqueN(STATEFP), ALAND=sum(ALAND), AWATER=sum(AWATER))]) %>%as_tibble()

# NRI_states_dt --------------------------------------------------------

devtools::load_all("~/Spatial/FEMA/femar"); debugonce(get_NRI_states_dt); NRI_states_dt <- get_NRI_states_dt()
str(NRI_states_dt)

NRI_states_dt[, .(sum(HRCN_EVNTS, na.rm = TRUE))]


(value_cols <- grep("VALUE$", names(NRI_states_dt), value = TRUE))
NRI_states_dt[, .(STATEFIPS, )]

(valt_cols <- grep("VALT$", names(NRI_states_dt), value = TRUE))


# NRI_states_vect ---------------------------------------------------------

NRI_states_vect <- get_NRI_states_vect()
plot(NRI_states_vect)

# correlations ------------------------------------------------------------

(st_num_cols <- which(sapply(NRI_states_dt, is.numeric)==TRUE))
?cor

(X <- NRI_states_dt[, .SD, .SDcols = st_num_cols])
dim(X)
X.cor <- cor(X)
dim(X.cor)
dimnames(X.cor)
?which
(arr.ind <- which(X.cor>=0.8&X.cor<0.9, arr.ind = TRUE))
i <- arr.ind[,1]
j <- arr.ind[,2]
dimnames(X.cor)[[1]][i]
dimnames(X.cor)[[2]][j]
cor.dt <- data.table(i,dimnames(X.cor)[[1]][i],j,dimnames(X.cor)[[2]][j], cor=X.cor[arr.ind]);setorderv(cor.dt, cols = c('cor'), order = -1L)
print(cor.dt[j!=i], n=999L)
# arrayInd(which(X.cor>0.9), dim(X.cor), useNames = TRUE)
str(arr.ind)
arr.ind[,]
X.cor[arr.ind]



