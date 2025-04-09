# setup ----
source("~/Spatial/Tigris/.RProfile")

options(rgdal_show_exportToProj4_warnings="none")
library(rgdal)
# library(tmap)

library(raster) # needed for plot(ca)
library(sf); help(package="sf")
library(rgeos)

options(sf_max.plot=1L)

# file.show('~/.Renviron')
readRenviron('~/.Renviron')
cat("CENSUS_KEY:",Sys.getenv("CENSUS_KEY"))
cat("TIGRIS_CACHE_DIR:", Sys.getenv('TIGRIS_CACHE_DIR'))
# if(! dir.exists(Sys.getenv('TIGRIS_CACHE_DIR')))
#   dir.create(Sys.getenv('TIGRIS_CACHE_DIR'))

source("~/Spatial/Tigris/.RProfile")
devtools::load_all("~/Spatial/FEMA/femar/")
# browseURL(the$CENSUS_WORKDIR)

# Lamberts Conical Projection ---------------------------------------------

lcc <- "+proj=lcc +lat_1=60 +lat_2=30 +lon_0=-60"
print(st_crs(lcc)$units)

# coastline_sf --------------------------------------------------------------
options(tigris_year=2020L)
getOption("tigris_year")
?tigris::coastline
?load_tiger
devtools::load_all("~/Spatial/FEMA/femar/"); coastline_sf <- get_coastline_sf(crs=3857)

?aggregate.sf
sf <- coastline_sf%>% subset(NAME=="Gulf")

out2_sf <- st_combine(sf)
out2_sf

out3_sf <- st_union(sf)
out3_sf

debugonce(aggregate)
?st_combine
?st_join
?by
help(package="sf")

?st_intersects
lst <- st_intersects(counties_sf,coastline_sf, sparse = TRUE)
dim(counties_sf);dim(coastline_sf);  dim(lst)
as.data.frame(lst)

length(lst)

str(lst[1:10L])
counties_sf[10,]
dev.off()

i <- which(counties_sf$STCOFIPS=="06037")

LA_sf <- counties_sf[i,]
plot(st_geometry(LA_sf), add=FALSE, reset=FALSE)
coastline_sf[lst[[i]],] %>% st_geometry() %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE, add=TRUE, col="blue")

coastline_sf[lst[[i]],] %>% st_geometry() %>% st_length() %>% sum()

?lapply

print(coastal_length)

?st_distance



imin <- which(out==min(out))
coastline_sf[imin,]

coastline_sf %>% st_geometry() %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE, las=2)
plot(st_geometry(LA_sf), add=TRUE)
plot(st_centroid(st_geometry(LA_sf)), add=TRUE)

st_intersection(counties_sf,coastline_sf)
# coastline_sf %>% subset(NAME %in% c("Atlantic","Atlántico","Caribbean","Gulf"), select="NAME") %>% plot(axes=TRUE, graticule=TRUE, reset=TRUE)


# coastline_sf %>% subset(NAME=="Atlántico") %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)

library(tmap)
tmap_mode("view")
coastline_sf %>% subset(NAME=="Gulf") %>% qtm()+tm_title(text="Gulf coastline")


coastline_sf %>% subset(NAME=="Atlántico") %>% qtm() # North shore of PR
coastline_sf %>% subset(NAME=="Caribe") %>% qtm() # South shore of PR

coastline_sf %>% subset(NAME=="Pacific") %>% qtm()+tm_title(text="Pacific coastline") # South of VI

coastline_sf %>% subset(NAME=="Caribbean") %>% qtm()+tm_title(text="Caribbean coastline") # South of VI

coastline_sf %>% subset(NAME=="Atlantic") %>% qtm()+tm_title(text="Atlantic coastline") # From Florida Keys to Maine, includes North of VI

(ATL.coastline_sf <- coastline_sf%>% subset(NAME=="Atlantic") %>% st_combine() )%>% plot()
st_buffer(ATL.coastline_sf, dist) %>% plot()


# coastline buffered ------------------------------------------------------
sf_use_s2()
coastline_lcc_sf <- coastline_sf %>% st_transform(st_crs(lcc))
dist <- 100; units(dist) <- "km"
?st_buffer
(coastline_lcc_dist_sf <- st_buffer(coastline_lcc_sf, dist = dist))
plot(coastline_lcc_sf['NAME'], reset=FALSE)
plot(st_geometry(coastline_lcc_dist_sf), col="grey", alpha=0.5, add=TRUE)
# #rename Caribe into Atlántico to avoid double counting of PR
# coastline_sf$NAME <- ifelse(coastline_sf$NAME=="Caribe", "Atlántico", coastline_sf$NAME)

# coastline_lst -----------------------------------------------------------

coastline_lst <- split(coastline_sf, f = coastline_sf$NAME)
str(coastline_lst)
st_crs(states_sf)

?st_combine
(ATL_coastline_sf <- coastline_lst[['Atlantic']] %>% st_geometry() %>% st_combine()) %>% plot()
(Pacific_coastline_sf <- coastline_lst[['Pacific']] %>% st_geometry() %>% st_combine()) %>% plot()


# states.coastline_sf -----------------------------------------------------

st_crs(states_sf)

(tmp_df <- lapply(coastline_lst, . %>% st_intersects(x=states_sf,y=.) %>% (function(x) lengths(x)>0)) %>% as.data.frame() )

(states.coastline_sf <- cbind(states_sf, tmp_df))

states.coastline_sf %>% subset(Atlantic==TRUE | Atlántico==TRUE, 'STUSPS') %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)

# counties.coastline_sf -----------------------------------------------------


(counties_sf <- counties(cb=TRUE) %>% st_transform( st_crs(3857)))
(tmp_df <- lapply(coastline_lst, . %>% st_intersects(x=counties_sf,y=.) %>% (function(x) lengths(x)>0)) %>% as.data.frame() )

(counties.coastline_sf <- cbind(counties_sf, tmp_df))

counties.coastline_sf %>% subset(Atlantic==TRUE | Atlántico==TRUE, 'STUSPS') %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)


# tracts.coastline_sf -----------------------------------------------------

?load_tiger
(tracts_sf <- tracts(cb=TRUE,keep_zipped_shapefile = TRUE) %>% st_transform( st_crs(3857)))
row.names(NRI_tracts_sf) <- NRI_tracts_sf$TRACTFIPS
?lapply

?st_distance

(tmp_df <- lapply(coastline_lst, . %>% st_intersects(x=NRI_tracts_sf,y=.) %>% (function(x) lengths(x)>0)) %>% as.data.frame() )
row.names(tmp_df)

setDT(tmp_df)
melt.data.table(tmp_df, )

(tracts.coastline_sf <- cbind(tracts_sf, tmp_df))

tracts.coastline_sf %>% subset(Atlantic==TRUE | Atlántico==TRUE, 'STUSPS') %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)


names(states_sf)

as.data.frame(states.coastline_lst)

out <- map(seq_along(states.coastline_lst), function(i) {
  browser();
  coast.name <- names(states.coastline_lst)[i]
  coast.bool <- lengths(states.coastline_lst[[i]])>0
  states_sf[[coast.name]] <- coast.bool
  return(table(coast.bool))
})

dev.off()
ATL.coastline_sf  %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)
ATL.coastline_sf %>% st_buffer(dist) %>% plot(add=TRUE)

coastline_multiline_lst <- lapply(coastline_lst, st_combine)
coastline_multiline_lst

lapply(coastline_multiline_lst[[1]] , plot)

# cbsa_agg_sf <- aggregate(cbsa_sf['ALAND'], coastline_sf, sum)
# cbsa_agg_sf

coastline_sfg <- coastline_sf %>% group_by(NAME)
# coastline_sfg %>%



str(coastline_lst, max.level=1L)
names(coastline_lst)
coastline_lst[[]]
process_coastline <- function(coastline_sf,cbsa_sf,verbose=FALSE) {
  # browser()
  print(table(coastline_sf$NAME))
  # transform to Lamber Conical Confocal
  coastline_lcc_sf <- coastline_sf %>% st_transform(st_crs(lcc))
  cbsa_lcc_sf <- cbsa_sf %>% st_transform(st_crs(lcc))

  # plot(st_geometry(coastline_lcc_sf), add=TRUE, lwd=2, col="blue")
  # buffer
  cbsa.coastline_lcc_lst <- st_intersects(cbsa_lcc_sf, coastline_lcc_sf)
  print(sum(lengths(cbsa.coastline_lcc_lst)>0))
  if(sum(lengths(cbsa.coastline_lcc_lst))==0) return(NULL)
  cbsa.coastline_lcc_sf  <- cbsa_lcc_sf[lengths(cbsa.coastline_lcc_lst)>0,]
  # merge with last_mf_stats
  cbsa.coastline_lcc_last_mf_sf<- merge(cbsa.coastline_lcc_sf, last_mf_stats_by_cbsa, by='CBSAFP')
  if(verbose){
    cbsa.coastline_lcc_last_mf_sf %>% summarise(sum(ACTIVE_UPB)) %>% plot(main=unique(coastline_sf$NAME)
                                                                          ,axes=TRUE, graticule=TRUE, reset=FALSE)
  }
  return(cbsa.coastline_lcc_last_mf_sf)
}



cbsa_lcc_buff_sf %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)
cbsa.coast_lcc_buff_last_mf.lst <- lapply(coastline_lst, process_coastline, cbsa_lcc_buff_sf)
?rbind.sf
cbsa.coast_lcc_buff_last_mf_sf <- do.call("rbind",cbsa.coast_lcc_buff_last_mf.lst)
cbsa.coast_lcc_buff_last_mf_sf
cbsa.coast_lcc_buff_last_mf_sf <- cbsa.coast_lcc_buff_last_mf_sf[!duplicated(cbsa.coast_lcc_buff_last_mf_sf, by=CBSAFP),]

cbsa.coast_lcc_buff_last_mf_sf$COAST.NAME <- rownames(cbsa.coast_lcc_buff_last_mf_sf) %>%stringi::stri_extract_first_regex(pattern="\\w+")
cbsa.coast_buff_last_mf_sf <- cbsa.coast_lcc_buff_last_mf_sf%>%st_transform(st_crs(4269))


cbsa.coast_buff_last_mf_sf %>% subset(COAST.NAME=="Atlantic")

cbsa.coast_buff_last_mf_sf$ACTIVE_UPB_BILLIONS=cbsa.coast_buff_last_mf_sf$ACTIVE_UPB/1e9
cbsa.coast_buff_last_mf_sf %>% subset(COAST.NAME=="Atlantic", select='ACTIVE_UPB_BILLIONS') %>%
  plot(axes=TRUE, graticule=TRUE, reset=FALSE
       #                                                                        , xlim=c(-130,-70), ylim=c(20,50)
       ,main="FNMA FM Active UBP in Atlantic Coast"
  )




(cbsa.coast_buff_last_mf_agg_sf <- cbsa.coast_buff_last_mf_sf %>% group_by(COAST.NAME) %>% summarise(ACTIVE_UPB_BILLIONS=sum(ACTIVE_UPB)/1e9) )
print(cbsa.coast_buff_last_mf_agg_sf)

cbsa.coast_lcc_buff_last_mf_agg_sf <- cbsa.coast_lcc_buff_last_mf_agg_sf
?plot.sf


# cbsa.coast_buff_last_mf_agg_png ----

cbsa.coast_buff_last_mf_agg_png <- file.path(.fn_mf_workdir, format(Sys.time(),"cbsa.coast_buff_last_mf_agg_%Y%m%d_%H%M.png")); print(file.info(cbsa.coast_buff_last_mf_agg_png))
library(Cairo)
Cairo::CairoPNG(filename = cbsa.coast_buff_last_mf_agg_png, width = 10.0, height = 6.0, dpi=300, units="in")



cbsa.coast_buff_last_mf_agg_sf %>%
  plot(axes=TRUE, graticule=TRUE, reset=FALSE , xlim=c(-130,-70), ylim=c(20,50)
       ,main="FNMA MF Active UPB ($B) by Coast Name"
  )
plot(st_geometry(coastline_sf),add=TRUE, col="blue", lwd=2)
plot(st_geometry(cbsa.nocoastline_last_mf_sf), add=TRUE, col="grey")
plot(st_geometry(states_sf),add=TRUE, lwd=2)
dev.off()
print(file.info(cbsa.coast_buff_last_mf_agg_png))
browseURL(dirname(cbsa.coast_buff_last_mf_agg_png))

cbsa.nocoastline_last_mf_sf <- cbsa_last_mf_sf %>% subset(! cbsa_last_mf_sf$CBSAFP %in% cbsa.coast_buff_last_mf_sf$CBSAFP) %>%
  summarize(ACTIVE_UPB_BILLIONS=sum(ACTIVE_UPB)/1e9)
cbsa.nocoastline_last_mf_sf
cbsa.coast_buff_last_mf_agg_sf
cbsa.coast_lcc_buff_last_mf_agg_sf %>% summarise(sum(ACTIVE_UPB_BILLIONS))
67 #Non-MSA or Multiple MSA
245 # Coastline
141 # interior
67+245+141

str(cbsa.coast_lcc_buff_last_mf.lst, max.level=1L)
plot(st_geometry(coastline_sf))

(coastline_sf <- coastline_sf %>% subset(! NAME %in% c('Great Lakes','Pacific','Arctic') ))
plot(st_geometry(coastline_sf))

table(coastline_sf$NAME)





# coastal states ----------------------------------------------------------
?st_intersects
print(table(coastline_sf$NAME))


(states.coastline.lst <- st_intersects(states_sf, coastline_sf#%>%subset(NAME %in% c("Atlantic","Atlántico","Caribbean","Gulf"))
                                       , sparse = TRUE))
print(states.coastline.lst)
lengths(states.coastline.lst) > 0

states_sf%>% subset( lengths(states.coastline.lst) > 0, 'STATEFP')

(states.coastline_sf <- states_sf[lengths(states.coastline.lst) > 0,])
?plot.sf

?tm_shape
states.coastline_sf['STUSPS'] %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE
                                       , key.pos=NULL
                                       # ,key.width = lcm(7.67)
)

print(states.coastline_sf$NAME)

# coastal states ----------------------------------------------------------
?st_intersects
states.coastline.dist.lst <- st_is_within_distance(states_sf, coastline_sf%>%
                                                     subset(NAME %in% c("Atlantic","Atlántico","Caribbean","Gulf"))
                                                   ,dist=dist
                                                   , sparse = TRUE)
?lengths
base::lengths(states.coastline.dist.lst) > 0

(states.coastline.dist_sf <- states_sf[lengths(states.coastline.dist.lst) > 0,])
?plot.sf
dev.off()
states.coastline.dist_sf['STUSPS'] %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE
                                            , key.pos=NULL
                                            # ,key.width = lcm(7.67)
)
print(states.coastline_sf$NAME)

# coastal cbsa --------------------------------------------------------
table(coastline_sf$NAME)
# coastline_sf <- coastline_sf %>% subset(NAME %in% c('Atlantic','Atlántico','Caribbean','Caribe','Gulf'))

(cbsa.coastline_lst <- st_intersects(cbsa_sf, coastline_sf %>% subset(NAME %in% c('Atlantic','Atlántico','Caribbean','Caribe','Gulf'))))
stopifnot(sum(lengths(cbsa.coastline_lst)>0)+sum(lengths(cbsa.coastline_lst)==0)==nrow(cbsa_sf))

(cbsa.coastline_sf <- cbsa_sf[lengths(cbsa.coastline_lst)>0,])
cbsa.coastline_sf

plot(cbsa.coastline_sf['NAME'], main='CBSAs on the Gulf, Caribbean and Atlantic coastline', axes=TRUE, graticule=TRUE, reset=FALSE
     , key.pos=NULL)
plot(st_geometry(coastline_sf), add=TRUE, col="black")
nrow(cbsa_sf)

# coastal cbsa buffered -----------------------------------------------
sf_use_s2(TRUE)
?st_intersects
(cbsa.coastline_dist_lst <- st_is_within_distance(cbsa_sf, coastline_sf, dist = dist))

par(mfrow=c(1,1))
(cbsa.coastline_dist_sf  <- cbsa_sf[lengths(cbsa.coastline_dist_lst)>0,]) %>%
  #  subset(subset=STATEFP!="02" & STATEFP !="15" & STATEFP<60) %>%
  st_geometry() %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE, main='CBSAs 100km from the Gulf, Caribbean and Atlantic coastline')

source("~/Spatial/Tigris/states.R")
plot(st_geometry(states.coastline_sf), add=TRUE, col="grey", alpha=0.9)

st_crs(states_sf)
cbsa.coastline_sf <- st_transform(cbsa.coastline_sf, st_crs(4269))

par(mfrow=c(1,1))
st_bbox(cbsa.coastline_sf)
cbsa.coastline_sf['NAME'] %>% subset(grepl("PR$", cbsa.coastline_sf$NAME)==FALSE) %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)

grep("PR$", cbsa.coastline_sf$NAME, invert = TRUE)
