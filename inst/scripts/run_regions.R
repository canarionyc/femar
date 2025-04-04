unloadNamespace("datasets")

?tigris::regions
regions_sf <- tigris::regions()

plot(st_geometry(regions_sf))


# states_sf ---------------------------------------------------------------
states_sf <- states(cb=FALSE)
subset(states_sf, REGION==1, select=c('STUSPS', 'NAME' )) # North East
subset(states_sf, REGION==2, select=c('STUSPS', 'NAME' )) # North West
subset(states_sf, REGION==3, select=c('STUSPS', 'NAME' )) # South East

# South West - Hawaii
subset(states_sf, REGION==4) %>% subset(! STATEFP %in% c('02','15') & as.integer(STATEFP)<60) %>%
  st_geometry() %>% plot(axes=TRUE, graticule=TRUE, reset=TRUE, las=2)

SW.states <- states_sf %>% st_drop_geometry() %>% subset(REGION==4 & ! STATEFP %in% c('02','15') & as.integer(STATEFP)<60, drop = TRUE)
SW.states

# PR, VI, Guam
subset(states_sf, REGION==9, select=c('STUSPS', 'NAME' ))
