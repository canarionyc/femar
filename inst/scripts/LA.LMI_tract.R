library(terra)

HUD_DATADIR <- configr::eval.config('HUD_DATADIR')
HUD_WORKDIR <- configr::eval.config('HUD_WORKDIR')# ; dir.create(HUD_WORKDIR)

# ?terra::vect
LMI_tract_vect <- terra::vect("E:\\Datasets\\HUD\\Low_to_Moderate_Income_Population_by_Tract.gdb",proxy=TRUE)



?terra::`[`
?terra::aggregate
?terra::query
(LA.LMI_county_vect <- terra::aggregate(LMI_tract_vect %>% terra::query(vars=c(
  'STATE', 'COUNTY'
  ,'LOWMOD'
  ,'LOWMODUNIV'
  ,'UCLMOD'
  ,'UCLMODU')
  ,where="STATE='22'")
  ,by=c( 'STATE', 'COUNTY')
  , fun=base::sum
))

# names(LMI_tract_vect)

# LMI_tract_vect

LA.LMI_tract_vect <- LMI_tract_vect %>% terra::query(where="STATE='22'")

terra::plot(LA.LMI_tract_vect,'LOWMODPCT'
            , col=rev(heat.colors(5)), main="Lousiana, Low to Moderate Income Population Share by Tract")
