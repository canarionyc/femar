# Puerto Rico -------------------------------------------------------------
NRI_counties_dt


str(counties_sf)
(PR_counties_sf <- NRI_counties_sf %>% subset(STATEFIPS=='72'))
summary(PR_counties_sf)

options(max.print = 9999L)

# PR_counties <- st_drop_geometry(PR_counties_sf) %>% setDT(key = c('STCOFIPS')) %>% remove_na_cols()
PR_counties <- NRI_counties_dt%>% remove_na_cols()
fsummary(PR_counties)

(PR_st_sf <- NRI_states_sf %>% subset(STATEFIPS=='72'))
PR_st <- st_drop_geometry(PR_st_sf) %>% setDT()
PR_st[, c("STATEFIPS", "POPULATION", "BUILDVALUE", "AGRIVALUE", "AREA",
          "EAL_VALT", "EAL_VALB", "EAL_VALP", "EAL_VALPE", "EAL_VALA",
          "ALR_VALB", "ALR_VALP", "ALR_VALA")]

PR_st[, .SD, .SDcols=ealt_cols]

dt <- PR_counties
dim(PR_counties)
summary.NRI <- function(dt) {
  s0 <- dt[, .(
    COUNTY_COUNT=uniqueN(STCOFIPS)
    ,POPULATION =sum(POPULATION)      ,BUILDVALUE=sum(BUILDVALUE)            ,AGRIVALUE=sum(AGRIVALUE)
    ,AREA=sum(AREA)
    ,EAL_VALT=sum(EAL_VALT)
    ,EAL_VALB=sum(EAL_VALB)
    ,EAL_VALP=sum(EAL_VALP)
    ,EAL_VALPE=sum(EAL_VALPE)
    , EAL_VALA=sum(EAL_VALA)

    ,ALR_VALB=sum(EAL_VALB)/sum(BUILDVALUE)
    , ALR_VALP=sum(EAL_VALP)/sum(POPULATION)
    , ALR_VALA=sum(EAL_VALA)/sum(AGRIVALUE)
  )]
  print(s0)


  ealt_cols <- grep("_EALT$", names(dt), value = TRUE)
  ealt <- dt[, .SD, .SDcols = ealt_cols]; print(ealt)

  s1 <- dt[, lapply(.SD, sum, na.rm=TRUE), .SDcols=ealt_cols]
  print(s1)
  s <- cbind(s0,s1)

  ealb_cols <- grep("_EALB$", names(dt), value = TRUE); print(ealb_cols)
  ealb <- dt[, .SD, .SDcols = ealb_cols]; print(ealb)
  s2 <- dt[, lapply(.SD, sum, na.rm=TRUE), .SDcols=ealb_cols]
  print(s2)
  s <- cbind(s,s2); print(s)
}

(alrb_cols <- grep("ALRB$", names(dt), value = TRUE))
(alrb_dt <- dt[, .SD, .SDcols = alrb_cols]%>% setnafill(type = "const", fill = 0))
rownames(alrb_dt) <- dt$STCOFIPS
print(alrb_dt)

(alrb.pc <- princomp(alrb_dt))
biplot(alrb.pc, cex=0.65, xpd=TRUE)

library(MASS)
?prcomp
(alrb.prc <- prcomp(alrb_dt))
methods(class="prcomp")
plot(alrb.prc)
?biplot.prcomp
biplot(alrb.prc, choices = 1:2)

dt[, .(STCOFIPS, CFLD_EALT)]
dt[,.(sum(CFLD_EALT ))]
qtm(PR_st_sf) + tm_fill(fill='EAL_RATNG')

?tm_fill
PR_counties_sf$ALR_VALB
qtm(PR_counties_sf) + tm_fill(fill='ALR_VALB', col="black")

(ealt_cols <- grep("_EALT$", names(PR_counties), value = TRUE))

(ealt_cty <- PR_counties[, .SD, .SDcols = c(ealt_cols)] %>% setnafill(type="const", fill=0))
rownames(ealt_cty) <- PR_counties$STCOFIPS

library(MASS)
?princomp
pr_ealt.pc <- princomp(ealt_cty)
biplot(pr_ealt.pc)


# check bad data ----------------------------------------------------------------

range(NRI_counties_sf$RFLD_EALT)

PR_counties_sf$RFLD_EALT%>% as_tibble()
PR_counties_sf$LNDS_EALT


