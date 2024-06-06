
configr::read.config()
list.files(file.path(.census_datadir, "rel2020", "zcta520"), full.names = TRUE, recursive = TRUE)


# how many counties contain a zip5? -------------------------------------------------------------------------

tab20_zcta520_county20_natl <-  get_tab20_zcta520_county20_natl()
tab20_zcta520_county20_natl[, .(GEOID_COUNTY_20_COUNT=uniqueN(GEOID_COUNTY_20)), by = 'GEOID_ZCTA5_20'][, .N, keyby='GEOID_COUNTY_20_COUNT']



# tab20_zcta520_county20_natl_stats ---------------------------------------


(tab20_zcta520_county20_natl_stats <- tab20_zcta520_county20_natl[, .(
  .N
  , GEOID_COUNTY_20=uniqueN(GEOID_COUNTY_20)
  ,AREALAND_ZCTA5_20=mean(AREALAND_ZCTA5_20)
  ,AREALAND_PART=sum(AREALAND_PART)
  , AREAWATER_PART=sum(AREAWATER_PART))
  ,keyby = c('GEOID_ZCTA5_20')]) %>% as_tibble()


# verify county area ------------------------------------------------------

tab20_zcta520_county20_natl[, .(AREALAND_COUNTY_20=mean(AREALAND_COUNTY_20)
                                , AREAWATER_COUNTY_20=mean(AREAWATER_COUNTY_20))
                            , keyby = c('GEOID_COUNTY_20')]; counties_dt[, .(GEOID,ALAND,    AWATER )]
(counties_dt <- get_counties_sf() %>% st_drop_geometry() %>% setDT(key = 'GEOID'))

tab20_zcta520_county20_natl_stats

# verification of crosswalk -----------------------------------------------

duplicated(tab20_zcta520_county20_natl, by='GEOID_ZCTA5_20')
duplicated(tab20_zcta520_county20_natl, by='GEOID_COUNTY_20')

NRI_counties_dt <- get_NRI_counties_dt(); NRI_counties_dt[, AREA:=set_units(AREA, "m^2")]
str(NRI_counties_dt)


## area --------------------------------------------------------------------
NRI_counties_dt[, .(STCOFIPS_COUNT=uniqueN(STCOFIPS), AREA=sum(AREA))] %>%as_tibble()
9160957327057+ 712091074342 # tiger counties aland + awater

sum(NRI_counties_dt[, .SD, .SDcols=EALB.cols], na.rm = TRUE) ; NRI_counties_dt[, .(sum(EAL_VALB, na.rm = TRUE))] # total expected annual loss on building value

NRI_counties_dt[, .(STCOFIPS,BUILDVALUE, EAL_VALB, EAL_VALB/BUILDVALUE, ALR_VALB)]

get_NRI_zcta520_dt <- function(){

  NRI_zcta520_dt_fst <- file.path(.census_workdir, "NRI_zcta520_dt.fst"); print(file.info(NRI_zcta520_dt_fst))
  if(file.exists(NRI_zcta520_dt_fst)) {
    print(fst.metadata(NRI_zcta520_dt_fst))
    NRI_zcta520_dt <- read_fst(NRI_zcta520_dt_fst, as.data.table = TRUE)
  } else {


    NRI_zcta520_cross_dt <- merge(tab20_zcta520_county20_natl,NRI_counties_dt, by.x='GEOID_COUNTY_20', by.y='STCOFIPS')

    str(NRI_zcta520_cross_dt)
    NRI_zcta520_cross_dt[, AREALAND_ZCTA5_20_CONTRIB_AREALAND_COUNTY_20_RATIO:=AREALAND_PART/AREALAND_COUNTY_20]


    (NRI_zcta520_dt <- NRI_zcta520_x_dt[, .(
      NUMBER_OF_COUNTIES_PARTICIPATIONS=uniqueN(GEOID_COUNTY_20)
      ,POPULATION =round(sum(POPULATION*AREALAND_ZCTA5_20_CONTRIB_AREALAND_COUNTY_20_RATIO),0L)
      ,BUILDVALUE =round(sum(BUILDVALUE* AREALAND_ZCTA5_20_CONTRIB_AREALAND_COUNTY_20_RATIO),0L)
      ,AGRIVALUE=round(sum(AGRIVALUE* AREALAND_ZCTA5_20_CONTRIB_AREALAND_COUNTY_20_RATIO),0L)
      #  ,RISK_VALUE=round(sum(RISK_VALUE* AREALAND_ZCTA5_20_CONTRIB_AREALAND_COUNTY_20_RATIO, na.rm = TRUE),0L)

      ,EAL_VALB=round(sum(EAL_VALB* AREALAND_ZCTA5_20_CONTRIB_AREALAND_COUNTY_20_RATIO),0L)
      ,EAL_VALPE=round(sum(EAL_VALPE* AREALAND_ZCTA5_20_CONTRIB_AREALAND_COUNTY_20_RATIO),0L)
      ,EAL_VALA=round(sum(EAL_VALA* AREALAND_ZCTA5_20_CONTRIB_AREALAND_COUNTY_20_RATIO),0L)
    ), keyby = .(GEOID_ZCTA5_20)][, c("ALR_VALB",'EAL_VALT'):=list(EAL_VALB/BUILDVALUE,EAL_VALB+EAL_VALPE+EAL_VALA)]
    ) %>% as_tibble()

    EALB.cols <- grep("EALB$", names(NRI_counties_dt), value = TRUE)
    (NRI_counties_EALB.stats <- NRI_counties_dt[, lapply(.SD, function(x) round(sum(x, na.rm = TRUE),0L))
                                        ,.SDcols =  EALB.cols
                                        # , keyby = .(GEOID_ZCTA5_20)
    ])%>% as_tibble()



    (tmp_EALB.stats <- NRI_zcta520_x_dt[, lapply(.SD, function(x) round(sum(x* AREALAND_ZCTA5_20_CONTRIB_AREALAND_COUNTY_20_RATIO, na.rm = TRUE),0L))
                                        ,.SDcols =  EALB.cols
                                        , keyby = .(GEOID_ZCTA5_20)
    ])%>% as_tibble()

    (NRI_zcta520_EALB.stats <- merge(NRI_zcta520_dt, tmp_EALB.stats, by='GEOID_ZCTA5_20'))

    # Exposed Building Value (range 0 to BUILDVALUE )--------------------------------------------------
    (EXPB.cols <- grep("(?<!DRGT_)EXPB$", names(NRI_counties_dt), value = TRUE, perl = TRUE))

    NRI_counties_dt[, lapply(.SD, min, na.rm=TRUE), .SDcols = EXPB.cols]

    (tmp_EXPB.stats <- NRI_zcta520_x_dt[, lapply(.SD, function(x) round(sum(x* AREALAND_ZCTA5_20_CONTRIB_AREALAND_COUNTY_20_RATIO, na.rm = TRUE),0L)), .SDcols=EXPB.cols
                                        , keyby = .(GEOID_ZCTA5_20)])%>% as_tibble()



    (NRI_zcta520_EXPB_EALB.stats <- merge(NRI_zcta520_EALB.stats, tmp_EXPB.stats, by='GEOID_ZCTA5_20'))

    # Annual Loss Rate on Building Value by Peril -----------------------------


    (ALRB.cols <- grep("ALRB$", names(NRI_counties_dt), value = TRUE))

    print(df <- data.frame(ALRB.cols, EALB.cols, EXPB.cols))

    for (i in seq.int(1, nrow(df))){
      NRI_zcta520_EXPB_EALB.stats[, df[i,1]:=get(df[i,2])/get(df[i,3])]; print(NRI_zcta520_EXPB_EALB.stats[, summary(get(df[i,1]))])
    }
    summary(NRI_zcta520_EXPB_EALB.stats)



    NRI_zcta520_dt <- NRI_zcta520_EXPB_EALB.stats
    write_fst(NRI_zcta520_dt, path = NRI_zcta520_dt_fst);   print(file.info(NRI_zcta520_dt_fst))
  }; str(NRI_zcta520_dt)
  NRI_zcta520_dt
}

str(NRI_zcta520_x_dt$GEOID_COUNTY_20)
str(NRI_zcta520_x_dt$GEOID_ZCTA5_20 )

options(scipen = 9999L)

names(NRI_counties_dt)
# Expected Annual Loss Rating ----
(EALR.cols <- grep("(?<!DRGT_)EALR$", names(NRI_counties_dt), value = TRUE, perl = TRUE))
print(cbind(EALB.cols, EALR.cols))
paste0(EALR.cols,"_2")




NRI_zcta520_EXPB.stats[, lapply(.SD, function(x) x/BUILDVALUE), .SDcols = EXPB.cols , by='GEOID_ZCTA5_20']



# Historical Loss Rate on single event on Building Value ----
(HLRB.cols <- grep("(?<!DRGT_)HLRB$", names(NRI_counties_dt), value = TRUE, perl = TRUE))
print(cbind(EALB.cols, HLRB.cols))


NRI_counties_dt[, .SD, .SDcols =  HLRB.cols]
NRI_counties_dt[, lapply(.SD, function(x) x/BUILDVALUE), .SDcols=EXPB.cols] # Fraction of Building Value exposed

# Annualized frequency by peril ----
HRCN_AFREQ

NRI_counties_dt[, .(STCOFIPS,HRCN_AFREQ)]

NRI_zcta520_x_dt[, .(GEOID_ZCTA5_20, AREALAND_PART, AREAWATER_PART
                     ,GEOID_COUNTY_20,AREALAND_COUNTY_20,AREAWATER_COUNTY_20,AREATOTAL_COUNTY_20=AREALAND_COUNTY_20+AREAWATER_COUNTY_20,AREA
                     ,HRCN_AFREQ)]

NRI_zcta520_x_dt[, .(HRCN_AFREQ=sum(HRCN_AFREQ*(AREALAND_PART + AREAWATER_PART)/(AREALAND_COUNTY_20+AREAWATER_COUNTY_20))), keyby = c('GEOID_ZCTA5_20')]
# NRI_zcta520 verifications -----------------------------------------------------------


## area --------------------------------------------------------------------
NRI_zcta520_x_dt[, .(sum(AREALAND_COUNTY_20+AREAWATER_COUNTY_20), sum(AREA))]

## EALB --------------------------------------------------------------------



(NRI_zcta520_EALB.stats <- NRI_zcta520_x_dt[, lapply(.SD, function(x) round(sum(x* AREALAND_ZCTA5_20_CONTRIB_AREALAND_COUNTY_20_RATIO, na.rm = TRUE),0L))
                                            ,.SDcols =  EALB.cols
                                            # , keyby = .(GEOID_ZCTA5_20)
])%>% as_tibble()
str(NRI_zcta520_EALB.stats)

sum(NRI_zcta520_EALB.stats[, -1], na.rm = TRUE)

NRI_counties_dt[, .(CWAV_EALB=sum(CWAV_EALB, na.rm = TRUE))]; NRI_zcta520_x_dt[, .(sum(CWAV_EALB* AREALAND_PART/AREALAND_COUNTY_20, na.rm = TRUE))]





NRI_counties_dt[, .(sum(EAL_VALT))]; NRI_zcta520_dt[, .(sum(EAL_VALT ))]
NRI_counties_dt[, .(sum(POPULATION))]; NRI_zcta520_dt[, .(sum(POPULATION_ZCTA5_20 ))]

NRI_zcta520_x_dt[, .(sum(AREALAND_PART/AREALAND_ZCTA5_20)), keyby = .(GEOID_ZCTA5_20)] # should be 1

NRI_zcta520_x_dt[, .(sum(AREALAND_ZCTA5_20_CONTRIB_AREALAND_COUNTY_20_RATIO)), keyby = .(GEOID_COUNTY_20)] # should be 1

