# setup -------------------------------------------------------------------
# rm(list = ls())
# source("~/Spatial/.RProfile")
library(configr)
configr::read.config()
devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)





stop()
browseURL(.NMDB_datadir)
browseURL(.NMDB_workdir)
dir.create(.NMDB_workdir)

file.choose()


?units_options
?valid_udunits
if(interactive())
  tibble::view(valid_udunits())
(val_ud.df <- valid_udunits()) %>% clipr::write_clip(object_type = "table")
print(valid_udunits(), n=276L)
units::valid_udunits_prefixes()
my_area <- set_units(1, "mile^2")
set_units(my_area, "km^2")
# layout ------------------------------------------------------------------


browseURL(system.file("extdata","FHFA","NMDB", "SFCensusTractFNM2022.xlsx", package = "femar"))
?read_xlsx
layout <- readxl::read_xlsx(system.file("extdata","FHFA","NMDB", "SFCensusTractFNM2022.xlsx", package = "femar")) %>% as.data.table() %>% sanitize()
layout[, 1:2]
?base::make.names
layout[!is.na(1), c(1,3)]
layout[, FIELD_NAME:=sanitize(FIELD_NAME)%>%make.names(unique = TRUE)]
str(layout)
as.character(layout[!is.na(FIELD_), FIELD_NAME])


# tracts_sf ---------------------------------------------------------------------

browseURL(.NMDB_workdir)

tracts_sf<-get_tracts_sf()

# fnma_sf2022c_loans ----------------------------------------------------


fnma_sf2022c_loans_fst <- file.path(.NMDB_workdir, "fnma_sf2022c_loans.fst"); print(file.info(fnma_sf2022c_loans_fst))
if(file.exists(fnma_sf2022c_loans_fst)) {
  print(fst.metadata(fnma_sf2022c_loans_fst))
  fnma_sf2022c_loans <- read_dt(fnma_sf2022c_loans_fst)
} else {
  SFCensusTractFNM2022_zip <- file.path( .NMDB_datadir,"Single-Family Census Tract File\\2022_SFCensusTractFNM2022.zip")
  unzip(SFCensusTractFNM2022_zip, exdir = .NMDB_workdir)

  list.files(path = .NMDB_workdir, full.names = TRUE)

  fnma_sf2022c_loans <- fread(file.path(.NMDB_workdir, "fnma_sf2022c_loans.txt"), col.names = as.character(layout[!is.na(FIELD_), FIELD_NAME])
                              , na.strings = c('9999.0','99.000', '999.00', '9999.000','9',  '99', '999', '999999', '999999999'))
  fnma_sf2022c_loans[, US_POSTAL_STATE_CODE:=sprintf("%02d", US_POSTAL_STATE_CODE)]
  fnma_sf2022c_loans[, COUNTY_2020_CENSUS:=sprintf("%03d", COUNTY_2020_CENSUS)]
  fnma_sf2022c_loans[, CENSUS_TRACT_2020_CENSUS:=sprintf("%06d", CENSUS_TRACT_2020_CENSUS)]
  str(fnma_sf2022c_loans$CENSUS_TRACT_2020_CENSUS)
  fnma_sf2022c_loans[, RURAL_CENSUS_TRACT:=factor(RURAL_CENSUS_TRACT,  levels = 1:2, labels = c("Y", "N"))]

  # loan descriptive --------------------------------------------------------


  fnma_sf2022c_loans[, table(PURPOSE_OF_LOAN, useNA = "ifany")]
  fnma_sf2022c_loans[, PURPOSE_OF_LOAN:=factor(PURPOSE_OF_LOAN, levels = c(1,2,4,7))]
  fnma_sf2022c_loans[, FEDERAL_GUARANTEE:=factor(FEDERAL_GUARANTEE, levels = 1:4)]
  fnma_sf2022c_loans[, table(FEDERAL_GUARANTEE, useNA = "ifany")]
  fnma_sf2022c_loans[, table(FIRST_TIME_HOME_BUYER, useNA = "ifany")]
  fnma_sf2022c_loans[, FIRST_TIME_HOME_BUYER:=factor(FIRST_TIME_HOME_BUYER, levels = 1:2, labels = c("Y", "N"))]

  fnma_sf2022c_loans[, DATE_OF_MORTGAGE_NOTE:=factor(DATE_OF_MORTGAGE_NOTE, levels = 1:2)]
  str(fnma_sf2022c_loans$LOAN_TO_VALUE_RATIO_AT_ORIGINATION_OR_COMBINED_LTV_WHERE_AVAILABLE)
  print(fnma_sf2022c_loans[,table(LOAN_TO_VALUE_RATIO_AT_ORIGINATION_OR_COMBINED_LTV_WHERE_AVAILABLE, useNA = "ifany")])
  # borrower descriptive ----------------------------------------------------


  fnma_sf2022c_loans[, table(AGE_OF_BORROWER, useNA = "ifany")]
  fnma_sf2022c_loans[, BORROWER_AGE_62_OR_OLDER:=factor(BORROWER_AGE_62_OR_OLDER, levels = 1:2, labels = c("Y", "N"))]


  # Borrower race -----------------------------------------------------------

  # 1 = American Indian or Alaska Native
  # 2 = Asian
  # 3 = Black or African American
  # 4 = Native Hawaiian or Other Pacific Islander
  # 5 = White
  # 6 = information not provided by borrower in a mail, internet, or telephone application
  # 7 = not applicable
  # 9 = not available

  # Rate spread ----

  fnma_sf2022c_loans[, HOEPA_STATUS:=factor(HOEPA_STATUS, levels = 1:2, labels = c("Y","N"))]
  fnma_sf2022c_loans[, PROPERTY_TYPE:=factor(PROPERTY_TYPE, levels = 1:2)]

  fnma_sf2022c_loans[, LIEN_STATUS:=factor(LIEN_STATUS)]
  print(fnma_sf2022c_loans[, table(LIEN_STATUS, useNA = "ifany")])

  fnma_sf2022c_loans[, DISCOUNT_POINTS:=DISCOUNT_POINTS/1e3L]
  print(fnma_sf2022c_loans[, range(DISCOUNT_POINTS, na.rm = TRUE)])
  # Duty to Serve -----------------------------------------------------------
  fnma_sf2022c_loans[, PERSISTENT_POVERTY_COUNTY:=factor(PERSISTENT_POVERTY_COUNTY, levels = 1:2, labels = c("Y","N",NA))]
  # fnma_sf2022c_loans[, PERSISTENT_POVERTY_COUNTY:=fct_na_value_to_level(PERSISTENT_POVERTY_COUNTY)]
  table(fnma_sf2022c_loans$PERSISTENT_POVERTY_COUNTY, useNA = "ifany")

  fnma_sf2022c_loans[, AREA_OF_CONCENTRATED_POVERTY:=factor(AREA_OF_CONCENTRATED_POVERTY, levels = 1:2, labels = c("Y","N",NA))]
  # fnma_sf2022c_loans[, AREA_OF_CONCENTRATED_POVERTY:=fct_na_value_to_level(AREA_OF_CONCENTRATED_POVERTY)]
  fnma_sf2022c_loans[, HIGH_OPPORTUNITY_AREA:=factor(HIGH_OPPORTUNITY_AREA, levels = 1:2, labels = c("Y","N",NA))]
  # fnma_sf2022c_loans[, HIGH_OPPORTUNITY_AREA:=fct_na_value_to_level(HIGH_OPPORTUNITY_AREA)]
  # save --------------------------------------------------------------------
  print(summary(fnma_sf2022c_loans))

  write_fst(fnma_sf2022c_loans,fnma_sf2022c_loans_fst); print(file.info(fnma_sf2022c_loans_fst))
}; str(fnma_sf2022c_loans)
tibble::view(fnma_sf2022c_loans,n=10L)
str(fnma_sf2022c_loans)

attach(fnma_sf2022c_loans)

detach(fnma_sf2022c_loans)
str(fnma_sf2022c_loans$PERSISTENT_POVERTY_COUNTY)

poverty <- fnma_sf2022c_loans[AREA_OF_CONCENTRATED_POVERTY=='Y', .(US_POSTAL_STATE_CODE,COUNTY_2020_CENSUS, CENSUS_TRACT_2020_CENSUS
                                  , X2020_CENSUS_TRACT_MEDIAN_INCOME, TRACT_INCOME_RATIO
                                  , LOCAL_AREA_MEDIAN_INCOME, AREA_MEDIAN_FAMILY_INCOME
                                  ,AREA_OF_CONCENTRATED_POVERTY)]


fnma_sf2022c_loans[, .(US_POSTAL_STATE_CODE,COUNTY_2020_CENSUS, CENSUS_TRACT_2020_CENSUS
                       , X2020_CENSUS_TRACT_MEDIAN_INCOME, TRACT_INCOME_RATIO
                       , LOCAL_AREA_MEDIAN_INCOME, AREA_MEDIAN_FAMILY_INCOME, AREA_OF_CONCENTRATED_POVERTY
                       , BORROWERS_ANNUAL_INCOME, BORROWER_INCOME_RATIO, BORROWER_INCOME_RATIO.check=round(BORROWERS_ANNUAL_INCOME/AREA_MEDIAN_FAMILY_INCOME,4L))]

?xyplot.formula

?equal.count
fnma_sf2022c_loans[, BORROWER_INCOME_RATIO.f :=cut(BORROWER_INCOME_RATIO, breaks = c(0,.50,.80,1.0,1.20, Inf))]
fnma_sf2022c_loans[, table(BORROWER_INCOME_RATIO.f, useNA = "ifany")]

?sample.int



# linear model ------------------------------------------------------------


fnma_sf2022c_loans[, lBORROWERS_ANNUAL_INCOME:=log10(BORROWERS_ANNUAL_INCOME)]
fnma_sf2022c_loans[, lAREA_MEDIAN_FAMILY_INCOME:=log10(AREA_MEDIAN_FAMILY_INCOME)]

idx <- sample.int(nrow(fnma_sf2022c_loans), size = 1e4L, replace = TRUE)
dta <- fnma_sf2022c_loans[idx]



lm1 <- lm(lBORROWERS_ANNUAL_INCOME~lAREA_MEDIAN_FAMILY_INCOME, data = dta)
summary(lm1)
lm1.rr <-
fivenum(lm1.rr)



dta[, c('fitted','resp.res') := list(fitted(lm1), residuals.lm(lm1, type = "response")) ]

dta_sf <- merge(tracts_sf, dta, by.x=c('STATEFP','COUNTYFP','TRACTCE'), by.y=c('US_POSTAL_STATE_CODE','COUNTY_2020_CENSUS','CENSUS_TRACT_2020_CENSUS'))

dta_sf%>% subset(! STATEFP %in% c('02','15') & STATEFP<60, select=resp.res ) %>% plot(axes=TRUE, graticule=TRUE, reset=FALSE)


plot(lBORROWERS_ANNUAL_INCOME~lAREA_MEDIAN_FAMILY_INCOME, data = dta)

plot(lm1 )

library(MASS)

# xyplot(BORROWERS_ANNUAL_INCOME ~AREA_MEDIAN_FAMILY_INCOME|PERSISTENT_POVERTY_COUNTY,  data=fnma_sf2022c_loans  )

bwplot(BORROWER_INCOME_RATIO     , data=fnma_sf2022c_loans   )

densityplot(~BORROWER_INCOME_RATIO, plot.ploints=FALSE)

# DISCOUNT_POINTS ---------------------------------------------------------

fnma_sf2022c_loans[, range(DISCOUNT_POINTS, na.rm = TRUE)]
fnma_sf2022c_loans[DISCOUNT_POINTS==max(DISCOUNT_POINTS, na.rm = TRUE), DISCOUNT_POINTS:=NA]
?densityplot
densityplot(~DISCOUNT_POINTS, groups = BORROWER_RACE_OR_NATIONAL_ORIGIN
            , data=fnma_sf2022c_loans, subset = BORROWER_RACE_OR_NATIONAL_ORIGIN %in% 1:5 & DISCOUNT_POINTS>0
            , na.rm=TRUE
            , drop.unused.levels = TRUE
            , plot.points=FALSE
            , from=0
            , to=500, cut=0
            , xlim = c(0,50)
            , lwd=c(1,1,1,1,2)
            , auto.key=TRUE)

(fnma_sf2022c_loans_stats <-
    fnma_sf2022c_loans[LIEN_STATUS==1 & BORROWER_RACE_OR_NATIONAL_ORIGIN==3L
#                       & RATE_SPREAD>=1.5
                       , .(RECORD_COUNT=.N
                          ,NUMBER_OF_BORROWERS =sum(NUMBER_OF_BORROWERS )
                          ,NUMBER_OF_WHITE_BORROWERS=sum(fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN==5,1L,0L,0L)+
                            fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.1==5,1L,0L,0L)+
                            fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.2==5,1L,0L,0L)+
                            fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.3==5,1L,0L,0L)+
                            fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.4==5,1L,0L,0L))
                          ,NUMBER_OF_BLACK_OR_AFRICAN_AMERICAN_BORROWERS=sum(fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN==3L,1L,0L,0L)+
                                                           fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.1==3L,1L,0L,0L)+
                                                           fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.2==3L,1L,0L,0L)+
                                                           fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.3==3L,1L,0L,0L)+
                                                           fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.4==3L,1L,0L,0L))
                          , NUMBER_OF_BORROWERS_NOT_HISPANIC_OR_LATINO=sum(fifelse(BORROWER_ETHNICITY==2, 1L,0L,0L))
                          # , NUMBER_OF_BORROWERS_HISPANIC_OR_LATINO_OR_UNKNOWN=sum(fifelse(BORROWER_ETHNICITY!=2, 1L,0L,1L))
                          , WA_RATE_SPREAD=weighted.mean(RATE_SPREAD, NOTE_AMOUNT)
                          , WA_DISCOUNT_POINTS=weighted.mean(DISCOUNT_POINTS, NOTE_AMOUNT)
                          , WA_INTEREST_RATE_AT_ORIGINATION=weighted.mean(INTEREST_RATE_AT_ORIGINATION,NOTE_AMOUNT)

    )
    , keyby = c('US_POSTAL_STATE_CODE'
                ,'COUNTY_2020_CENSUS'
                # ,'CENSUS_TRACT_2020_CENSUS'
                )][, c('NUMBER_OF_NON_WHITE_BORROWERS_PCT'
                       ,'NUMBER_OF_BORROWERS_HISPANIC_OR_LATINO_OR_UNKNOWN_PCT'
                       ,'NUMBER_OF_BLACK_OR_AFRICAN_AMERICAN_BORROWERS_PCT'):=list(
                         100*(1-NUMBER_OF_WHITE_BORROWERS/NUMBER_OF_BORROWERS)
                         ,100*(1-NUMBER_OF_BORROWERS_NOT_HISPANIC_OR_LATINO/NUMBER_OF_BORROWERS)
                         ,100*NUMBER_OF_BLACK_OR_AFRICAN_AMERICAN_BORROWERS/NUMBER_OF_BORROWERS)]) %>% tibble::view()

tibble::view(fnma_sf2022c_loans_stats)

xyplot(WA_RATE_SPREAD ~NUMBER_OF_NON_WHITE_BORROWERS_PCT, fnma_sf2022c_loans_stats
#       , subset=RECORD_COUNT>=1e3L
       )
xyplot(WA_RATE_SPREAD ~NUMBER_OF_BORROWERS_HISPANIC_OR_LATINO_OR_UNKNOWN_PCT, fnma_sf2022c_loans_stats
#       , subset=RECORD_COUNT>=1e0L
       )

xyplot(WA_RATE_SPREAD ~NUMBER_OF_BLACK_OR_AFRICAN_AMERICAN_BORROWERS_PCT, fnma_sf2022c_loans_stats
       #       , subset=RECORD_COUNT>=1e0L
)


xyplot(WA_INTEREST_RATE_AT_ORIGINATION ~NUMBER_OF_NON_WHITE_BORROWERS_PCT, fnma_sf2022c_loans_stats
#       , subset=RECORD_COUNT>=1e3L
       )
densityplot(~WA_INTEREST_RATE_AT_ORIGINATION, fnma_sf2022c_loans_stats)

xyplot(WA_INTEREST_RATE_AT_ORIGINATION ~NUMBER_OF_BORROWERS_HISPANIC_OR_LATINO_OR_UNKNOWN_PCT, fnma_sf2022c_loans_stats
#       , subset=RECORD_COUNT>=1e3L
       )

xyplot(WA_INTEREST_RATE_AT_ORIGINATION ~NUMBER_OF_BLACK_OR_AFRICAN_AMERICAN_BORROWERS_PCT, fnma_sf2022c_loans_stats
       #       , subset=RECORD_COUNT>=1e3L
)


# fnma_sf2022c_loans_stats2 -----------------------------------------------


(fnma_sf2022c_loans_stats2 <-
    fnma_sf2022c_loans[LIEN_STATUS==1
                       #                       & RATE_SPREAD>=1.5
                       , .(RECORD_COUNT=.N
                           ,NUMBER_OF_BORROWERS =sum(NUMBER_OF_BORROWERS )
                           ,NUMBER_OF_WHITE_BORROWERS=sum(fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN==5,1L,0L,0L)+
                                                            fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.1==5,1L,0L,0L)+
                                                            fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.2==5,1L,0L,0L)+
                                                            fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.3==5,1L,0L,0L)+
                                                            fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.4==5,1L,0L,0L))
                           ,NUMBER_OF_BLACK_OR_AFRICAN_AMERICAN_BORROWERS=sum(fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN==3L,1L,0L,0L)+
                                                                                fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.1==3L,1L,0L,0L)+
                                                                                fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.2==3L,1L,0L,0L)+
                                                                                fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.3==3L,1L,0L,0L)+
                                                                                fifelse(BORROWER_RACE_OR_NATIONAL_ORIGIN.4==3L,1L,0L,0L))
                           , NUMBER_OF_BORROWERS_NOT_HISPANIC_OR_LATINO=sum(fifelse(BORROWER_ETHNICITY==2, 1L,0L,0L))
                           # , NUMBER_OF_BORROWERS_HISPANIC_OR_LATINO_OR_UNKNOWN=sum(fifelse(BORROWER_ETHNICITY!=2, 1L,0L,1L))
                           , WA_RATE_SPREAD=weighted.mean(RATE_SPREAD, NOTE_AMOUNT)
                           , DISCOUNT_POINTS=weighted.mean(DISCOUNT_POINTS, NOTE_AMOUNT)
                           , WA_INTEREST_RATE_AT_ORIGINATION=weighted.mean(INTEREST_RATE_AT_ORIGINATION,NOTE_AMOUNT)

                       )
                       , keyby = c('US_POSTAL_STATE_CODE'
                                   ,'COUNTY_2020_CENSUS'
                                   # ,'CENSUS_TRACT_2020_CENSUS'
                                   ,'BORROWER_RACE_OR_NATIONAL_ORIGIN'
                       )])

densityplot(~WA_INTEREST_RATE_AT_ORIGINATION, groups = BORROWER_RACE_OR_NATIONAL_ORIGIN
            , data=fnma_sf2022c_loans_stats2, subset = BORROWER_RACE_OR_NATIONAL_ORIGIN %in% 1:5
            , plot.points=FALSE, drop.unused.levels = TRUE
            , auto.key=TRUE
            , lwd=c(1,1,1,1,2))

densityplot(~WA_RATE_SPREAD, groups = BORROWER_RACE_OR_NATIONAL_ORIGIN, data=fnma_sf2022c_loans_stats2
            , subset = BORROWER_RACE_OR_NATIONAL_ORIGIN %in% 1:5
            , from=0
#            , to=1
            , cut = 0
            , plot.points=FALSE
            , auto.key=TRUE)

densityplot(~DISCOUNT_POINTS, groups = BORROWER_RACE_OR_NATIONAL_ORIGIN, data=fnma_sf2022c_loans_stats2
            , subset = BORROWER_RACE_OR_NATIONAL_ORIGIN %in% 1:5
            , from=0
            #            , to=1
            , cut = 0
            , plot.points=FALSE
            , auto.key=TRUE)

# fnma_sf2022c_loans_stats_sf ---------------------------------------------

fnma_sf2022c_loans_stats_sf <- merge(tracts_sf, fnma_sf2022c_loans_stats, by.x=c('STATEFP','COUNTYFP','TRACTCE'), by.y=c('US_POSTAL_STATE_CODE','COUNTY_2020_CENSUS','CENSUS_TRACT_2020_CENSUS'))
nrow(fnma_sf2022c_loans_stats);nrow(fnma_sf2022c_loans_stats_sf)
fnma_sf2022c_loans_stats_sf %>% subset(! STATEFP %in% c('02','15') & STATEFP<60 & STATEFP %in% c('06'), select=N)%>% plot(axes=TRUE, graticule=TRUE, reset=FALSE, border=NA)


brace <- grep("^BORROWER_RACE", names(fnma_sf2022c_loans), value = TRUE)


# Borrower Ethnicity-------------------------------------------------------------------------

# 1 = Hispanic or Latino
# 2 = Not Hispanic or Latino
# 3 = information not provided by borrower in a mail, internet, or telephone application
# 4 = not applicable
# 9 = not available

fnma_sf2022c_loans[, summary(.SD), .SDcols = c('NUMBER_OF_BORROWERS', brace)]

# GWR ---------------------------------------------------------------------

attach(fnma_sf2022c_loans)

RATE_SPREAD
?densityplot
densityplot(~RATE_SPREAD, data=fnma_sf2022c_loans, subset=LIEN_STATUS==1 & RATE_SPREAD>=1.5, from=1.5,to=3.5, cut = 0, plot.points=FALSE)

fnma_sf2022c_loans[LIEN_STATUS==1 & RATE_SPREAD>=1.5, plot(density(RATE_SPREAD), xlim=c(1.5,3.5))]

fnma_sf2022c_loans[LIEN_STATUS==2 ]
fnma_sf2022c_loans[LIEN_STATUS==2 & RATE_SPREAD>=3.5, plot(density(RATE_SPREAD), xlim=c(3.5,5.5))]

fnma_sf2022c_loans[LIEN_STATUS==1 & RATE_SPREAD %between% c(2,2.5)]

detach(fnma_sf2022c_loans)
