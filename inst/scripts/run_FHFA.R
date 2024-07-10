# setup -------------------------------------------------------------------
# rm(list = ls())
# source("~/Spatial/.RProfile")
library(configr)
configr::read.config()
devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)


?units_options
?valid_udunits
if(interactive())
  View(valid_udunits())
(val_ud.df <- valid_udunits()) %>% clipr::write_clip(object_type = "table")
print(valid_udunits(), n=276L)
units::valid_udunits_prefixes()
my_area <- set_units(1, "mile^2")
set_units(my_area, "km^2")
stop()
browseURL(.NMDB_datadir)
browseURL(.NMDB_workdir)
dir.create(.NMDB_workdir)

file.choose()

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


# geo ---------------------------------------------------------------------

browseURL(.NMDB_workdir)

tracts_sf<-get_tracts_sf()

# SFCensusTractFNM2022 ----------------------------------------------------

SFCensusTractFNM2022_zip <- file.path( .NMDB_datadir,"Single-Family Census Tract File\\2022_SFCensusTractFNM2022.zip")
unzip(SFCensusTractFNM2022_zip, exdir = .NMDB_workdir)

list.files(path = .NMDB_workdir, full.names = TRUE)

fnma_sf2022c_loans_fst <- file.path(.NMDB_workdir, "fnma_sf2022c_loans.fst"); print(file.info(fnma_sf2022c_loans_fst))
if(file.exists(fnma_sf2022c_loans_fst)) {
  print(fst.metadata(fnma_sf2022c_loans_fst))
  fnma_sf2022c_loans <- read_dt(fnma_sf2022c_loans_fst)
} else {


  fnma_sf2022c_loans <- fread(file.path(.NMDB_workdir, "fnma_sf2022c_loans.txt"), col.names = as.character(layout[!is.na(FIELD_), FIELD_NAME])
                              , na.strings = c('9999.0','99.000', '999.00', '9999.000','9',  '99', '999999', '999999999'))
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


  # Rate spread ----


  fnma_sf2022c_loans[, HOEPA_STATUS:=factor(HOEPA_STATUS, levels = 1:2, labels = c("Y","N"))]
  fnma_sf2022c_loans[, PROPERTY_TYPE:=factor(PROPERTY_TYPE, levels = 1:2)]
  fnma_sf2022c_loans[, LIEN_STATUS:=factor(LIEN_STATUS)]
print(fnma_sf2022c_loans[, table(LIEN_STATUS, useNA = "ifany")])

# Duty to Serve -----------------------------------------------------------
  fnma_sf2022c_loans[, PERSISTENT_POVERTY_COUNTY:=factor(PERSISTENT_POVERTY_COUNTY, levels = 1:2, labels = c("Y","N"))]
  fnma_sf2022c_loans[, AREA_OF_CONCENTRATED_POVERTY:=factor(AREA_OF_CONCENTRATED_POVERTY, levels = 1:2, labels = c("Y","N"))]
  fnma_sf2022c_loans[, HIGH_OPPORTUNITY_AREA:=factor(HIGH_OPPORTUNITY_AREA, levels = 1:2, labels = c("Y","N"))]

# save --------------------------------------------------------------------
print(summary(fnma_sf2022c_loans))

  write_fst(fnma_sf2022c_loans,fnma_sf2022c_loans_fst); print(file.info(fnma_sf2022c_loans_fst))
}; str(fnma_sf2022c_loans)
tibble::view(fnma_sf2022c_loans,n=10L)
str(fnma_sf2022c_loans)

(fnma_sf2022c_loans_stats <- fnma_sf2022c_loans[, .N, keyby = c('US_POSTAL_STATE_CODE','COUNTY_2020_CENSUS','CENSUS_TRACT_2020_CENSUS')])

fnma_sf2022c_loans_stats_sf <- merge(tracts_sf, fnma_sf2022c_loans_stats, by.x=c('STATEFP','COUNTYFP','TRACTCE'), by.y=c('US_POSTAL_STATE_CODE','COUNTY_2020_CENSUS','CENSUS_TRACT_2020_CENSUS'))
nrow(fnma_sf2022c_loans_stats);nrow(fnma_sf2022c_loans_stats_sf)
fnma_sf2022c_loans_stats_sf %>% subset(! STATEFP %in% c('02','15') & STATEFP<60 & STATEFP %in% c('06'), select=N)%>% plot(axes=TRUE, graticule=TRUE, reset=FALSE, border=NA)


# GWR ---------------------------------------------------------------------


attach(fnma_sf2022c_loans)

RATE_SPREAD

fnma_sf2022c_loans[LIEN_STATUS==1 & RATE_SPREAD>=1.5, plot(density(RATE_SPREAD), xlim=c(1.5,3.5))]

fnma_sf2022c_loans[LIEN_STATUS==2 ]
fnma_sf2022c_loans[LIEN_STATUS==2 & RATE_SPREAD>=3.5, plot(density(RATE_SPREAD), xlim=c(3.5,5.5))]

fnma_sf2022c_loans[LIEN_STATUS==1 & RATE_SPREAD %between% c(2,2.5)]

detach(fnma_sf2022c_loans)
