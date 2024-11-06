
# setup -------------------------------------------------------------------

tigris::fips_codes %>% subset(state=="LA")
tigris::filter_state()

# Low_to_Moderate_Income_Population_by_Tract ------------------------------
# https://hudgis-hud.opendata.arcgis.com/datasets/HUD::low-to-moderate-income-population-by-tract/about

# This service identifies U.S. Census Tracts in which 51% or more of the
# households earn less than 80 percent of the Area Median Income (AMI). The
# Community Development Block Grant (CDBG) program requires that each CDBG
# funded activity must either principally benefit low- and moderate-income
# persons, aid in the prevention or elimination of slums or blight, or meet a
# community development need having a particular urgency because existing
# conditions pose a serious and immediate threat to the health or welfare of the
# community and other financial resources are not available to meet that need.
# With respect to activities that principally benefit low- and moderate-income
# persons, at least 51 percent of the activity's beneficiaries must be low and
# moderate income.

# The Community Development Block Grant (CDBG) program requires that each CDBG
# funded activity must either principally benefit low- and moderate-income
# persons, aid in the prevention or elimination of slums or blight, or meet a
# community development need having a particular urgency because existing
# conditions pose a serious and immediate threat to the health or welfare of the
# community and other financial resources are not available to meet that need.
# With respect to activities that principally benefit low- and moderate-income
# persons, at least 51 percent of the activity's beneficiaries must be low and
# moderate income. For CDBG, a person is considered to be of low income only if
# he or she is a member of a household whose income would qualify as "very low
# income" under the Section 8 Housing Assistance Payments program. Generally,
# these Section 8 limits are based on 50% of area median. Similarly, CDBG
# moderate income relies on Section 8 "lower income" limits, which are generally
# tied to 80% of area median. These data are derived from the 2011-2015 American
# Community Survey (ACS) and based on Census 2010 geography.

# https://www.hudexchange.info/programs/acs-low-mod-summary-data/

?configr::read.config
cfg <- configr::read.config()
str(cfg$default)

# list2env(cfg$default, .GlobalEnv)

attach(cfg$default)



# ZIP to TRACT crosswalk --------------------------------------------------

list.files(HUD_DATADIR)

# ZIP_TRACT_fst 2020 ----
ZIP_TRACT_fst <- file.path(HUD_WORKDIR, "ZIP_TRACT.fst"); print(file.info(ZIP_TRACT_fst)['size'])
if(file.exists(ZIP_TRACT_fst)) {
  print(fst.metadata(ZIP_TRACT_fst))
  ZIP_TRACT <- read_fst(ZIP_TRACT_fst, as.data.table=TRUE)
} else {
  ZIP_TRACT_xlsx<-file.path(HUD_DATADIR, "ZIP_TRACT_062024.xlsx"   )
  ZIP_TRACT <- readxl::read_xlsx(ZIP_TRACT_xlsx) %>% setDT(key = 'ZIP')
  write_fst(ZIP_TRACT,ZIP_TRACT_fst); print(file.info(ZIP_TRACT_fst)['size'])
}; str(ZIP_TRACT)
ZIP_TRACT

ZIP_TRACT[, .(sum(TOT_RATIO)), by=.(ZIP)]
ZIP_TRACT[ZIP=="10010"]
ZIP_TRACT[ZIP3=="100"]

ZIP_TRACT[, ZIP3:=substr(ZIP,1,3)]
