# https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2020.html



# tab20_zcta520_county20_natl crosswalk -----------------------------------------------

get_tab20_zcta520_county20_natl <- function(){

  tab20_zcta520_county20_natl_fst <- file.path(.census_workdir, "tab20_zcta520_county20_natl.fst"); print(file.info(tab20_zcta520_county20_natl_fst))
  if(file.exists(tab20_zcta520_county20_natl_fst)) {
    print(fst.metadata(tab20_zcta520_county20_natl_fst))
    tab20_zcta520_county20_natl <- read_fst(tab20_zcta520_county20_natl_fst, as.data.table = TRUE)
  } else {

    tab20_zcta520_county20_natl_txt <- file.path(CENSUS_DATADIR, "rel2020", "zcta520","tab20_zcta520_county20_natl.txt")
    # ?fread
    (tab20_zcta520_county20_natl <- fread(tab20_zcta520_county20_natl_txt
                                          ,na.strings=c("NA","")
                                          , colClasses = c(OID_ZCTA5_20 = "character", GEOID_ZCTA5_20 = "character", NAMELSAD_ZCTA5_20 = "character",
                                                           AREALAND_ZCTA5_20 = "numeric", AREAWATER_ZCTA5_20 = "numeric",
                                                           MTFCC_ZCTA5_20 = "character", CLASSFP_ZCTA5_20 = "character",
                                                           FUNCSTAT_ZCTA5_20 = "character", OID_COUNTY_20 = "character",
                                                           GEOID_COUNTY_20 = "character", NAMELSAD_COUNTY_20 = "character",
                                                           AREALAND_COUNTY_20 = "numeric", AREAWATER_COUNTY_20 = "numeric",
                                                           MTFCC_COUNTY_20 = "character", CLASSFP_COUNTY_20 = "character"
                                                           , FUNCSTAT_COUNTY_20 = "character"
                                                           , AREALAND_PART = "numeric",AREAWATER_PART = "numeric")))



    # dput(sapply(tab20_zcta520_county20_natl,class))


    write_fst(tab20_zcta520_county20_natl, path = tab20_zcta520_county20_natl_fst);   print(file.info(tab20_zcta520_county20_natl_fst))
  }; str(tab20_zcta520_county20_natl)
  tab20_zcta520_county20_natl
}

