

# tab20_tract20_tract10_natl_fst ----
get_tab20_tract20_tract10_natl <- function(){
  tab20_tract20_tract10_natl_fst <- file.path(the$CENSUS_WORKDIR, "tab20_tract20_tract10_natl.fst"); print(file.info(tab20_tract20_tract10_natl_fst)['size'])
  if(file.exists(tab20_tract20_tract10_natl_fst)) {
    print(fst.metadata(tab20_tract20_tract10_natl_fst))
    tab20_tract20_tract10_natl <- read_fst(tab20_tract20_tract10_natl_fst, as.data.table=TRUE)
  } else {
    ?fread

    getOption("datatable.integer64")
    tab20_tract20_tract10_natl <- fread("https://www2.census.gov/geo/docs/maps-data/data/rel2020/tract/tab20_tract20_tract10_natl.txt"
                                        ,integer64 = "character")
    ?sub
    geoid_tract_cols <- c('GEOID_TRACT_10','GEOID_TRACT_20')
    tab20_tract20_tract10_natl[, (geoid_tract_cols):=lapply(.SD,sub, pattern = "(\\d+)(\\d{2})$", replacement = "\\1.\\2", perl = TRUE), .SDcols = geoid_tract_cols]

    # tab20_tract20_tract10_natl[, GEOID_TRACT_10:=sub(x = GEOID_TRACT_10, pattern = "(\\d+)(\\d{2})$", replacement = "\\1.\\2", perl = TRUE)]
    write_fst(tab20_tract20_tract10_natl,tab20_tract20_tract10_natl_fst); print(file.info(tab20_tract20_tract10_natl_fst)['size'])
  }; str(tab20_tract20_tract10_natl)
  tab20_tract20_tract10_natl
}



# CA census tract 2010 to 2000 rel file -----------------------------------
# ca06trf_fst ----
get_ca06trf <- function(){
  ca06trf_fst <- file.path(the$CENSUS_WORKDIR, "ca06trf.fst"); print(file.info(ca06trf_fst)['size'])
  if(file.exists(ca06trf_fst)) {
    print(fst.metadata(ca06trf_fst))
    ca06trf <- read_fst(ca06trf_fst, as.data.table=TRUE)
  } else {
    geoid_tract_cols <- c('GEOID00','GEOID10')
    #   ?connection
    # tmp <- strsplit(url("https://www2.census.gov/geo/docs/maps-data/data/rel/trfheader.txt"), split = ",")[[1]]
    # str(tmp)
    # ?strsplit
    col.names <- strsplit("STATE00,COUNTY00,TRACT00,GEOID00,POP00,HU00,PART00,AREA00,AREALAND00,STATE10,COUNTY10,TRACT10,GEOID10,POP10,HU10,PART10,AREA10,AREALAND10,AREAPT,AREALANDPT,AREAPCT00PT,ARELANDPCT00PT,AREAPCT10PT,AREALANDPCT10PT,POP10PT,POPPCT00,POPPCT10,HU10PT,HUPCT00,HUPCT10"
                          , split = ",")[[1]]
    # list(character = "STATE00", character = "COUNTY00", character = "TRACT00",
    #       character = "GEOID00", integer = "POP00", integer = "HU00",
    #       character = "PART00", numeric = "AREA00", numeric = "AREALAND00",
    #       character = "STATE10", character = "COUNTY10", character = "TRACT10",
    #       character = "GEOID10", integer = "POP10", integer = "HU10",
    #       character = "PART10", numeric = "AREA10", numeric = "AREALAND10",
    #       numeric = "AREAPT", numeric = "AREALANDPT", numeric = "AREAPCT00PT",
    #       numeric = "ARELANDPCT00PT", numeric = "AREAPCT10PT", numeric = "AREALANDPCT10PT",
    #       numeric = "POP10PT", numeric = "POPPCT00", numeric = "POPPCT10",
    #       numeric = "HU10PT", numeric = "HUPCT00", numeric = "HUPCT10")
    ca06trf <- fread("https://www2.census.gov/geo/docs/maps-data/data/rel/trf_txt/ca06trf.txt"
                     , col.names = col.names
                     ,colClasses = c("character", "character", "character", "character", "integer",
                                     "integer", "character", "numeric", "numeric", "character", "character",
                                     "character", "character", "integer", "integer", "character",
                                     "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric")

                     # ,integer64 = "character"
                     )
    str(ca06trf)
    ca06trf[, (geoid_tract_cols):=lapply(.SD,sub, pattern = "(\\d+)(\\d{2})$", replacement = "\\1.\\2", perl = TRUE), .SDcols = geoid_tract_cols]
    write_fst(ca06trf,ca06trf_fst); print(file.info(ca06trf_fst)['size'])
  }; str(ca06trf)
  ca06trf
}
# https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2020.html



# tab20_zcta520_county20_natl crosswalk -----------------------------------------------

get_tab20_zcta520_county20_natl <- function(){

  tab20_zcta520_county20_natl_fst <- file.path(the$CENSUS_WORKDIR, "tab20_zcta520_county20_natl.fst"); print(file.info(tab20_zcta520_county20_natl_fst))
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

