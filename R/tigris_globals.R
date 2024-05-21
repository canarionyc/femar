utils::globalVariables(c('.'))

# fips codes --------------------------------------------------------------

utils::globalVariables(c('STATEFIPS','COUNTYFIPS','STCOFIPS','TRACT','TRACTFIPS'))

# states ------------------------------------------------------------------
utils::globalVariables(c("STATEFP", "STATENS", "AFFGEOID", "GEOID", "STUSPS", "NAME",
                         "LSAD", "ALAND", "AWATER", "geometry"))


# counties ----------------------------------------------------------------


utils::globalVariables(c("STATEFP", "COUNTYFP", "COUNTYNS", "GEOID", "NAME", "NAMELSAD",
                         "LSAD", "CLASSFP", "MTFCC", "CSAFP", "CBSAFP", "METDIVFP", "FUNCSTAT",
                         "ALAND", "AWATER", "INTPTLAT", "INTPTLON", "geometry"))


# tabblock ----------------------------------------------------------------

utils::globalVariables(c("STATEFP20", "COUNTYFP20", "TRACTCE20", "BLOCKCE20", "GEOID20",
                                                                           "GEOIDFQ20",
                         "NAME20", "MTFCC20", "UR20", "UACE20", "FUNCSTAT20",
                         "ALAND20", "AWATER20", "INTPTLAT20", "INTPTLON20", "HOUSING20",
                         "POP20"))
