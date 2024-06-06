# identities --------------------------------------------------------------


## Risk Value --------------------------------------------------------------

NRI_counties_dt[, .(STCOFIPS,  EAL_VALB+EAL_VALPE+EAL_VALA,EAL_VALT)]

NRI_counties_dt[, .(STCOFIPS,RISK_VALUE, EAL_VALT, RISK_VALUE, EAL_VALT*CRF_VALUE)]

NRI_counties_dt[, .(STCOFIPS,BUILDVALUE, EAL_VALB, EAL_VALB/BUILDVALUE, ALR_VALB)]

EALB.cols <- grep("EALB$", names(NRI_counties_dt), value = TRUE)

NRI_counties_dt[, .(STCOFIPS,  EAL_VALB, rowSums(.SD, na.rm = TRUE)), .SDcols =  EALB.cols]

cbind(sum(NRI_counties_dt[, .SD, .SDcols=EALB.cols], na.rm = TRUE) , NRI_counties_dt[, .(EAL_VALB=sum(EAL_VALB, na.rm = TRUE))]) %>%as_tibble() # total expected annual loss on building value
