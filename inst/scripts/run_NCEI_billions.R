# setup -------------------------------------------------------------------
rm(list = ls())
source("~/Spatial/.RProfile")
library(configr)
configr::read.config();ls(all.names = TRUE)
devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)
# source("~/Spatial/Tigris/tigris_setup.R")

stop()

list.files(.NRI_datadir, full.names = TRUE)

browseURL(NRI_WORKDIR)

# time-series of costs unnormalized cpi-adjusted -------------------------------------------------------------
# https://www.ncei.noaa.gov/access/billions/time-series/NECR/cost
time_series_fst <- file.path(NOAA_WORKDIR, "time_series.fst"); print(file.info(time_series_fst))
if(file.exists(time_series_fst)) {
  print(fst.metadata(time_series_fst))
  time_series <- read_fst(time_series_fst, as.data.table = TRUE)
} else {

  time_series_csv.vec <- list.files(.ncei_datadir, pattern = "time-series.*", full.names = TRUE)
  print(time_series_csv.vec)
  # ?rio::import_list
  time_series <- rio::import_list(time_series_csv.vec, rbind=TRUE, setclass = 'data.table') %>% sanitize()
  str(time_series)
  time_series[, table(STATE)]
  (range_cols <- grep("_RANGE", names(time_series), value = TRUE))
  # sub("RANGE","MIN", range_cols)
  # ?stri_split_fixed
  time_series[, sub("RANGE","MIN", range_cols):=lapply(.SD,.%>% stri_extract_first(regex="\\d+") %>% as.numeric()), .SDcols=range_cols]
  time_series[, sub("RANGE","MAX", range_cols):=lapply(.SD,.%>% stri_extract_last(regex="\\d+") %>% as.numeric()), .SDcols=range_cols]
  time_series[, ALL_DISASTERS_EX_DRAUGHT_COST_MAX:= ALL_DISASTERS_COST_MAX -DROUGHT_COST_MAX]
  time_series[, ALL_DISASTERS_EX_DRAUGHT_COST_MIN:= ALL_DISASTERS_COST_MIN -DROUGHT_COST_MIN]
  write_fst(time_series, path = time_series_fst);   print(file.info(time_series_fst))

  time_series
}; str(time_series)
time_series
str(time_series)

table(time_series$STATE)

time_series[STATE %in% unique(tigris::fips_codes$state), .(YEAR, STATE, ALL_DISASTERS_COST_MIN ,ALL_DISASTERS_COST_MAX )]
debugonce(yscale.components.log)
?panel.segplot
args(panel.segplot)
segplot(YEAR~ALL_DISASTERS_EX_DRAUGHT_COST_MIN+ALL_DISASTERS_EX_DRAUGHT_COST_MAX|STATE
        , data = subset(time_series, subset= STATE %in% "LA"  & # unique(tigris::fips_codes$state) &
                                                  ALL_DISASTERS_EX_DRAUGHT_COST_MAX>0 &
                          YEAR>=2000
        )
        , horizontal = FALSE
        , panel=function(x,y,z,level,subscripts,at,...) {
          dots <- list(...); str(dots)
         # browser()
          panel.segplot(x,y,z,level=level,subscripts,at,...)
        }
        #        , scales = list( y = list(logsc = 10))
        , yscale.components = yscale.components.dollar_M(scale = 1e-3, suffix = "B")
        ,col="red", lwd=2, as.table=TRUE)


ts <- time_series[STATE=="FL", .(YEAR,ALL_DISASTERS_EX_DRAUGHT_COST_MIN,ALL_DISASTERS_EX_DRAUGHT_COST_MAX)]
plot(ALL_DISASTERS_EX_DRAUGHT_COST_MAX~YEAR, ts, type="n", xlim=c(1980,2024), main="FL", ylab="$1000 US", las=2)
segments(x0 = ts$YEAR, y0 = ts$ALL_DISASTERS_EX_DRAUGHT_COST_MIN, y1=ts$ALL_DISASTERS_EX_DRAUGHT_COST_MAX, lwd=2)

?ts
FL.ts <- ts(time_series[STATE=="FL", .(ALL_DISASTERS_EX_DRAUGHT_COST_MIN,ALL_DISASTERS_EX_DRAUGHT_COST_MAX)],start=1980)
plot(FL.ts, type="h", plot.type = "single")

source("~/lattice_setup.R")
?xyplot.ts
xyplot.ts(FL.ts)


# US-cost -----------------------------------------------------------------

US_cost <- time_series[grepl("US-cost", `_FILE`)] %>%  remove_na_cols()

(severe_storm_colnames <-grep("SEVERE_STORM", names(US_cost), value = TRUE))
(tropical_cyclone_colnames <-grep("TROPICAL_CYCLONE", names(US_cost), value = TRUE))
 US_cost[, .SD, .SDcols=c('YEAR',tropical_cyclone_colnames)]
dim(US_cost)
 barplot(TROPICAL_CYCLONE_COST ~YEAR, US_cost)

US_cost[YEAR %between% c(1980,2007),.(sum(TROPICAL_CYCLONE_COUNT), sum(TROPICAL_CYCLONE_COST))]
xyplot(TROPICAL_CYCLONE_COUNT ~YEAR, US_cost, type="h")
 xyplot(TROPICAL_CYCLONE_COST ~YEAR, US_cost, type="h")
