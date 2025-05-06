# setup -------------------------------------------------------------------
rm(list = ls())
source("~/Spatial/.RProfile")
library(configr)
configr::read.config();ls(all.names = TRUE)
devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)
# source("~/Spatial/Tigris/tigris_setup.R")

stop()

list.files(the$NRI_DATADIR, full.names = TRUE)

browseURL(the$NRI_WORKDIR)
browseURL(the$NCEI_DATADIR)
# time-series of costs unnormalized cpi-adjusted -------------------------------------------------------------
# https://www.ncei.noaa.gov/access/billions/time-series/NECR/cost
time_series_fst <- file.path(the$NOAA_WORKDIR, "time_series.fst"); print(file.info(time_series_fst))
if(file.exists(time_series_fst)) {
  print(fst.metadata(time_series_fst))
  time_series <- read_fst(time_series_fst, as.data.table = TRUE)
} else {

  time_series_csv.vec <- list.files(the$NCEI_DATADIR, pattern = "time-series.*", full.names = TRUE)
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
?segplot

fl_data <-  subset(time_series, subset= STATE %in% "FL"  & # unique(tigris::fips_codes$state) &
                     ALL_DISASTERS_EX_DRAUGHT_COST_MAX>0 &
                     YEAR>=2000
)
fl_data[, .(YEAR,ALL_DISASTERS_COST_MIN,ALL_DISASTERS_COST_MAX)]

latticeExtra::segplot(YEAR~ALL_DISASTERS_EX_DRAUGHT_COST_MIN+ALL_DISASTERS_EX_DRAUGHT_COST_MAX|STATE
                      , data = subset(time_series, subset= STATE %in% "FL"  & # unique(tigris::fips_codes$state) &
                                        ALL_DISASTERS_EX_DRAUGHT_COST_MAX>0 &
                                        YEAR>=2000
                      )
                      , horizontal = TRUE
                      , panel=function(x,y,z,level,subscripts,at,...) {
                        dots <- list(...); str(dots)
                        # browser()
                        panel.segplot(x,y,z,level=level,subscripts,at,...)
                      }
                      #        , scales = list( y = list(logsc = 10))
#                      , xscale.components = xscale.components.dollar_M(scale = 1e-3, suffix = "B")
                      ,col="red", lwd=2, as.table=TRUE)


time_series[STATE=="FL" & YEAR>=2000,.(YEAR, ALL_DISASTERS_COST_RANGE)]

plot(c(as.Date('2000-01-01'), as.Date('2025-01-01')), c(0, 200), type="n", ylab="Combined Cost Range ($B)", xlab=NA
     ,main="Florida Billion-Dollar Weather and Climate Disaster Events 2000-2025 (CPI-Adjusted)"
     , sub="Source: https://www.ncei.noaa.gov/")
time_series[STATE=="FL", rect(xleft=lubridate::ymd(YEAR, truncated=2),ybottom=ALL_DISASTERS_EX_DRAUGHT_COST_MIN/1e3
                              ,xright=lubridate::ymd(YEAR, truncated=2)+months(12)
                              ,ytop=ALL_DISASTERS_EX_DRAUGHT_COST_MAX/1e3
                              , col="red", border = "black"

                             )]


source("~/lattice_setup.R")
?xyplot.ts
xyplot.ts(FL.ts)

library(readxl)
?read_xlsx

bill <- read_xlsx(file.path(the$NCEI_DATADIR,"time-series-FL-cost-1980-2025.xlsx"), sheet = "Billion-Dollar", skip = 5L
                  ,col_names = c("Event", "Type", NULL,"Begin", NULL, "End","Cost","Deaths")
                  , col_types = c("text","text","skip","text","skip","text","text", "numeric")) %>%
  as.data.table() %>% na.omit()
str(bill)
bill <- na.omit(bill)


bill$Begin <- mdy(bill$Begin ) # %>% floor_date(unit = "months")
bill$End <- mdy(bill$End)  # %>% floor_date(unit = "months")
bill$Cost <- gsub(pattern = "[\\*\\$]", replacement = "", bill$Cost, perl = TRUE) %>%as.numeric()
bill[, Year :=year(Begin)]
bill
setDT(bill, key = c("Begin",        "End"))

bill_stats <- bill[, .(.N, Cost=sum(Cost), Event=trimws(paste(fifelse(Cost>=50,Event,""), collapse = " "))), keyby = c("Year")]
bill_stats
?barplot.formula
mp <- barplot(Cost ~ Year, bill_stats, las=2, col=2
              #, ylim=c(0,250)
              )# ;abline(v=mp[,1])
# ?text
text(x=mp[,1], y=bill_stats$Cost*0.5, labels = bill_stats$Event,srt=90
      ,adj=c(0.5,0.5)
#     ,pos=1, offset=0
,xpd=1
     )
library(wordcloud)
?wordlayout
lay <- wordlayout(x=mp[,1], y=bill_stats$Cost, words = bill_stats$Event, xlim = c(0,25), rotate90=FALSE)
lay
text(lay[,1], lay[,2], bill_stats$Event, srt=0, adj=c(0.5,0))
?textplot
?strwidth



# barplot -----------------------------------------------------------------

dev.off()
debugonce(barplot)
bill_stats
mp <- barplot(bill_stats$Cost, names.arg=bill_stats$Year, las=2, col=2
              , ylab="Annual Combined Cost ($B)", xlab=NA
              , main="Florida Billion-Dollar Weather and Climate Disaster Events 2000-2025 (CPI-Adjusted)"
              , sub="Source: https://www.ncei.noaa.gov/")
mp


text(x=mp[,1], y=bill_stats$Cost*0., labels = bill_stats$Event,srt=90
     ,adj=c(0,0.5)
     #     ,pos=1, offset=0
     ,xpd=1
)

# florida_billion_dollar_disasters_png ----

florida_billion_dollar_disasters_png <- file.path(the$FEMA_WORKDIR, format(Sys.time(),"florida_billion_dollar_disasters_%Y%m%d_%H%M.png")); print(file.info(florida_billion_dollar_disasters_png))
library(Cairo)
dev.copy(device=Cairo::CairoPNG,filename = florida_billion_dollar_disasters_png, width = 10.0, height = 6.0, dpi=300, units="in")
dev.off()
print(file.info(florida_billion_dollar_disasters_png)['size'])
browseURL(dirname(florida_billion_dollar_disasters_png))



# -------------------------------------------------------------------------


plot(c(as.Date('2000-01-01'), as.Date('2025-01-01')), c(0, 210), type="n", xlab=NA, ylab=NA)
?rect
rect(xleft =bill$Begin, ybottom =  0, xright  = bill$End, ytop =bill$Cost
     , col="red", border = NA)
library(wordcloud)
?wordlayout
lay <- wordlayout(x=bill$Begin, y = bill$Cost, words=bill$Event, rotate90 = TRUE, cex=2/3
                  ,xlim=c(as.Date('2000-01-01'), as.Date('2025-01-01'))
#                  , ylim= c(0, 210)
                  )
lay
text(lay[,1]+.5*lay[,3],lay[,2]+.5*lay[,4],bill_stats$Event, srt=90, xpd = TRUE, cex=2/3)
rect(xleft = lay[,1], ybottom = lay[,2], xright = lay[,1]+lay[,3], ytop = lay[,2]+lay[,4])
?text
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
