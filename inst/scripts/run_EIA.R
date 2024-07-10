
#+ setup -------------------------------------------------------------------
# rm(list = ls())
source("~/Spatial/.RProfile")
options(digits = 3L)
options(scipen = 999L);getOption("scipen")

library(configr)
configr::read.config()
devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)
print(getOption("tigris_year"))
# source("~/lattice_setup.R")


options(rio.import.class='data.table')
getOption("rio.import.class")
# ?rio::import
library(survey)
getOption("survey.multicore")

?svybys
?as.formula
by.vars <- c('TYPEHUQ'
             #             ,'CLIMATE_REGION'
             ,'STATE_POSTAL'
             #             ,'KOWNRENT'
             ,'YEARMADERANGE')
(by.vars.fmla <- as.formula(paste("~ ", paste(by.vars, collapse= "+"))))

stop()

browseURL("https://r-survey.r-forge.r-project.org/survey/")


browseURL(system.file("extdata","DOE","EIA","RECS", "RECS_2020_Codebook_for_Public_File_-_v7.xlsx", package="femar"))
file.choose()
load( file.path(.NRI_workdir, "run_EIA.RData"))

browseURL(.EIA_workdir)
list.files(.EIA_datadir, full.names = TRUE, recursive = TRUE)
#dir.create(.EIA_workdir)
?strOptions
?make.formula
make.formula
op <- options(str=strOptions())

# https://www.eia.gov/consumption/residential/

# TYPEHUQ
# 1 Mobile home
# 2 Single-family house detached from any other house
# 3 Single-family house attached to one or more other houses (for example: duplex, row house, or townhome)
# 4 Apartment in a building with 2 to 4 units
# 5 Apartment in a building with 5 or more units

# FUELHEAT
# 1 Natural gas from underground pipes
# 2 Propane (bottled gas)
# 3 Fuel oil
# 5 Electricity
# 7 Wood or pellets
# 99 Other
# -2 Not applicable

(site_to_source.df <- data.frame(fuel_type=names(site_to_source.lst), fuel_source_to_site=unlist(site_to_source.lst, use.names = FALSE)))

# RECS2020 ----------------------------------------------------------------
RECS2020 <- get_RECS2020()
str(RECS2020$COUNT)
setnames(RECS2020, 'COUNT', 'HOUSEHOLD')

# RECS2020[,TOTALBTU.calc := BTUEL+BTUNG+BTULP+BTUFO+BTUWD ]
# RECS2020[,SOURCE_TOTALBTU := 2.8*BTUEL+1.05*BTUNG+1.01*BTULP+1.01*BTUFO+1.00*BTUWD ]
# RECS2020[, .(TOTALBTU, TOTALBTU.calc,SOURCE_TOTALBTU)]

# levels(RECS2020$TYPEHUQ) <- c("MH", "SF.detached", "SF.attached", "APT.2-4u", "APT.5u+")
# levels(RECS2020$TYPEHUQ) <- as.character(1:5)

dim(RECS2020)
str(RECS2020, give.attr=FALSE, list.len=999L)
View(RECS2020)

?svytotal

str(RECS2020$TYPEHUQ)

# ?fct_recode
# RECS2020$TYPEHUQ <- fct_recode()

# recs_mf <- RECS2020[TYPEHUQ=="5"]
# recs_mf%>% as_tibble()
#
# recs_mf[, .(.N), keyby = c('STATE_FIPS','KOWNRENT')]
# (recs_mf_zz <- recs_mf[, .(.N), keyby = c('KOWNRENT')])

# RECS2020[, .(.N, BTUNG=weighted.mean(BTUNG,NWEIGHT), BTUEL=weighted.mean(BTUEL,NWEIGHT))]


# Climate Zones in RECS2020 -----------------------------------------------


(climate_dt <- RECS2020[, unique(.SD), .SDcols=c('STATE_POSTAL', 'STATE_NAME', 'CLIMATE_REGION', 'IECC_CLIMATE_CODE')
                        , keyby = 'STATE_FIPS'])
key(climate_dt)




climate_dt

RECS2020[, table(BA_CLIMATE, useNA = "ifany")]
RECS2020[, table(CLIMATE_REGION, useNA = "ifany")]

RECS2020[, table(IECC_CLIMATE_CODE, useNA = "ifany")]



str(RECS2020$TYPEHUQ)
RECS2020$HOUSEHOLD
RECS2020$BA_CLIMATE



#+ Natural_Gas ----

# Calculate the frequency and RSE of hhs that used natural gas as their
# main space-heating fuel (Table HC6.1)

## Step 1 ----

# Create a new variable to flag the records of hhs that used
# natural gas as their main space-heating fuel. This new variable
# NG_MAINSPACEHEAT is equal to 1 if the hh used natural gas as its main
# space-heating fuel and 0 otherwise.

RECS2020$NG_MAINSPACEHEAT <- ifelse(RECS2020$FUELHEAT == 1, 1, 0)

## Step 2. ----


## Step 3. ----

# Number of hh that use NG heating in main space
RECS2020[, .(NG_MAINSPACEHEAT=sum(NG_MAINSPACEHEAT*NWEIGHT))]%>% as_tibble()


# Number of hh that use NG heating in main space per property type
RECS2020[, .(NG_MAINSPACEHEAT=sum(NG_MAINSPACEHEAT*NWEIGHT),NWEIGHT=sum(NWEIGHT)), keyby = .(TYPEHUQ)]%>% as_tibble()

# Define the survey design with the Jackknife replicate weights to calculate
# appropriate standard errors uising `svrepdesign`:
?svrepdesign

RECS2020_5 <- RECS2020[TYPEHUQ=="5"]

# RECS --------------------------------------------------------------------

# Define the Jackknife replicate weights you will use for estimation:
(repweights<-select(RECS2020,NWEIGHT1:NWEIGHT60))
names(RECS2020)

(RECS <- svrepdesign(data = RECS2020,
                     weight = ~NWEIGHT,
                     repweights = repweights,
                     type = "JK1",
                     combined.weights = TRUE,
                     scale = (ncol(repweights)-1)/ncol(repweights),
                     mse = TRUE))
summary(RECS)
class(RECS); methods(class="svyrep.design")
dim(RECS)


?svytotal
(HOUSEHOLD.total <- svytotal(~HOUSEHOLD, design = RECS))
confint(HOUSEHOLD.total)

?svyby



# attach(RECS2020)


# RECS5 -------------------------------------------------------------------
?subset.svyrep.design

RECS5_rds <- file.path(.fema_workdir, "RECS5.rds"); print(file.info(RECS5_rds))
if(file.exists(  RECS5_rds)) {
  RECS5 <- readRDS(RECS5_rds)
} else {

  RECS5 <- subset(RECS, TYPEHUQ=="5")
  # RECS5 <- transform(RECS5, BA_CLIMATE7=fct_collapse(BA_CLIMATE, "Subarctic/Very Cold"=c("Subarctic","Very Cold")))
  attr(RECS5, "path") <- RECS5_rds
  saveRDS(RECS5,   RECS5_rds); print(file.info(RECS5_rds))
}; str(RECS5)

levels(RECS5$variables$BA_CLIMATE)

nrow(RECS5)

# Structural and geographic characteristics ----
## by Housing unit type (HC2.1) ----
browseURL("https://www.eia.gov/consumption/residential/data/2020/hc/pdf/HC%202.1.pdf")
browseURL(file.path(.EIA_datadir, "RECS", "HC 2.1.xlsx"))

# Total number of hh ----
?svyrepstat
RECS2020[, .(NWEIGHT=sum(NWEIGHT))]; (HC2_1 <- svytotal(~HOUSEHOLD, RECS))
class(HC2_1)
methods(class="svrepstat")


# svytotal(~HOUSEHOLD, design = subset(RECS, TYPEHUQ %in% 5))


library(weights)
help(package="weights")
weights(RECS) %>% colSums() %>% as_tibble()# total number of hh

# Square Footage ----------------------------------------------------------

## Total square footage of U.S. homes (HC10.1) ----
# https://www.eia.gov/consumption/residential/data/2020/hc/pdf/HC%2010.1.pdf

# sqft.col_names <- grep("^[^Z]", names(RECS2020), value = TRUE) %>%
#   grep(pattern="RANGE",  value = TRUE, invert = TRUE) %>%
#   grep(pattern="INC",  value = TRUE, invert = TRUE) %>%
#   grep(pattern="SQFT", x=., value = TRUE, perl = TRUE)
#
# (sqft.fmla <- as.formula(paste("~HOUSEHOLD+", paste(sqft.col_names,collapse  = "+"), sep =  " ")))

# stri_detect_regex(names(RECS2020), pattern = "^[^Z].*SQ")


browseURL("E:\\Datasets\\EIA\\RECS\\HC 10.1.pdf")
browseURL("E:\\Datasets\\EIA\\RECS\\HC 10.1.xlsx")
# Number of housing units (million)	Total square footage (billion square feet)
#           Total U.S.a	Totalb	Heated	Cooled
# All homes	  123.53	  224.62	199.32	164.87

HC10_1_rda <- file.path(.EIA_workdir, "HC10_1.rda"); print(file.info(HC10_1_rda))
if(file.exists(  HC10_1_rda)) {
  load(HC10_1_rda, verbose=TRUE)
} else {

  HC10_1.svytotal <- svytotal(~HOUSEHOLD+TOTSQFT_EN+TOTHSQFT+TOTCSQFT, RECS)

  print(HC10_1.svytotal)
  confint(HC10_1.svytotal)

  ?svybys
  # marginals ----
  # debugonce(svybys)
  HC10_1.svybys.svytotal <- svybys(~HOUSEHOLD+TOTSQFT_EN+TOTHSQFT+TOTCSQFT
                                   ,by=~TYPEHUQ+STATE_POSTAL, design = RECS, FUN = svytotal)
  print(HC10_1.svybys.svytotal)

  # full breakdown
  HC10_1.svyby.svytotal <- svyby(~HOUSEHOLD+TOTSQFT_EN+TOTHSQFT+TOTCSQFT
                                 ,by=~TYPEHUQ+STATE_POSTAL
                                 #                      +YEARMADERANGE
                                 , design = RECS, FUN = svytotal)
  print(HC10_1.svyby.svytotal)
  # str(HC10_1.svyby.svytotal)


  save(list = ls(pattern = "HC10_1\\.svy"), file = HC10_1_rda); print(file.info(HC10_1_rda))
}; str(HC10_1)

HC10_1.svyby.dt <- as.data.table(HC10_1.svyby)
HC10_1.svyby.dt[, .(sum(HOUSEHOLD),TOTSQFT_EN=sum(TOTSQFT_EN))]

## Average square footage of U.S. homes (HC10.9) ---------------------------
browseURL("E:\\Datasets\\EIA\\RECS\\HC 10.9.pdf")
list.files(file.path(.EIA_datadir, "RECS"), pattern = "\\.xlsx", full.names = TRUE)
browseURL("E:\\Datasets\\EIA\\RECS\\HC 10.9.xlsx")
?svyratio
HC10_9.rda <- file.path(.EIA_workdir, "HC10_9.rda"); print(file.info(HC10_9.rda))
if(file.exists(  HC10_9.rda)) {
  load(HC10_9.rda, verbose=TRUE)
} else {

  HC10_9.out1 <- svyratio(sqft.fmla,~HOUSEHOLD, RECS)
  print(HC10_9.out1)

  (HC10_9 <- readxl::read_xlsx("E:\\Datasets\\EIA\\RECS\\HC 10.9.xlsx", skip=3))
  names(HC10_9)
  str(HC10_9)
  print(HC10_9[1,])
  # class(HC10_9.out1)
  # ?svyby

  ## Housing unit type -------------------------------------------------------

  # HC10_9.svybys.svyratio <- svybys(~TOTSQFT_EN+TOTHSQFT+TOTCSQFT,by=~TYPEHUQ+STATE_POSTAL
  #                                  ,denominator=~HOUSEHOLD
  #                       , RECS, svyratio, keep.var=TRUE)

  print(HC10_9.svybys.svyratio)
  ?svytable
  ?svyboxplot
  # ?save

  # Census region and division
  # Number of housing units (million)		"Average square footage per housing unit"

  # Northeast	                21.92		  1,827
  # Midwest	                  27.04		  2,006
  # South	                    46.84		  1,806
  # West	                    27.72		  1,650


  (HC10_9.svyby_REGIONC <- svyby(~TOTSQFT_EN+TOTHSQFT+TOTCSQFT,by=~REGIONC

                                 ,denominator=~HOUSEHOLD, RECS, svyratio, keep.var=TRUE))
  (HC10_9.svyby_TYPEHUQ_REGIONC <- svyby(~TOTSQFT_EN+TOTHSQFT+TOTCSQFT,by=~TYPEHUQ+REGIONC
                                         #                       +YEARMADERANGE
                                         ,denominator=~HOUSEHOLD, RECS, svyratio, keep.var=TRUE))
  ?svyratio
  methods(class="svyratio")
  ?svyby
  HC10_9.svyby_TYPEHUQ_STATE_POSTAL <- svyby(~TOTSQFT_EN+TOTHSQFT+TOTCSQFT,by=~TYPEHUQ+STATE_POSTAL

                                             ,denominator=~HOUSEHOLD, RECS, svyratio, keep.var=TRUE)
  class(HC10_9.svyby_TYPEHUQ_STATE_POSTAL)
  HC10_9.svyby_TYPEHUQ_STATE_POSTAL %>%
    subset(TYPEHUQ %in% 4:5) %>%
    subset(STATE_POSTAL %in% c('CA')) %>%
    print()

  ?save
  save(HC10_9.svyby_REGIONC,HC10_9.svyby_TYPEHUQ_REGIONC,HC10_9.svyby_TYPEHUQ_STATE_POSTAL, file=HC10_9.rda); print(file.info(HC10_9.rda))
}; str(HC10_9.out1)


coef(HC10_9.svybys.svyratio)


# plot sqft per home ------------------------------------------------------

# HC10_1_rda <- file.path(.EIA_workdir, "HC10_1.rda"); print(file.info(HC10_1_rda))
# stopifnot(file.exists(  HC10_1_rda))
#   load(HC10_1_rda, verbose=TRUE)



## sqft per hh ----

HC10_9.rda <- file.path(.EIA_workdir, "HC10_9.rda"); print(file.info(HC10_9.rda))
stopifnot(file.exists(HC10_9.rda))
load(HC10_9.rda, verbose=TRUE)

methods(class = "svyby")
(HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt <- setDT(HC10_9.svyby_TYPEHUQ_STATE_POSTAL, key = c('TYPEHUQ', 'STATE_POSTAL'))%>% sanitize())
# (HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt <- HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt[! TYPEHUQ %in% 1:3, ] %>% droplevels())


HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt[, (3:8):=lapply(.SD, round), .SDcols = 3:8]
HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt

levels(HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt$TYPEHUQ)
?lattice::dotplot
?fct_reorder
?dcast.data.table
(HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt.wide <- dcast(HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt, STATE_POSTAL~TYPEHUQ
                                                    , value.var = c('TOTSQFT_EN_SLASH_COUNT'
                                                                    ,'TOTHSQFT_SLASH_COUNT', 'TOTCSQFT_SLASH_COUNT'
                                                                    ,'SETOTSQFT_EN_SLASH_COUNT')))

HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt.wide[, STATE_POSTAL:=fct_reorder(STATE_POSTAL, TOTSQFT_EN_SLASH_COUNT_4)]
levels(HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt.wide$STATE_POSTAL)
dotplot(STATE_POSTAL ~TOTSQFT_EN_SLASH_COUNT_2+TOTSQFT_EN_SLASH_COUNT_4+TOTSQFT_EN_SLASH_COUNT_5
        , data = HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt.wide
        ,drop.unused.levels = TRUE)

as.formula(paste("~",paste(c('TOTSQFT_EN_SLASH_COUNT' ,'TOTHSQFT_SLASH_COUNT', 'TOTCSQFT_SLASH_COUNT'), collapse = "+")))

dotplot(STATE_POSTAL ~TOTSQFT_EN_SLASH_COUNT_2+TOTSQFT_EN_SLASH_COUNT_4+TOTSQFT_EN_SLASH_COUNT_5
        , data = HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt.wide
        ,drop.unused.levels = TRUE)

dotplot(STATE_POSTAL ~TOTHSQFT_SLASH_COUNT, groups=TYPEHUQ, data = HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt)
dotplot(STATE_POSTAL ~TOTCSQFT_SLASH_COUNT, groups=TYPEHUQ, data = HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt)


bwplot( TYPEHUQ ~TOTHSQFT_SLASH_COUNT, data = HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt)
HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt[TOTHSQFT_SLASH_COUNT<500] # Hawai

bwplot( TYPEHUQ ~TOTCSQFT_SLASH_COUNT, data = HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt)

bwplot( TYPEHUQ ~TOTSQFT_EN_SLASH_COUNT, data = HC10_9.svyby_TYPEHUQ_STATE_POSTAL.dt)

# Consumption and Expenditures (Site Energy) ----
# https://www.eia.gov/consumption/residential/data/2020/index.php?view=consumption
## CE1.1 Summary consumption and expenditures in the U.S.  -------------------------------------------------------
?svytotal
# Total U.S.b	Total (trillion Btu)
# All homes	123.53	9,481

# CE1.1 Summary consumption and expenditures in the U.S. - totals and intensities
#https://www.eia.gov/consumption/residential/data/2020/c&e/pdf/ce1.1.pdf
# https://www.eia.gov/consumption/residential/data/2020/c&e/xls/ce1.1.xlsx
browseURL("https://www.eia.gov/consumption/residential/data/2020/c&e/xls/ce1.1.xlsx")
CE1_1_rda <- file.path(.EIA_workdir, "CE1_1.rda"); print(file.info(CE1_1_rda))
if(file.exists(  CE1_1_rda)) {
  load(CE1_1_rda, verbose=TRUE)
} else {
  (CE1_1_total<- svytotal(~HOUSEHOLD+TOTSQFT_EN+TOTALBTU, design = RECS))
  print(CE1_1_total)

  (CE1_1_total<- svytotal(~HOUSEHOLD+TOTSQFT_EN+TOTALBTU, design = RECS))
  ?svyby
  (CE1_1_total<- svyby(~TOTSQFT_EN+TOTALBTU,by=~HOUSEHOLD, design = RECS, FUN = svytotal
                       ,verbose=TRUE,vartype="ci"))
  methods(class="svyby")
  ?barplot.svyby

  # (CE1_1_bys <- svybys(~HOUSEHOLD+TOTSQFT_EN+TOTALBTU
  #                      , by=by.vars.fmla
  #                      , design = RECS, FUN = svytotal
  #                      ,keep.var =FALSE
  #                      ,verbose=TRUE))


  # names(CE1_1_bys) <- by.vars
  # str(CE1_1_bys)

  save(list=ls(pattern = "CE1_1_(total|bys)"),    file=CE1_1_rda); print(file.info(CE1_1_rda))
}; str(CE1_1)

methods(class = "svrepstat")



## by Fuel (includes Btu and physical unit tabs) ---------------------------
# Number of housing units (million)	"Total site energy consumptiona
# (trillion Btu)"
# Total U.S.b	                    Total	Electricity	Natural gas	Propane	Fuel oil or kerosene
# All homes	              123.53	9,481	    4,453	4,241	391	396
# Main heating fuel
# Natural gas	              62.71	5,996	    1,933	4,052	4	7
# Electricity	              42.57	2,081	    1,899	133	43	5
# Fuel oil or kerosene	     4.93	  547	      152	11	11	373
# Propane	                   5.21	  512	      198	1	312	Q
# Wood	                     2.25	  115	       83	5	18	9
# Some other fuelg	            Q	Q	Q	Q	Q	Q
# Does not use heating equipment	5.79	229	186	39	3	Q



# https://www.eia.gov/consumption/residential/data/2020/c&e/xls/ce2.1.xlsx
CE2_1_xlsx <- file.path(.EIA_datadir, "RECS", "ce2.1.xlsx"); print(file.info(CE2_1_xlsx))
?download.file
browseURL("https://www.eia.gov/consumption/residential/data/2020/c&e/xls/ce2.1.xlsx")
# capabilities("libcurl")
#  download.file("https://www.eia.gov/consumption/residential/data/2020/c&e/xls/ce2.1.xlsx"
#                , destfile = CE2_1_xlsx, method = "auto",cacheOK = FALSE )
browseURL(CE2_1_xlsx)

## btu_by_fuel_type: site energy consumption ----

btu.fmla <- ~BUEL
class(btu.fmla)
?svytotal
(btu_by_fuel_type <- svytotal(
  ~TOTALBTU
  +BTUEL # Total electricity use, in thousand Btu, 2020, including self-generation of solar power
  +BTUNG # Total natural gas use, in thousand Btu, 2020
  +BTULP # Total propane use, in thousand Btu, 2020
  +BTUFO # Total fuel oil/kerosene use, in thousand Btu, 2020
  +BTUWD # Total wood use, in thousand Btu, 2020
  # +TOTALBTUSPH # Total usage for space heating including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  # +TOTALBTUWTH # Total usage for water heating including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  # +TOTALBTUOTH # Total usage for 'Other' including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  # +TOTALBTU # Total usage including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  , design = RECS))
print(btu_by_fuel_type)
class(btu_by_fuel_type)
methods(class="svrepstat")
barplot(btu_by_fuel_type)

svyby(
  ~TOTALBTU
  +BTUEL # Total electricity use, in thousand Btu, 2020, including self-generation of solar power
  +BTUNG # Total natural gas use, in thousand Btu, 2020
  +BTULP # Total propane use, in thousand Btu, 2020
  +BTUFO # Total fuel oil/kerosene use, in thousand Btu, 2020
  +BTUWD
  , by=~HOUSEHOLD
  , design = RECS
  , FUN=svytotal)

# btu_fuel_per_hh_by_hh_type_and_state -----------------

(btu_fuel_by_hh_type_and_state <- svyby(
  ~HOUSEHOLD
  +TOTSQFT_EN+TOTHSQFT+TOTCSQFT
  +TOTALBTU
  +BTUEL # Total electricity use, in thousand Btu, 2020, including self-generation of solar power
  +BTUNG # Total natural gas use, in thousand Btu, 2020
  +BTULP # Total propane use, in thousand Btu, 2020
  +BTUFO # Total fuel oil/kerosene use, in thousand Btu, 2020
  +BTUWD # Total wood use, in thousand Btu, 2020
  # +TOTALBTUSPH # Total usage for space heating including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  # +TOTALBTUWTH # Total usage for water heating including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  # +TOTALBTUOTH # Total usage for 'Other' including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  # +TOTALBTU # Total usage including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  , by=~TYPEHUQ+STATE_POSTAL
  , FUN=svytotal
  , design = RECS))  %>% subset(STATE_POSTAL=="CA")
print(btu_fuel_by_hh_type_and_state %>% subset(STATE_POSTAL=="CA"))
class(btu_fuel_by_hh_type_and_state)
# barplot(btu_fuel_by_hh_type_and_state, beside=TRUE, legend=TRUE)

# btu_fuel_by_hh_type_and_state.df<- as.data.frame(btu_fuel_by_hh_type_and_state)
# sum(btu_fuel_by_hh_type_and_state.df$TOTALBTU)/1e12 # 9.48 trillion btu

(btu_fuel_per_hh_by_hh_type_and_state <- svyby(
  ~TOTSQFT_EN+TOTHSQFT+TOTCSQFT
  +TOTALBTU
  +BTUEL # Total electricity use, in thousand Btu, 2020, including self-generation of solar power
  +BTUNG # Total natural gas use, in thousand Btu, 2020
  +BTULP # Total propane use, in thousand Btu, 2020
  +BTUFO # Total fuel oil/kerosene use, in thousand Btu, 2020
  +BTUWD # Total wood use, in thousand Btu, 2020
  # +TOTALBTUSPH # Total usage for space heating including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  # +TOTALBTUWTH # Total usage for water heating including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  # +TOTALBTUOTH # Total usage for 'Other' including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  # +TOTALBTU # Total usage including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  , by=~TYPEHUQ+STATE_POSTAL
  , FUN=svyratio,denominator=~HOUSEHOLD
  , design = RECS)) %>% subset(STATE_POSTAL=="CA")

btu_fuel_per_hh_by_hh_type_and_state %>% subset(STATE_POSTAL=="CA")
class(btu_fuel_per_hh_by_hh_type_and_state)
?as.data.table
?base::as.data.frame

(btu_fuel_per_hh_by_hh_type_and_state.dt <- as.data.table(btu_fuel_per_hh_by_hh_type_and_state) %>% sanitize())%>% subset(STATE_POSTAL=="CA")
btu_fuel_per_hh_by_hh_type_and_state.dt[, TOTALBTU_SLASH_COUNT.check:=rowSums(.SD), .SDcols = BTUEL_SLASH_COUNT:BTUWD_SLASH_COUNT]
btu_fuel_per_hh_by_hh_type_and_state.dt[STATE_POSTAL=="CA",
                                        #                                        .( TYPEHUQ,STATE_POSTAL,TOTALBTU_SLASH_COUNT,TOTALBTU_SLASH_COUNT.check)
] %>% print()

site_to_source.df
str(btu_fuel_per_hh_by_hh_type_and_state.dt)
# btu_fuel_per_hh_by_hh_type_and_state.dt[, SOURCE_BTUEL_SLASH_COUNT:=2.8*BTUEL_SLASH_COUNT]
?melt.data.table
(btu_fuel_per_hh_by_hh_type_and_state_long <- melt.data.table(btu_fuel_per_hh_by_hh_type_and_state.dt
                                                              , id.vars = c('TYPEHUQ', 'STATE_POSTAL')
                                                              , value.name = "site_value") %>% add_source_to_site()
) %>% subset(STATE_POSTAL=="CA")
unique(btu_fuel_per_hh_by_hh_type_and_state_long$variable)




if(.msg){
  btu_fuel_per_hh_by_hh_type_and_state_long[grepl(pattern = "BTUEL", x = variable) & STATE_POSTAL=="CA"] %>% print()
  btu_fuel_per_hh_by_hh_type_and_state_long[grepl(pattern = "BTUNG", x = variable)& STATE_POSTAL=="CA"]%>% print()
  btu_fuel_per_hh_by_hh_type_and_state_long[grepl(pattern = "(BTULP|BTUFO)", x = variable)& STATE_POSTAL=="CA"]%>% print()
}
# btu_fuel_per_hh_by_hh_type_and_state_long[, value:=round(value,0)]

site_to_source.df
?as.data.table.data.frame
(btu_by_fuel_type.dt <- btu_by_fuel_type %>% as.data.table(keep.rownames="fuel_type"))

states_sf <- get_states_sf()


# btu_fuel_per_sqft_by_hh_type_and_state ------------------------------------

(btu_fuel_per_sqft_by_hh_type_and_state <- svyby(
  ~HOUSEHOLD
  +TOTALBTU
  +BTUEL # Total electricity use, in thousand Btu, 2020, including self-generation of solar power
  +BTUNG # Total natural gas use, in thousand Btu, 2020
  +BTULP # Total propane use, in thousand Btu, 2020
  +BTUFO # Total fuel oil/kerosene use, in thousand Btu, 2020
  +BTUWD # Total wood use, in thousand Btu, 2020
  # +TOTALBTUSPH # Total usage for space heating including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  # +TOTALBTUWTH # Total usage for water heating including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  # +TOTALBTUOTH # Total usage for 'Other' including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  # +TOTALBTU # Total usage including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  , by=~TYPEHUQ+STATE_POSTAL
  , FUN=svyratio,denominator=~TOTSQFT_EN
  , design = RECS)) %>% subset(STATE_POSTAL=="CA")

(btu_fuel_per_sqft_by_hh_type_and_state_long <- setDT(btu_fuel_per_sqft_by_hh_type_and_state, key=c('TYPEHUQ', 'STATE_POSTAL'))%>% melt.data.table(
  , id.vars = c('TYPEHUQ', 'STATE_POSTAL')
  , value.name = "site_value") %>% add_source_to_site()
)%>% subset(STATE_POSTAL=="CA")

print(btu_fuel_per_sqft_by_hh_type_and_state %>% subset(STATE_POSTAL=="CA"))

btu_fuel_per_sqft_by_hh_type_and_state

.btu_stats_rda <- file.path(.EIA_workdir, "btu_stats.rda"); print(file.info(.btu_stats_rda))
if(file.exists(  .btu_stats_rda)) {
  load(.btu_stats_rda, verbose=TRUE)
} else {
  save(list=ls(pattern = "^btu"), file =  .btu_stats_rda); print(file.info(.btu_stats_rda))
}; str(btu_stats)

# source_energy_consumption -----------------------------------------------

btu_by_fuel_type.dt[site_to_source.df,source_total:=total*fuel_source_to_site,  on='fuel_type']

btu_by_fuel_type.dt

btu_by_fuel_type.dt[, sum(total)]/1e12 #  9.82627 trillion BTU


# site_btu_by_usage ------------------------------------------------------------

(btu_by_usage <- svytotal(
  ~TOTALBTUSPH # Total usage for space heating including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  +TOTALBTUWTH # Total usage for water heating including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  +TOTALBTUOTH # Total usage for 'Other' including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  # +TOTALBTU # Total usage including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020
  , design = RECS) %>% as.data.frame())
sum(btu_by_usage$total)

sum(coef(btu_by_fuel_type))/1e12
svyratio(~TOTALBTU,~HOUSEHOLD, design = RECS);coef(out)['TOTALBTU']/coef(out)['HOUSEHOLD'] # 76.8 thousand BTU per hh
svyratio(~TOTALBTU,~TOTSQFT_EN, RECS);coef(out)['TOTALBTU']/coef(out)['TOTSQFT_EN'] # 42.2 thousand BTU/sqf
svyratio(~TOTALBTU,~NHSLDMEM, RECS) # 31.4 million BTU per hh member


# consumption -------------------------------------------------------------


(consumption.df <- as.data.frame(svytotal(~
                                            DOLLAREL+DOLLARNG+DOLLARLP+
                                            DOLLARFO
                                          , design = RECS)))
colSums(consumption.df) %>% as_tibble() # 232.7 billion dollars

?svyby
(RECS2020.stats <- svyby(~HOUSEHOLD+TOTSQFT_EN+TOTALBTU, by=~STATE_POSTAL+TYPEHUQ, design = RECS, FUN = svytotal
                         ,keep.var=FALSE))
class(RECS2020.stats)
print(RECS2020.stats)



## Step 4. ----

# Use `svytotal` to sum the number of hhs by `NG_MAINSPACEHEAT`, using the survey design defined above.
# debugonce(svytotal)
?svytotal
(NG_MAINSPACEHEAT_Total<-svytotal(~NG_MAINSPACEHEAT,RECS))
methods(class="svrepstat")

RECS2020[, .(NG_MAINSPACEHEAT=sum(NG_MAINSPACEHEAT*NWEIGHT))]%>% as_tibble()
# Answer. The estimated total hhs that used natural gas as their main
# space-heating fuel is 62,713,449 hhs. The calculation for the RSE is
# (483,047 / 62,713,449)*100 = 0.77. The sampling error is less than 1% of the
# estimate, which is relatively small. Alternatively, the RSE can be derived
# from:
(NG_MAINSPACEHEAT_Total_RSE<-(SE(NG_MAINSPACEHEAT_Total)/coef(NG_MAINSPACEHEAT_Total))*100)

# percent of hh that use NG
# To obtain the proportion estimate, use the `svymean()` function instead of the
# `svytotal` in the expression in Step 4. In addition, the confint() function
# provides the 95% confidence limits.
svymean(~NG_MAINSPACEHEAT,RECS)

RECS2020[, weighted.mean(NG_MAINSPACEHEAT,NWEIGHT)]


# Total BTU usage including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020 ----

svytotal(~TOTALBTU, RECS)

#+ BTU_NG  ----
# https://www.eia.gov/consumption/residential/data/2020/state/pdf/ce4.1.ng.st.pdf
# Calculate the sum and average of the total natural gas used for the hhs
# in South Carolina (SC) (Table CE4.1.NG.ST Annual hh site natural gas
# consumption in the United States by end use – totals and percentages, 2020).
# To calculate the total consumption estimates in R, use the svytotal()
# function; and use the `svymean()` function for the average consumption . In
# addition, use `svyby()` to group hhs by USENG and state (STATE_POSTAL)
# and the `subset()` function to limit the results to SC only.

# First, create a new variable to flag the hhs that have positive natural
# gas consumption for any natural gas end use. This new variable NGUSE is equal
# to 1 if BTUNG is greater than 0 and 0 otherwise. Then, run the survey design
# for the dataset again before producing estimates using the functions mentioned
# above.
lattice:: histogram(RECS2020$BTUNG)

RECS2020$NGUSE <- ifelse(RECS2020$BTUNG > 0, 1,0)
(RECS <- svrepdesign(data = RECS2020,
                     weight = ~NWEIGHT,
                     repweights = repweights,
                     type = "JK1",
                     combined.weights = TRUE,
                     scale = (ncol(repweights)-1)/ncol(repweights),
                     mse = TRUE)
)

# calculate the total:
(BTUNG_TOTAL<-svyby(~BTUNG, by=~STATE_POSTAL+NGUSE, RECS, svytotal))
# BTUNG_TOTAL<- BTUNG_TOTAL%>% subset(NGUSE==1)
class(BTUNG_TOTAL)
survey::dotchart(BTUNG_TOTAL)

?dotplot
BTUNG_TOTAL$STATE_POSTAL <- fct_reorder(BTUNG_TOTAL$STATE_POSTAL , BTUNG_TOTAL$BTUNG)
dotplot(STATE_POSTAL ~ BTUNG, BTUNG_TOTAL, subset=NGUSE==1)

?survey::dotchart.svyby

(BTUNG_SCTOTAL<-subset(BTUNG_TOTAL, STATE_POSTAL=='SC')) %>% as_tibble # trillion BTU


# The output below shows the result for SC. The total estimated consumption for
# hhs that used natural gas in SC is 26.2 trillion British thermal units
# (Btu). The RSE for the total is ( 2509266634/26220994238)*100 = 9.6%.

# calculate the mean:
?svyby
(BTUNG_MEAN<-svyby(~BTUNG, by=~STATE_POSTAL+NGUSE, subset(RECS, NGUSE==1), svymean))
BTUNG_MEAN$STATE_POSTAL <- fct_reorder(BTUNG_MEAN$STATE_POSTAL , BTUNG_MEAN$BTUNG)
class(BTUNG_MEAN)
dotchart(BTUNG_MEAN)

dotplot(STATE_POSTAL ~ BTUNG, BTUNG_MEAN, subset=NGUSE==1)

(BTUNG_SCMEAN<-subset(BTUNG_MEAN, STATE_POSTAL=='SC'))

# The average consumption per hh is 34.4 million BTU, with RSE=6.3. As
# mentioned in R example 1, the 95% confidence limits with the `conflint()`
# function. Note that the estimates for NGUSE = 0 reflect consumption for homes
# that do not use any natural gas.

#+ BTU_PER_SQFT ----

# Calculate the energy intensity per square foot by climate zone for the United
# States (Table CE1.1)

# https://www.eia.gov/consumption/residential/data/2020/c&e/pdf/ce1.1.pdf

## Step 1 ----

#Create a new variable called CLIMATE_REGION to combine climate zones, and rerun
#the survey design RECS.

?fct_collapse

(RECS <- svrepdesign(data = RECS2020,
                     weight = ~NWEIGHT,
                     repweights = repweights,
                     type = "JK1",
                     combined.weights = TRUE,
                     scale = (ncol(repweights)-1)/ncol(repweights),
                     mse = TRUE))

## Step 2 ----

# To calculate the energy intensity per square foot for all U.S. homes, use the
# `svyratio()` function.
(TOTALBTU_per_TOTSQFT_EN<-svyratio(~TOTALBTU, ~TOTSQFT_EN, RECS))

# The national estimate for energy intensity per square foot is about 42,000
# Btu, as shown in Table CE1.1. The RSE is (0.1801853/42.20056)*100 = 0.43.

# TOTALBTU_per_TOTSQFT_EN_by_CLIMATE_REGION --------------------------------------------

# To calculate the regional energy intensity per square foot, use `svyratio` with
#`svyby()`.
(TOTALBTU_per_TOTSQFT_EN_by_CLIMATE_REGION<-svyby(~TOTALBTU,denominator=~TOTSQFT_EN, by=~CLIMATE_REGION, RECS, svyratio))

?svyboxplot
opar <- par(las=2, mar=c(5.1, 7.1, 4.1, 2.1))
svyboxplot(TOTALBTU~CLIMATE_REGION,design = RECS, all.outliers=TRUE, horizontal=TRUE)
par( mar=c(5.1, 4.1, 4.1, 2.1))

class(RECS)
methods(class="svyrep.design")
model.frame(RECS) %>% as_tibble()
# As an example, the average total consumption per square foot in the hot-humid
# climate is about 35,000 Btu, as shown in the table below.


# TOTALBTU_per_TOTSQFT_EN_by_STATE_POSTAL ----------------------------------------------


(TOTALBTU_per_TOTSQFT_EN_by_STATE_POSTAL<-svyby(~TOTALBTU, by=~STATE_POSTAL,denominator=~TOTSQFT_EN, RECS, svyratio))

?graphics::dotchart
?dotchart.svyby
?dotchart.svrepstat
survey::dotchart(TOTALBTU_per_TOTSQFT_EN_by_STATE_POSTAL, main="TOTALBTU_per_TOTSQFT_EN_by_STATE_POSTAL", xlab="BTU per sqf")

# TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ ----------------------------------------------

(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ<-svyby(~TOTALBTU,denominator=~TOTSQFT_EN, RECS, svyratio, by=~TYPEHUQ))

svytable()
?svychisq

# TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_CLIMATE_REGION ------------------------------------


(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_CLIMATE_REGION<-svyby(~TOTALBTU,denominator=~TOTSQFT_EN, RECS, svyratio, by=~TYPEHUQ+CLIMATE_REGION))
ftable(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_CLIMATE_REGION)
methods(class="ftable")

(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_CLIMATE_REGION.df <- as.data.frame(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_CLIMATE_REGION))

xyplot(`TOTALBTU/TOTSQFT_EN`~TYPEHUQ, groups = CLIMATE_REGION, data = TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_CLIMATE_REGION.df, type="b")

# TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL ------------------------------------

TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL_fst <- file.path(.EIA_workdir, "TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL.fst"); print(file.info(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL_fst))
if(file.exists(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL_fst)) {
  print(fst.metadata(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL_fst))
  TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL <- read_fst(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL_fst, as.data.table = TRUE)
} else {

  (TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL<-svyby(~TOTALBTU,denominator=~TOTSQFT_EN, RECS, svyratio, by=~TYPEHUQ+STATE_POSTAL))
  ftable(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL)
  methods(class="ftable")

  (TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL.df <- as.data.frame(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL))
  str(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL.df)
  TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL <- setDT(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL.df, key = c('TYPEHUQ','STATE_POSTAL'))
  str(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL)
  write_fst(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL, path = TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL_fst);   print(file.info(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL_fst))
}; str(TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL)
TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL


xyplot(`TOTALBTU/TOTSQFT_EN`~TYPEHUQ, groups = STATE_POSTAL, data = TOTALBTU_per_TOTSQFT_EN_by_TYPEHUQ_STATE_POSTAL.df, type="b")

#+ R Example 4 ----

#Compare if the proportions and the consumption means for hhs using
#natural gas as their main space-heating fuel are statistically different among
#the hhs in different Census regions.

#Use the `svychisq()` function to obtain chi-square statistics.
?svyby
# BTU per Sqft by census region
? svyratio
?svymean
?svytotal
(TOTALBTU_per_TOTSQFT_EN_by_REGIONC<-svyby(formula = ~TOTALBTU, by=~REGIONC,denominator=~TOTSQFT_EN
                                           , design = RECS, FUN=svyratio))

survey::dotchart(TOTALBTU_per_TOTSQFT_EN_by_REGIONC)
(NGSPH_CHISQ<-svychisq(~NG_MAINSPACEHEAT+REGIONC,design=RECS,statistic="Chisq"))
class(NGSPH_CHISQ)
methods(class="htest")




# To compare if the average space-heating consumption estimates for hhs
# using natural gas as their main space-heating fuel are different among the
# Census regions, use the svyglm() function to run a regression model and obtain
# the coefficient, and use the regTermTest() function to obtain the F-statistic.
(RECS_NGSPH<-subset(RECS,NG_MAINSPACEHEAT==1))
svytotal(x = ~TOTALBTUSPH,design = RECS_NGSPH)

(NGSPH_REGIONGLM<-svyglm(TOTALBTUSPH~factor(REGIONC), design=RECS_NGSPH))

(regTermTest(NGSPH_REGIONGLM, ~factor(REGIONC), method="Wald"))

#+ cleanup -----------------------------------------------------------------

save.image(file = file.path(.NRI_workdir, "run_EIA.RData"))

# Notes to Consider When Using the Microdata File and Replicate Weights
# 1. Publication standards: We do not publish RECS estimates where the RSE is higher than 50 or the number of hhs used for the calculation is less than 10 (indicated by a Q in the data tables). We recommend following these guidelines for custom analysis using the public use microdata file.
# 2. Imputation variables: We imputed most variables for Don’t Know and Refuse responses. The Z variables, also referred to as imputation flags, are in the public use microdata file. The imputation flag indicates whether we based the corresponding non-Z variable was reported data (Z variable = 0) or if we imputed it (Z variable = 1). Variables from the RECS questionnaire that we did not impute, contained no missing data, or were not from the questionnaire have no corresponding Z variables. We recommend using the imputed data, where available, to avoid biased estimation.
# 3. Standardized coding: Variables that we did not ask all respondents use the response code –2 for Not Applicable. For example, respondents who answered that they did not use any televisions at home (TVCOLOR = 0) were not asked what size television they most use at home, so TVSIZE1 = -2. Use caution when performing calculations on variables that have -2 responses.
# 4. Indicator variables: The microdata file contains variables to indicate the use of major fuels and specific end uses within each housing unit for 2020. We derived these variables from answers given by each respondent, and they indicate whether the respondent had access to the fuel, used the fuel, and engaged in a specific end use. All indicators are either a 0 or a 1 for each combination of major fuel and end use. For example, respondents who say they heated their homes with electricity in 2020 will have the derived variable ELWARM = 1. If respondents say they have equipment but did not use it, the corresponding indicator is 0. As an example, respondents in a warm climate might have heating equipment but did not use it in 2020. For this case, ELWARM is 0.
# 5. Confidentiality: We collected the 2020 RECS under the authority of the Confidential Information Protection and Statistical Efficiency Act (CIPSEA). The agency, project staff, and our contractors and agents are personally accountable for protecting the identity of individual respondents. We took the following steps to avoid disclosing personally identifiable information in the public-use microdata file.
# June 2023
# U.S. Energy Information Administration | Using the Microdata File to Compute Estimates and Standard Errors 14
# • We removed local geographic identifiers of sampled housing units, such as addresses.
# • We removed the following variables because we received too few responses or because we found a disclosure risk:
#   – COMBINED (on-site combined heat and power)
# – WIND (on-site wind generation)
# – PVINSTALL (year photovoltaic solar [PV] was installed)
# – PVCAPACITY (capacity of PV system in kilowatts)
# – APTEVCHG (do respondents in apartment building with 5+ units have access to an electric vehicle [EV] charger)
# – EVMAKE, EVMODEL, EVYEAR (EV make, model, and year)
# – EVCHRGAPT, EVCHRGWKS, EVCHRGBUS, EVCHRGMUNI, EVCHRGHWY, EVHCRGOTH (respondent charged an EV at their apartment building, place of work, a business or shopping center, a municipal parking lot, a highway rest stop, a car dealership, or somewhere else)
# – EVHOMEAMT (what percentage of EV charging was done at home)
# – EVCHRGTYPE (what type of EV charger does respondent have at home)
# – EVWRKMILES (average number of miles EV is driven a week)
# • The following variables were top-coded:
#   – BEDROOMS (number of bedrooms) to 6
# – OTHROOMS (number of other rooms) to 9
# – NCOMBATH (number of full bathrooms) to 4
# – NHAFBATH (number of half bathrooms) to 2
# – HHAGE (age of the hher) to 90
# – NHSLDMEM (number of hh members) to 7
# – NUMCHILD (number of children under 18) to 4
# • We added random errors to weather and climate (HDD30YR and CDD30YR) values, as well as to the annualized consumption variables for electricity and natural gas.
# June 2023
# Adjustments were minor and do not result in significant differences for aggregate estimation.
# • We rounded the SQFTEST, TOTSQFT_EN, TOTHSQFT, and TOTCSQFT variables to the nearest 10.

# References
# Lohr, S.L. (2010). Sampling: Desing and Analysis. 2nd ed. Boston: Brooks/Cole. Page 380–383.
# Lumley, T. (2017) "Survey: analysis of complex survey samples". R package version 4.1-1.
