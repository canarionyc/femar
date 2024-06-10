
# setup -------------------------------------------------------------------
# rm(list = ls())
source("~/Spatial/.RProfile")
library(configr)
configr::read.config()
devtools::load_all("~/fstutils/", export_all = TRUE)
devtools::load_all("~/Spatial/FEMA/femar", reset=TRUE, export_all = TRUE)
print(getOption("tigris_year"))
# source("~/lattice_setup.R")
library(readr)

library(survey)
stop()
browseURL(.EIA_workdir)
list.files(.EIA_datadir, full.names = TRUE, recursive = TRUE)
#dir.create(.EIA_workdir)
?strOptions
strOptions()

# TYPEHUQ
# "1 Mobile home
# 2 Single-family house detached from any other house
# 3 Single-family house attached to one or more other houses (for example: duplex, row house, or townhome)
# 4 Apartment in a building with 2 to 4 units
# 5 Apartment in a building with 5 or more units"

# RECS2020_fst ------------------------------------------------------------

RECS2020_fst <- file.path(.EIA_workdir, "RECS2020.FST"); print(file.info(RECS2020_fst))
if(file.exists(RECS2020_fst)) {
  print(fst.metadata(RECS2020_fst))
  RECS2020 <- read_fst(RECS2020_fst, as.data.table = TRUE)
} else {
  RECS2020.csv <- file.path(.EIA_datadir, "RECS", "recs2020_public_v7.csv")

  RECS2020 <- readr::read_csv(RECS2020.csv
                                        ,name_repair = sanitize
#                          ,na = c("", "-2")
                                        , col_types = cols(
    DOEID = col_character(),
    REGIONC = col_character(),
    DIVISION = col_character(),
    STATE_FIPS = col_character(),
    STATE_POSTAL = col_character(),
    STATE_NAME = col_character(),
    BA_CLIMATE = col_factor(),
    IECC_CLIMATE_CODE = col_factor(),
    UATYP10 = col_factor(),
    HDD65 = col_double(),
    CDD65 = col_double(),
    HDD30YR_PUB = col_double(),
    CDD30YR_PUB = col_double(),
    TYPEHUQ = col_factor(levels=as.character(1:5)),
    CELLAR = col_double(),
    CRAWL = col_double(),
    CONCRETE = col_double(),
    BASEOTH = col_double(),
    BASEFIN = col_double(),
    ATTIC = col_double(),
    ATTICFIN = col_double(),
    STORIES = col_double(),
    PRKGPLC1 = col_double(),
    SIZEOFGARAGE = col_double(),
    KOWNRENT = col_double(),
    YEARMADERANGE = col_double(),
    BEDROOMS = col_double(),
    NCOMBATH = col_double(),
    NHAFBATH = col_double(),
    OTHROOMS = col_double(),
    TOTROOMS = col_double(),
    STUDIO = col_double(),
    WALLTYPE = col_double(),
    ROOFTYPE = col_double(),
    HIGHCEIL = col_double(),
    DOOR1SUM = col_double(),
    WINDOWS = col_double(),
    TYPEGLASS = col_double(),
    ORIGWIN = col_double(),
    WINFRAME = col_double(),
    TREESHAD = col_double(),
    ADQINSUL = col_double(),
    DRAFTY = col_double(),
    UGASHERE = col_double(),
    SWIMPOOL = col_double(),
    MONPOOL = col_double(),
    POOLPUMP = col_double(),
    FUELPOOL = col_double(),
    RECBATH = col_double(),
    MONTUB = col_double(),
    FUELTUB = col_double(),
    NUMFRIG = col_double(),
    SIZRFRI1 = col_double(),
    TYPERFR1 = col_double(),
    AGERFRI1 = col_double(),
    ICE = col_double(),
    SIZRFRI2 = col_double(),
    TYPERFR2 = col_double(),
    AGERFRI2 = col_double(),
    LOCRFRI2 = col_double(),
    WINECHILL = col_double(),
    NUMFREEZ = col_double(),
    UPRTFRZR = col_double(),
    SIZFREEZ = col_double(),
    FREEZER = col_double(),
    AGEFRZR = col_double(),
    RANGE = col_double(),
    COOKTOP = col_double(),
    OVEN = col_double(),
    RANGEFUEL = col_double(),
    RANGEINDT = col_double(),
    RCOOKUSE = col_double(),
    ROVENUSE = col_double(),
    COOKTOPFUEL = col_double(),
    COOKTOPINDT = col_double(),
    COOKTOPUSE = col_double(),
    OVENFUEL = col_double(),
    OVENUSE = col_double(),
    MICRO = col_double(),
    AMTMICRO = col_double(),
    OUTGRILLFUEL = col_double(),
    OUTGRILL = col_double(),
    NUMMEAL = col_double(),
    USECOFFEE = col_double(),
    TOAST = col_double(),
    TOASTOVN = col_double(),
    CROCKPOT = col_double(),
    PRSSCOOK = col_double(),
    RICECOOK = col_double(),
    BLENDER = col_double(),
    APPOTHER = col_double(),
    ELFOOD = col_double(),
    LPCOOK = col_double(),
    UGCOOK = col_double(),
    DISHWASH = col_double(),
    DWASHUSE = col_double(),
    DWCYCLE = col_double(),
    AGEDW = col_double(),
    CWASHER = col_double(),
    TOPFRONT = col_double(),
    WASHLOAD = col_double(),
    WASHTEMP = col_double(),
    AGECWASH = col_double(),
    DRYER = col_double(),
    DRYRFUEL = col_double(),
    DRYRUSE = col_double(),
    AGECDRYER = col_double(),
    TVCOLOR = col_double(),
    TVSIZE1 = col_double(),
    TVTYPE1 = col_double(),
    TVUSE1 = col_double(),
    TVONWD1 = col_double(),
    TVONWE1 = col_double(),
    TVSIZE2 = col_double(),
    TVTYPE2 = col_double(),
    TVUSE2 = col_double(),
    TVONWD2 = col_double(),
    TVONWE2 = col_double(),
    TVSIZE3 = col_double(),
    TVTYPE3 = col_double(),
    TVUSE3 = col_double(),
    TVONWD3 = col_double(),
    TVONWE3 = col_double(),
    CABLESAT = col_double(),
    COMBODVR = col_double(),
    SEPDVR = col_double(),
    INTSTREAM = col_double(),
    PLAYSTA = col_double(),
    DVD = col_double(),
    VCR = col_double(),
    TVAUDIOSYS = col_double(),
    DESKTOP = col_double(),
    NUMLAPTOP = col_double(),
    NUMTABLET = col_double(),
    ELPERIPH = col_double(),
    NUMSMPHONE = col_double(),
    CELLPHONE = col_double(),
    TELLWORK = col_double(),
    TELLDAYS = col_double(),
    TLDESKTOP = col_double(),
    TLLAPTOP = col_double(),
    TLTABLET = col_double(),
    TLMONITOR = col_double(),
    TLOTHER = col_double(),
    ONLNEDUC = col_double(),
    INTERNET = col_double(),
    INTYPECELL = col_double(),
    INTYPEBROAD = col_double(),
    INTYPEOTH = col_double(),
    SMARTSPK = col_double(),
    SSLIGHT = col_double(),
    SSTEMP = col_double(),
    SSSECURE = col_double(),
    SSTV = col_double(),
    SSOTHER = col_double(),
    HEATHOME = col_double(),
    DNTHEAT = col_double(),
    HEATAPT = col_double(),
    EQUIPM = col_double(),
    FUELHEAT = col_double(),
    EQUIPAGE = col_double(),
    GEOHP = col_double(),
    EQUIPAUXTYPE = col_double(),
    EQUIPAUX = col_double(),
    FUELAUX = col_double(),
    USEEQUIPAUX = col_double(),
    NUMPORTEL = col_double(),
    NUMFIREPLC = col_double(),
    NUMDLHP = col_double(),
    BASEHEAT = col_double(),
    ATTCHEAT = col_double(),
    GARGHEAT = col_double(),
    HUMIDTYPE = col_double(),
    NUMPORTHUM = col_double(),
    USEHUMID = col_double(),
    ELWARM = col_double(),
    UGWARM = col_double(),
    LPWARM = col_double(),
    FOWARM = col_double(),
    WDWARM = col_double(),
    AIRCOND = col_double(),
    COOLAPT = col_double(),
    ACEQUIPM_PUB = col_double(),
    ACEQUIPAGE = col_double(),
    ACEQUIPAUXTYPE_PUB = col_double(),
    NUMDLHPAC = col_double(),
    NUMWWAC = col_double(),
    NUMPORTAC = col_double(),
    BASECOOL = col_double(),
    ATTCCOOL = col_double(),
    GARGCOOL = col_double(),
    NUMCFAN = col_double(),
    NUMFLOORFAN = col_double(),
    USECFAN = col_double(),
    HOUSEFAN = col_double(),
    ATTICFAN = col_double(),
    DEHUMTYPE = col_double(),
    NUMPORTDEHUM = col_double(),
    USEDEHUM = col_double(),
    ELCOOL = col_double(),
    TYPETHERM = col_double(),
    HEATCNTL = col_double(),
    TEMPHOME = col_double(),
    TEMPGONE = col_double(),
    TEMPNITE = col_double(),
    COOLCNTL = col_double(),
    TEMPHOMEAC = col_double(),
    TEMPGONEAC = col_double(),
    TEMPNITEAC = col_double(),
    H2OAPT = col_double(),
    H2OMAIN = col_double(),
    WHEATSIZ = col_double(),
    WHEATBKT = col_double(),
    WHEATAGE = col_double(),
    FUELH2O = col_double(),
    MORETHAN1H2O = col_double(),
    FUELH2O2 = col_double(),
    ELWATER = col_double(),
    FOWATER = col_double(),
    LPWATER = col_double(),
    SOLWATER = col_double(),
    WDWATER = col_double(),
    UGWATER = col_double(),
    LGTIN1TO4 = col_double(),
    LGTIN4TO8 = col_double(),
    LGTINMORE8 = col_double(),
    LGTINLED = col_double(),
    LGTINCFL = col_double(),
    LGTINCAN = col_double(),
    LGTOUTANY = col_double(),
    LGTOUTNITE = col_double(),
    LGTOUTLED = col_double(),
    LGTOUTCFL = col_double(),
    LGTOUTCAN = col_double(),
    ELPAY = col_double(),
    NGPAY = col_double(),
    LPGPAY = col_double(),
    FOPAY = col_double(),
    SMARTMETER = col_double(),
    INTDATAACC = col_double(),
    MEDICALDEV = col_double(),
    POWEROUT = col_double(),
    WHYPOWEROUT = col_double(),
    BACKUP = col_double(),
    SOLAR = col_double(),
    ELOTHER = col_double(),
    UGOTH = col_double(),
    LPOTHER = col_double(),
    FOOTHER = col_double(),
    USEEL = col_double(),
    USENG = col_double(),
    USELP = col_double(),
    USEFO = col_double(),
    USESOLAR = col_double(),
    USEWOOD = col_double(),
    ALLELEC = col_double(),
    HHSEX = col_double(),
    HHAGE = col_double(),
    EMPLOYHH = col_double(),
    EDUCATION = col_double(),
    SDESCENT = col_double(),
    HOUSEHOLDER_RACE = col_double(),
    NHSLDMEM = col_double(),
    NUMCHILD = col_double(),
    NUMADULT1 = col_double(),
    NUMADULT2 = col_double(),
    ATHOME = col_double(),
    MONEYPY = col_double(),
    SCALEB = col_double(),
    SCALEG = col_double(),
    SCALEE = col_double(),
    PAYHELP = col_double(),
    NOHEATBROKE = col_double(),
    NOHEATEL = col_double(),
    NOHEATNG = col_double(),
    NOHEATBULK = col_double(),
    NOHEATDAYS = col_double(),
    NOHEATHELP = col_double(),
    COLDMA = col_double(),
    NOACBROKE = col_double(),
    NOACEL = col_double(),
    NOACDAYS = col_double(),
    NOACHELP = col_double(),
    HOTMA = col_double(),
    ENERGYASST = col_double(),
    ENERGYASST20 = col_double(),
    ENERGYASST19 = col_double(),
    ENERGYASST18 = col_double(),
    ENERGYASST17 = col_double(),
    ENERGYASST16 = col_double(),
    ENERGYASSTOTH = col_double(),
    SQFTEST = col_double(),
    SQFTRANGE = col_double(),
    SQFTINCB = col_double(),
    SQFTINCA = col_double(),
    SQFTINCG = col_double(),
    TOTSQFT_EN = col_double(),
    TOTHSQFT = col_double(),
    TOTCSQFT = col_double(),
    ZACEQUIPAGE = col_double(),
    ZADQINSUL = col_double(),
    ZAGECDRYER = col_double(),
    ZAGECWASH = col_double(),
    ZAGEDW = col_double(),
    ZAGEFRZR = col_double(),
    ZAGERFRI1 = col_double(),
    ZAGERFRI2 = col_double(),
    ZAIRCOND = col_double(),
    ZAMTMICRO = col_double(),
    ZATHOME = col_double(),
    ZATTCCOOL = col_double(),
    ZATTCHEAT = col_double(),
    ZATTIC = col_double(),
    ZATTICFAN = col_double(),
    ZATTICFIN = col_double(),
    ZBACKUP = col_double(),
    ZBASECOOL = col_double(),
    ZBASEFIN = col_double(),
    ZBASEHEAT = col_double(),
    ZBASEOTH = col_double(),
    ZBEDROOMS = col_double(),
    ZBLENDER = col_double(),
    ZCABLESAT = col_double(),
    ZCELLAR = col_double(),
    ZCELLPHONE = col_double(),
    ZCOLDMA = col_double(),
    ZCOMBODVR = col_double(),
    ZCONCRETE = col_double(),
    ZCOOKTOP = col_double(),
    ZCOOKTOPFUEL = col_double(),
    ZCOOKTOPINDT = col_double(),
    ZCOOKTOPUSE = col_double(),
    ZCOOLAPT = col_double(),
    ZCOOLCNTL = col_double(),
    ZCRAWL = col_double(),
    ZCROCKPOT = col_double(),
    ZCWASHER = col_double(),
    ZDEHUMTYPE = col_double(),
    ZDESKTOP = col_double(),
    ZDISHWASH = col_double(),
    ZDOOR1SUM = col_double(),
    ZDRAFTY = col_double(),
    ZDRYER = col_double(),
    ZDRYRFUEL = col_double(),
    ZDRYRUSE = col_double(),
    ZDVD = col_double(),
    ZDWASHUSE = col_double(),
    ZDWCYCLE = col_double(),
    ZEDUCATION = col_double(),
    ZELPAY = col_double(),
    ZELPERIPH = col_double(),
    ZEMPLOYHH = col_double(),
    ZENERGYASST = col_double(),
    ZENERGYASST16 = col_double(),
    ZENERGYASST17 = col_double(),
    ZENERGYASST18 = col_double(),
    ZENERGYASST19 = col_double(),
    ZENERGYASST20 = col_double(),
    ZENERGYASSTOTH = col_double(),
    ZEQUIPAGE = col_double(),
    ZEQUIPAUXTYPE = col_double(),
    ZEQUIPM = col_double(),
    ZFOPAY = col_double(),
    ZFREEZER = col_double(),
    ZFUELAUX = col_double(),
    ZFUELH2O = col_double(),
    ZFUELH2O2 = col_double(),
    ZFUELHEAT = col_double(),
    ZFUELPOOL = col_double(),
    ZFUELTUB = col_double(),
    ZGARGCOOL = col_double(),
    ZGARGHEAT = col_double(),
    ZH2OAPT = col_double(),
    ZH2OMAIN = col_double(),
    ZHEATAPT = col_double(),
    ZHEATCNTL = col_double(),
    ZHEATHOME = col_double(),
    ZHHAGE = col_double(),
    ZHHSEX = col_double(),
    ZHIGHCEIL = col_double(),
    ZHOTMA = col_double(),
    ZHOUSEFAN = col_double(),
    ZHUMIDTYPE = col_double(),
    ZICE = col_double(),
    ZINTERNET = col_double(),
    ZINTSTREAM = col_double(),
    ZINTYPEBROAD = col_double(),
    ZINTYPECELL = col_double(),
    ZINTYPEOTH = col_double(),
    ZKOWNRENT = col_double(),
    ZLGTIN1TO4 = col_double(),
    ZLGTIN4TO8 = col_double(),
    ZLGTINCAN = col_double(),
    ZLGTINCFL = col_double(),
    ZLGTINLED = col_double(),
    ZLGTINMORE8 = col_double(),
    ZLGTOUTANY = col_double(),
    ZLGTOUTCAN = col_double(),
    ZLGTOUTCFL = col_double(),
    ZLGTOUTLED = col_double(),
    ZLGTOUTNITE = col_double(),
    ZLOCRFRI2 = col_double(),
    ZLPGPAY = col_double(),
    ZMICRO = col_double(),
    ZMONEYPY = col_double(),
    ZMONPOOL = col_double(),
    ZMONTUB = col_double(),
    ZMORETHAN1H2O = col_double(),
    ZNCOMBATH = col_double(),
    ZNGPAY = col_double(),
    ZNHAFBATH = col_double(),
    ZNHSLDMEM = col_double(),
    ZNOACBROKE = col_double(),
    ZNOACDAYS = col_double(),
    ZNOACEL = col_double(),
    ZNOACHELP = col_double(),
    ZNOHEATBROKE = col_double(),
    ZNOHEATBULK = col_double(),
    ZNOHEATDAYS = col_double(),
    ZNOHEATEL = col_double(),
    ZNOHEATHELP = col_double(),
    ZNOHEATNG = col_double(),
    ZNUMADULT1 = col_double(),
    ZNUMADULT2 = col_double(),
    ZNUMCFAN = col_double(),
    ZNUMCHILD = col_double(),
    ZNUMDLHP = col_double(),
    ZNUMDLHPAC = col_double(),
    ZNUMFIREPLC = col_double(),
    ZNUMFLOORFAN = col_double(),
    ZNUMFREEZ = col_double(),
    ZNUMFRIG = col_double(),
    ZNUMLAPTOP = col_double(),
    ZNUMMEAL = col_double(),
    ZNUMPORTAC = col_double(),
    ZNUMPORTDEHUM = col_double(),
    ZNUMPORTEL = col_double(),
    ZNUMPORTHUM = col_double(),
    ZNUMSMPHONE = col_double(),
    ZNUMTABLET = col_double(),
    ZNUMWWAC = col_double(),
    ZONLNEDUC = col_double(),
    ZORIGWIN = col_double(),
    ZOTHROOMS = col_double(),
    ZOUTGRILLFUEL = col_double(),
    ZOUTLET = col_double(),
    ZOVEN = col_double(),
    ZOVENFUEL = col_double(),
    ZOVENUSE = col_double(),
    ZPAYHELP = col_double(),
    ZPLAYSTA = col_double(),
    ZPOOLPUMP = col_double(),
    ZPOWEROUT = col_double(),
    ZPRKGPLC1 = col_double(),
    ZPRSSCOOK = col_double(),
    ZRANGE = col_double(),
    ZRANGEFUEL = col_double(),
    ZRANGEINDT = col_double(),
    ZRCOOKUSE = col_double(),
    ZRECBATH = col_double(),
    ZRICECOOK = col_double(),
    ZROOFTYPE = col_double(),
    ZROVENUSE = col_double(),
    ZSCALEB = col_double(),
    ZSCALEE = col_double(),
    ZSCALEG = col_double(),
    ZSDESCENT = col_double(),
    ZSEPDVR = col_double(),
    ZSIZEOFGARAGE = col_double(),
    ZSIZFREEZ = col_double(),
    ZSIZRFRI1 = col_double(),
    ZSIZRFRI2 = col_double(),
    ZSMARTSPK = col_double(),
    ZSQFTEST = col_double(),
    ZSQFTINCA = col_double(),
    ZSQFTINCB = col_double(),
    ZSQFTINCG = col_double(),
    ZSQFTRANGE = col_double(),
    ZSSLIGHT = col_double(),
    ZSSOTHER = col_double(),
    ZSSSECURE = col_double(),
    ZSSTEMP = col_double(),
    ZSSTV = col_double(),
    ZSTORIES = col_double(),
    ZSWIMPOOL = col_double(),
    ZTELLDAYS = col_double(),
    ZTELLWORK = col_double(),
    ZTEMPGONE = col_double(),
    ZTEMPGONEAC = col_double(),
    ZTEMPHOME = col_double(),
    ZTEMPHOMEAC = col_double(),
    ZTEMPNITE = col_double(),
    ZTEMPNITEAC = col_double(),
    ZTLDESKTOP = col_double(),
    ZTLLAPTOP = col_double(),
    ZTLMONITOR = col_double(),
    ZTLOTHER = col_double(),
    ZTLTABLET = col_double(),
    ZTOAST = col_double(),
    ZTOASTOVN = col_double(),
    ZTOPFRONT = col_double(),
    ZTREESHAD = col_double(),
    ZTVAUDIOSYS = col_double(),
    ZTVCOLOR = col_double(),
    ZTVONWD1 = col_double(),
    ZTVONWD2 = col_double(),
    ZTVONWD3 = col_double(),
    ZTVONWE1 = col_double(),
    ZTVONWE2 = col_double(),
    ZTVONWE3 = col_double(),
    ZTVSIZE1 = col_double(),
    ZTVSIZE2 = col_double(),
    ZTVSIZE3 = col_double(),
    ZTVTYPE1 = col_double(),
    ZTVTYPE2 = col_double(),
    ZTVTYPE3 = col_double(),
    ZTVUSE1 = col_double(),
    ZTVUSE2 = col_double(),
    ZTVUSE3 = col_double(),
    ZTYPEGLASS = col_double(),
    ZTYPERFR1 = col_double(),
    ZTYPERFR2 = col_double(),
    ZTYPETHERM = col_double(),
    ZUGASHERE = col_double(),
    ZUPRTFRZR = col_double(),
    ZUSECFAN = col_double(),
    ZUSECOFFEE = col_double(),
    ZUSEDEHUM = col_double(),
    ZUSEEQUIPAUX = col_double(),
    ZUSEHUMID = col_double(),
    ZVCR = col_double(),
    ZWALLTYPE = col_double(),
    ZWASHLOAD = col_double(),
    ZWASHTEMP = col_double(),
    ZWHEATAGE = col_double(),
    ZWHEATBKT = col_double(),
    ZWHEATSIZ = col_double(),
    ZWHYPOWEROUT = col_double(),
    ZWINDOWS = col_double(),
    ZWINECHILL = col_double(),
    ZWINFRAME = col_double(),
    ZYEARMADERANGE = col_double(),
    ZTOTROOMS = col_double(),
    ZDNTHEAT = col_double(),
    ZTYPEHUQ = col_double(),
    ZSTUDIO = col_double(),
    ZOUTGRILL = col_double(),
    ZHOUSEHOLDER_RACE = col_double(),
    ZACEQUIPM_PUB = col_double(),
    ZACEQUIPAUXTYPE_PUB = col_double(),
    NWEIGHT = col_double(),
    NWEIGHT1 = col_double(),
    NWEIGHT2 = col_double(),
    NWEIGHT3 = col_double(),
    NWEIGHT4 = col_double(),
    NWEIGHT5 = col_double(),
    NWEIGHT6 = col_double(),
    NWEIGHT7 = col_double(),
    NWEIGHT8 = col_double(),
    NWEIGHT9 = col_double(),
    NWEIGHT10 = col_double(),
    NWEIGHT11 = col_double(),
    NWEIGHT12 = col_double(),
    NWEIGHT13 = col_double(),
    NWEIGHT14 = col_double(),
    NWEIGHT15 = col_double(),
    NWEIGHT16 = col_double(),
    NWEIGHT17 = col_double(),
    NWEIGHT18 = col_double(),
    NWEIGHT19 = col_double(),
    NWEIGHT20 = col_double(),
    NWEIGHT21 = col_double(),
    NWEIGHT22 = col_double(),
    NWEIGHT23 = col_double(),
    NWEIGHT24 = col_double(),
    NWEIGHT25 = col_double(),
    NWEIGHT26 = col_double(),
    NWEIGHT27 = col_double(),
    NWEIGHT28 = col_double(),
    NWEIGHT29 = col_double(),
    NWEIGHT30 = col_double(),
    NWEIGHT31 = col_double(),
    NWEIGHT32 = col_double(),
    NWEIGHT33 = col_double(),
    NWEIGHT34 = col_double(),
    NWEIGHT35 = col_double(),
    NWEIGHT36 = col_double(),
    NWEIGHT37 = col_double(),
    NWEIGHT38 = col_double(),
    NWEIGHT39 = col_double(),
    NWEIGHT40 = col_double(),
    NWEIGHT41 = col_double(),
    NWEIGHT42 = col_double(),
    NWEIGHT43 = col_double(),
    NWEIGHT44 = col_double(),
    NWEIGHT45 = col_double(),
    NWEIGHT46 = col_double(),
    NWEIGHT47 = col_double(),
    NWEIGHT48 = col_double(),
    NWEIGHT49 = col_double(),
    NWEIGHT50 = col_double(),
    NWEIGHT51 = col_double(),
    NWEIGHT52 = col_double(),
    NWEIGHT53 = col_double(),
    NWEIGHT54 = col_double(),
    NWEIGHT55 = col_double(),
    NWEIGHT56 = col_double(),
    NWEIGHT57 = col_double(),
    NWEIGHT58 = col_double(),
    NWEIGHT59 = col_double(),
    NWEIGHT60 = col_double(),
    KWH = col_double(),
    BTUEL = col_double(),
    DOLLAREL = col_double(),
    ELXBTU = col_double(),
    PERIODEL = col_double(),
    ZELAMOUNT = col_double(),
    KWHSPH = col_double(),
    KWHCOL = col_double(),
    KWHWTH = col_double(),
    KWHRFG = col_double(),
    KWHRFG1 = col_double(),
    KWHRFG2 = col_double(),
    KWHFRZ = col_double(),
    KWHCOK = col_double(),
    KWHMICRO = col_double(),
    KWHCW = col_double(),
    KWHCDR = col_double(),
    KWHDWH = col_double(),
    KWHLGT = col_double(),
    KWHTVREL = col_double(),
    KWHTV1 = col_double(),
    KWHTV2 = col_double(),
    KWHTV3 = col_double(),
    KWHAHUHEAT = col_double(),
    KWHAHUCOL = col_double(),
    KWHCFAN = col_double(),
    KWHDHUM = col_double(),
    KWHHUM = col_double(),
    KWHPLPMP = col_double(),
    KWHHTBPMP = col_double(),
    KWHHTBHEAT = col_double(),
    KWHEVCHRG = col_double(),
    KWHNEC = col_double(),
    KWHOTH = col_double(),
    BTUELSPH = col_double(),
    BTUELCOL = col_double(),
    BTUELWTH = col_double(),
    BTUELRFG = col_double(),
    BTUELRFG1 = col_double(),
    BTUELRFG2 = col_double(),
    BTUELFRZ = col_double(),
    BTUELCOK = col_double(),
    BTUELMICRO = col_double(),
    BTUELCW = col_double(),
    BTUELCDR = col_double(),
    BTUELDWH = col_double(),
    BTUELLGT = col_double(),
    BTUELTVREL = col_double(),
    BTUELTV1 = col_double(),
    BTUELTV2 = col_double(),
    BTUELTV3 = col_double(),
    BTUELAHUHEAT = col_double(),
    BTUELAHUCOL = col_double(),
    BTUELCFAN = col_double(),
    BTUELDHUM = col_double(),
    BTUELHUM = col_double(),
    BTUELPLPMP = col_double(),
    BTUELHTBPMP = col_double(),
    BTUELHTBHEAT = col_double(),
    BTUELEVCHRG = col_double(),
    BTUELNEC = col_double(),
    BTUELOTH = col_double(),
    DOLELSPH = col_double(),
    DOLELCOL = col_double(),
    DOLELWTH = col_double(),
    DOLELRFG = col_double(),
    DOLELRFG1 = col_double(),
    DOLELRFG2 = col_double(),
    DOLELFRZ = col_double(),
    DOLELCOK = col_double(),
    DOLELMICRO = col_double(),
    DOLELCW = col_double(),
    DOLELCDR = col_double(),
    DOLELDWH = col_double(),
    DOLELLGT = col_double(),
    DOLELTVREL = col_double(),
    DOLELTV1 = col_double(),
    DOLELTV2 = col_double(),
    DOLELTV3 = col_double(),
    DOLELAHUHEAT = col_double(),
    DOLELAHUCOL = col_double(),
    DOLELCFAN = col_double(),
    DOLELDHUM = col_double(),
    DOLELHUM = col_double(),
    DOLELPLPMP = col_double(),
    DOLELHTBPMP = col_double(),
    DOLELHTBHEAT = col_double(),
    DOLELEVCHRG = col_double(),
    DOLELNEC = col_double(),
    DOLELOTH = col_double(),
    CUFEETNG = col_double(),
    BTUNG = col_double(),
    DOLLARNG = col_double(),
    NGXBTU = col_double(),
    PERIODNG = col_double(),
    ZNGAMOUNT = col_double(),
    BTUNGSPH = col_double(),
    BTUNGWTH = col_double(),
    BTUNGCOK = col_double(),
    BTUNGCDR = col_double(),
    BTUNGPLHEAT = col_double(),
    BTUNGHTBHEAT = col_double(),
    BTUNGNEC = col_double(),
    BTUNGOTH = col_double(),
    CUFEETNGSPH = col_double(),
    CUFEETNGWTH = col_double(),
    CUFEETNGCOK = col_double(),
    CUFEETNGCDR = col_double(),
    CUFEETNGPLHEAT = col_double(),
    CUFEETNGHTBHEAT = col_double(),
    CUFEETNGNEC = col_double(),
    CUFEETNGOTH = col_double(),
    DOLNGSPH = col_double(),
    DOLNGWTH = col_double(),
    DOLNGCOK = col_double(),
    DOLNGCDR = col_double(),
    DOLNGPLHEAT = col_double(),
    DOLNGHTBHEAT = col_double(),
    DOLNGNEC = col_double(),
    DOLNGOTH = col_double(),
    GALLONLP = col_double(),
    BTULP = col_double(),
    DOLLARLP = col_double(),
    LPXBTU = col_double(),
    PERIODLP = col_double(),
    ZLPAMOUNT = col_double(),
    BTULPSPH = col_double(),
    BTULPWTH = col_double(),
    BTULPCOK = col_double(),
    BTULPCDR = col_double(),
    BTULPNEC = col_double(),
    BTULPOTH = col_double(),
    GALLONLPSPH = col_double(),
    GALLONLPWTH = col_double(),
    GALLONLPCOK = col_double(),
    GALLONLPCDR = col_double(),
    GALLONLPNEC = col_double(),
    GALLONLPOTH = col_double(),
    DOLLPSPH = col_double(),
    DOLLPWTH = col_double(),
    DOLLPCOK = col_double(),
    DOLLPCDR = col_double(),
    DOLLPNEC = col_double(),
    DOLLPOTH = col_double(),
    GALLONFO = col_double(),
    BTUFO = col_double(),
    DOLLARFO = col_double(),
    FOXBTU = col_double(),
    PERIODFO = col_double(),
    ZFOAMOUNT = col_double(),
    BTUFOSPH = col_double(),
    BTUFOWTH = col_double(),
    BTUFONEC = col_double(),
    BTUFOOTH = col_double(),
    GALLONFOSPH = col_double(),
    GALLONFOWTH = col_double(),
    GALLONFONEC = col_double(),
    GALLONFOOTH = col_double(),
    DOLFOSPH = col_double(),
    DOLFOWTH = col_double(),
    DOLFONEC = col_double(),
    DOLFOOTH = col_double(),
    BTUWD = col_double(),
    ZWDAMOUNT = col_double(),
    TOTALBTUSPH = col_double(),
    TOTALDOLSPH = col_double(),
    TOTALBTUWTH = col_double(),
    TOTALDOLWTH = col_double(),
    TOTALBTUOTH = col_double(),
    TOTALDOLOTH = col_double(),
    TOTALBTU = col_double(),
    TOTALDOL = col_double(),
    DBT1 = col_double(),
    DBT99 = col_double(),
    GWT = col_double(),
    WOODTYPE = col_double(),
    OUTLET = col_double(),
    ELECVEH = col_double(),
    EVCHRGHOME = col_double(),
    EVCHRGAPT = col_double(),
    EVCHRGWKS = col_double(),
    EVCHRGBUS = col_double(),
    EVCHRGMUNI = col_double(),
    EVCHRGDLR = col_double(),
    EVCHRGHWY = col_double(),
    EVCHRGOTH = col_double(),
    EVHOMEAMT = col_double(),
    EVCHRGTYPE = col_double()
  )) %>% setDT(key = c('DOEID'))
  write_fst(RECS2020, path = RECS2020_fst);   print(file.info(RECS2020_fst))
}; str(RECS2020,give.attr = FALSE)
RECS2020

attach(RECS2020)
RECS2020[, table(TYPEHUQ, useNA = "ifany")]
detach(RECS2020)

recs_mf <- RECS2020[TYPEHUQ=="5"]
recs_mf%>% as_tibble()

recs_mf[, .(.N), keyby = c('STATE_FIPS','KOWNRENT')]
(recs_mf_zz <- recs_mf[, .(.N), keyby = c('KOWNRENT')])

RECS2020[, .(.N, BTUEL=sum(BTUEL), BTUNG=sum(BTUNG))]

# R Example 1 ----

# Calculate the frequency and RSE of households that used natural gas as their
#main space-heating fuel (Table HC6.1)

## Step 1. ----

# Create a new variable to flag the records of households that used
# natural gas as their main space-heating fuel. This new variable
# NG_MAINSPACEHEAT is equal to 1 if the household used natural gas as its main
# space-heating fuel and 0 otherwise.
RECS2020$NG_MAINSPACEHEAT <- ifelse(RECS2020$FUELHEAT == 1, 1, 0)

## Step 2. ----

# Define the Jackknife replicate weights you will use for estimation:
(repweights<-select(RECS2020,NWEIGHT1:NWEIGHT60))

## Step 3. ----

# Define the survey design with the Jackknife replicate weights to calculate
# appropriate standard errors uising `svrepdesign`:
?svrepdesign
  (RECS <- svrepdesign(data = RECS2020,
                      weight = ~NWEIGHT,
                      repweights = repweights,
                      type = "JK1",
                      combined.weights = TRUE,
                      scale = (ncol(repweights)-1)/ncol(repweights),
                      mse = TRUE))
summary(RECS)
class(RECS)
methods(class="svyrep.design")

## Step 4. ----

# Use `svytotal` to sum the number of households by `NG_MAINSPACEHEAT`, using the survey design defined above.
(NG_MAINSPACEHEAT_Total<-as.data.frame(svytotal(~NG_MAINSPACEHEAT,RECS)))

# Answer. The estimated total households that used natural gas as their main
# space-heating fuel is 62,713,449 households. The calculation for the RSE is
# (483,047 / 62,713,449)*100 = 0.77. The sampling error is less than 1% of the
# estimate, which is relatively small. Alternatively, the RSE can be derived
# from:
  (NG_MAINSPACEHEAT_Total$RSE<-(NG_MAINSPACEHEAT_Total$SE/NG_MAINSPACEHEAT_Total$total)*100)

# To obtain the proportion estimate, use the `svymean()` function instead of the `svytotal` in the expression in Step 4. In addition, the confint() function provides the 95% confidence limits.

# R Example 2: ----

# Calculate the sum and average of the total natural gas used for the households
# in South Carolina (SC) (Table CE4.1.NG.ST Annual household site natural gas
# consumption in the United States by end use – totals and percentages, 2020).
# To calculate the total consumption estimates in R, use the svytotal()
# function; and use the svymean() function for the average consumption . In
# addition, use svyby() to group households by USENG and state (STATE_POSTAL)
# and the subset() function to limit the results to SC only. First, create a new
# variable to flag the households that have positive natural gas consumption for
# any natural gas end use. This new variable NGUSE is equal to 1 if BTUNG is
# greater than 0 and 0 otherwise. Then, run the survey design for the dataset
# again before producing estimates using the functions mentioned above.


RECS2020$NGUSE <- ifelse(RECS2020$BTUNG > 0, 1,0)
RECS <- svrepdesign(data = RECS2020,
                    weight = ~NWEIGHT,
                    repweights = repweights,
                    type = "JK1",
                    combined.weights = TRUE,
                    scale = (ncol(repweights)-1)/ncol(repweights),
                    mse = TRUE)

# calculate the total:
(BTUNG_TOTAL<-svyby(~BTUNG, by=~STATE_POSTAL+NGUSE, RECS, svytotal))
(BTUNG_SCTOTAL<-subset(BTUNG_TOTAL, STATE_POSTAL=='SC'))

# calculate the mean:
(BTUNG_MEAN<-svyby(~BTUNG, by=~STATE_POSTAL+NGUSE, RECS, svymean))
(BTUNG_SCMEAN<-subset(BTUNG_MEAN, STATE_POSTAL=='SC'))
# The output below shows the result for SC. The total estimated consumption for
# households that used natural gas in SC is 26.2 trillion British thermal units
# (Btu). The RSE for the total is ( 2509266634/26220994238)*100 = 9.6%. The
# average consumption per household is 34.4 million BTU, with RSE=6.3. As
# mentioned in R example 1, the 95% confidence limits with the `conflint()`
# function. Note that the estimates for NGUSE = 0 reflect consumption for homes
# that do not use any natural gas.


# R Example 3: ----

# Calculate the energy intensity per square foot by climate zone for the United States (Table CE1.1)
## Step 1. ----

#Create a new variable called Climate_region to combine climate zones, and rerun
#the survey design RECS.
RECS2020$Climate_Region <- as.factor(ifelse(RECS2020$BA_CLIMATE=='Subarctic', 'Very cold/Cold',
                                            ifelse(RECS2020$BA_CLIMATE=='Very-Cold', 'Very cold/Cold',
                                                   ifelse(RECS2020$BA_CLIMATE=='Cold', 'Very cold/Cold',
                                                          ifelse(RECS2020$BA_CLIMATE=='Mixed-Humid', 'Mixed-humid',
                                                                 ifelse(RECS2020$BA_CLIMATE=='Mixed-Dry', 'Mixed-dry/Hot-dry',
                                                                        ifelse(RECS2020$BA_CLIMATE=='Hot-Dry', 'Mixed-dry/Hot-dry',
                                                                               ifelse(RECS2020$BA_CLIMATE=='Hot-Humid', 'Hot-humid',
                                                                                      ifelse(RECS2020$BA_CLIMATE=='Marine', 'Marine', NA)))))))))
(RECS <- svrepdesign(data = RECS2020,
                    weight = ~NWEIGHT,
                    repweights = repweights,
                    type = "JK1",
                    combined.weights = TRUE,
                    scale = (ncol(repweights)-1)/ncol(repweights),
                    mse = TRUE))

## Step 2. ----

# To calculate the energy intensity per square foot for all U.S. homes, use the `svyratio()` function.
BTUPERSQFT<-svyratio(~TOTALBTU, ~TOTSQFT_EN, RECS)

The national estimate for energy intensity per square foot is about 42,000 Btu, as shown in Table CE1.1. The RSE is (0.1801853/42.20056)*100 = 0.43.

#To calculate the regional energy intensity per square foot, use `svyratio` with
#`svyby()`.
(BTUPERSQFTBYREGION<-svyby(~TOTALBTU, by=~Climate_Region,denominator=~TOTSQFT_EN, RECS, svyratio))

# As an example, the average total consumption per square foot in the hot-humid
# climate is about 35,000 Btu, as shown in the table below.

# R Example 4: ----

#Compare if the proportions and the consumption means for households using
#natural gas as their main space-heating fuel are statistically different among
#the households in different Census regions. To compare if the proportions of
#households using natural gas as their main space-heating fuel are different
#among the Census regions, use the `svychisq()` function to obtain chi-square
#statistics.

NGSPH_CHISQ<-svychisq(~NG_MAINSPACEHEAT+REGIONC,design=RECS,statistic="Chisq")

# To compare if the average space-heating consumption estimates for households
# using natural gas as their main space-heating fuel are different among the
# Census regions, use the svyglm() function to run a regression model and obtain
# the coefficient, and use the regTermTest() function to obtain the F-statistic.
(RECS_NGSPH<-subset(RECS,NG_MAINSPACEHEAT==1))
(NGSPH_REGIONGLM<-svyglm(TOTALBTUSPH~factor(REGIONC), design=RECS_NGSPH))
(regTermTest(NGSPH_REGIONGLM, ~factor(REGIONC), method="Wald"))

# Notes to Consider When Using the Microdata File and Replicate Weights
# 1. Publication standards: We do not publish RECS estimates where the RSE is higher than 50 or the number of households used for the calculation is less than 10 (indicated by a Q in the data tables). We recommend following these guidelines for custom analysis using the public use microdata file.
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
# – HHAGE (age of the householder) to 90
# – NHSLDMEM (number of household members) to 7
# – NUMCHILD (number of children under 18) to 4
# • We added random errors to weather and climate (HDD30YR and CDD30YR) values, as well as to the annualized consumption variables for electricity and natural gas.
# June 2023
# Adjustments were minor and do not result in significant differences for aggregate estimation.
# • We rounded the SQFTEST, TOTSQFT_EN, TOTHSQFT, and TOTCSQFT variables to the nearest 10.

# References
# Lohr, S.L. (2010). Sampling: Desing and Analysis. 2nd ed. Boston: Brooks/Cole. Page 380–383.
# Lumley, T. (2017) "Survey: analysis of complex survey samples". R package version 4.1-1.
